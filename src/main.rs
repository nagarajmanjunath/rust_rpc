use std::net::SocketAddr;
use jsonrpc_core;
use jsonrpc_core::{Compatibility,MetaIoHandler,BoxFuture, Result};
use jsonrpc_http_server as http;
use std::fmt;
use serde_derive::{Serialize, Deserialize};
use ethereum_types::{H160, U256, Address};
use jsonrpc_derive::rpc;
use ansi_term::Colour;
use jsonrpc_core::futures::{future, Future};
use std::collections::HashSet;
use std::io;
pub type HttpServer = http::Server;
pub use http::{
	hyper,
	RequestMiddleware, RequestMiddlewareAction,
	AccessControlAllowOrigin, Host, DomainsValidation, cors::AccessControlAllowHeaders
};


pub fn new_http(
	id: &str,
	options: &str,
	conf: HttpConfiguration,
) -> Result<Option<HttpServer>,String> {
	if !conf.enabled {
		return Ok(None);
	}

	let domain = DAPPS_DOMAIN;
	let url = format!("{}:{}", conf.interface, conf.port);
	let addr = url.parse().map_err(|_| format!("Invalid {} listen host/port given: {}", id, url))?;
	let handler = setup_rpc_server(conf.apis);

	let cors_domains = into_domains(conf.cors);
	let allowed_hosts = into_domains(with_domain(conf.hosts, domain, &Some(url.clone().into())));

	let start_result = start_http(
		&addr,
		cors_domains,
		allowed_hosts,
        handler,
		RpcExtractor,
		conf.server_threads,
		conf.max_payload,
		conf.keep_alive,
	);

	match start_result {
		Ok(server) => Ok(Some(server)),
		Err(ref err) if err.kind() == io::ErrorKind::AddrInUse => Err(
			format!("{} address {} is already in use, make sure that another instance of an Ethereum client is not running or change the address using the --{}-port and --{}-interface options.", id, url, options, options)
		),
		Err(e) => Err(format!("{} error: {:?}", id, e)),
	}
}


/// HTTP RPC server impl-independent metadata extractor
pub trait HttpMetaExtractor: Send + Sync + 'static {
	/// Type of Metadata
	type Metadata: jsonrpc_core::Metadata;
	/// Extracts metadata from given params.
	fn read_metadata(&self, origin: Option<String>, user_agent: Option<String>) -> Self::Metadata;
}

pub struct MetaExtractor<T> {
	extractor: T,
}

impl<T> MetaExtractor<T> {
	pub fn new(extractor: T) -> Self {
		MetaExtractor { extractor }
	}
}

impl<M, T> http::MetaExtractor<M> for MetaExtractor<T> where
	T: HttpMetaExtractor<Metadata = M>,
	M: jsonrpc_core::Metadata,
{
	fn read_metadata(&self, req: &hyper::Request<hyper::Body>) -> M {
		let as_string = |header: Option<&hyper::header::HeaderValue>| {
			header.and_then(|val| val.to_str().ok().map(ToOwned::to_owned))
		};

		let origin = as_string(req.headers().get("origin"));
		let user_agent = as_string(req.headers().get("user-agent"));
		self.extractor.read_metadata(origin, user_agent)
	}
}


#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
#[serde(rename_all = "kebab-case")]
pub enum Origin {
	/// RPC server (includes request origin)
	Rpc(String),
	/// Unknown
	Unknown,
}

impl Default for Origin {
	fn default() -> Self {
		Origin::Unknown
	}
}

impl fmt::Display for Origin {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Origin::Rpc(ref origin) => write!(f, "{} via RPC", origin),
			Origin::Unknown => write!(f, "unknown origin"),
		}
	}
}

/// RPC methods metadata.
#[derive(Clone, Default, Debug)]
pub struct Metadata {
	/// Request origin
	pub origin: Origin,

}


impl jsonrpc_core::Metadata for Metadata {}

/// Common HTTP & IPC metadata extractor.
pub struct RpcExtractor;

impl HttpMetaExtractor for RpcExtractor {
	type Metadata = Metadata;

	fn read_metadata(&self, origin: Option<String>, user_agent: Option<String>) -> Metadata {
		Metadata {
			origin: Origin::Rpc(
				format!("{} / {}",
						origin.unwrap_or_else(|| "unknown origin".to_string()),
						user_agent.unwrap_or_else(|| "unknown agent".to_string()))
			),
		}
	}
}

/// Start http server asynchronously and returns result with `Server` handle on success or an error.
pub fn start_http<M, S, H, T>(
	addr: &SocketAddr,
	cors_domains: http::DomainsValidation<http::AccessControlAllowOrigin>,
	allowed_hosts: http::DomainsValidation<http::Host>,
	handler: H,
	extractor: T,
	threads: usize,
	max_payload: usize,
	keep_alive: bool,
) -> ::std::io::Result<HttpServer> where
	M: jsonrpc_core::Metadata,
	S: jsonrpc_core::Middleware<M>,
	H: Into<jsonrpc_core::MetaIoHandler<M, S>>,
	T: HttpMetaExtractor<Metadata=M>,
{
	let extractor = MetaExtractor::new(extractor);
	Ok(http::ServerBuilder::with_meta_extractor(handler, extractor)
		.keep_alive(keep_alive)
		.threads(threads)
		.cors(cors_domains)
		.allowed_hosts(allowed_hosts)
		.health_api(("/api/health", "parity_nodeStatus"))
		.cors_allow_headers(AccessControlAllowHeaders::Any)
		.max_request_body_size(max_payload * 1024 * 1024)
		.start_http(addr)?)
}




//Transcation_Request
/// Transaction request coming from RPC
#[derive(Debug, Clone, Default, Eq, PartialEq, Hash)]
pub struct TransactionRequest {
	/// Sender
	pub from: Option<H160>,
	/// Recipient
	pub to: Option<H160>,
	/// Gas Price
	pub gas_price: Option<U256>,
	/// Gas
	pub gas: Option<U256>,
	/// Value of transaction in wei
	pub value: Option<U256>,

	pub data: Option<Bytes>,
	/// Transaction's nonce
	pub nonce: Option<U256>,

}

pub fn format_ether(i: U256) -> String {
	let mut string = format!("{}", i);
	let idx = string.len() as isize - 18;
	if idx <= 0 {
		let mut prefix = String::from("0.");
		for _ in 0..idx.abs() {
			prefix.push('0');
		}
		string = prefix + &string;
	} else {
		string.insert(idx as usize, '.');
	}
	String::from(string.trim_end_matches('0').trim_end_matches('.'))
}

impl fmt::Display for TransactionRequest {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let eth = self.value.unwrap_or_default();
		match self.to {
			Some(ref to) => write!(
				f,
				"{} ETH from {} to 0x{:?}",
				Colour::White.bold().paint(format_ether(eth)),
				Colour::White.bold().paint(
					self.from.as_ref()
						.map(|f| format!("0x{:?}", f))
						.unwrap_or_else(|| "?".to_string())),
				to
			),
			None => write!(
				f,
				"{} ETH from {} for contract creation",
				Colour::White.bold().paint(format_ether(eth)),
				Colour::White.bold().paint(
					self.from.as_ref()
						.map(|f| format!("0x{:?}", f))
						.unwrap_or_else(|| "?".to_string())),
			),
		}
	}
}

impl From<TransactionRequest> for TransactionRequest {
	fn from(r:TransactionRequest) -> Self {
		TransactionRequest {
			from: r.from.map(Into::into),
			to: r.to.map(Into::into),
			gas_price: r.gas_price.map(Into::into),
			gas: r.gas.map(Into::into),
			value: r.value.map(Into::into),
			data: r.data.map(Into::into),
			nonce: r.nonce.map(Into::into),
		}
	}
}

impl From<FilledTransactionRequest> for TransactionRequest {
	fn from(r: FilledTransactionRequest) -> Self {
		TransactionRequest {
			from: Some(r.from),
			to: r.to,
			gas_price: Some(r.gas_price),
			gas: Some(r.gas),
			value: Some(r.value),
			data: Some(r.data.into()),
			nonce: r.nonce,

		}
	}
}

impl Into<TransactionRequest> for TransactionRequest {
	fn into(self) -> TransactionRequest {
		TransactionRequest {
			from: self.from.map(Into::into),
			to: self.to.map(Into::into),
			gas_price: self.gas_price.map(Into::into),
			gas: self.gas.map(Into::into),
			value: self.value.map(Into::into),
			data: self.data.map(Into::into),
			nonce: self.nonce.map(Into::into),

		}
	}
}

//Bytes
/// Wrapper structure around vector of bytes.
#[derive(Debug, PartialEq, Eq, Default, Hash, Clone)]
pub struct Bytes(pub Vec<u8>);

impl Bytes {
	/// Simple constructor.
	pub fn new(bytes: Vec<u8>) -> Bytes {
		Bytes(bytes)
	}
	/// Convert back to vector
	pub fn into_vec(self) -> Vec<u8> {
		self.0
	}
}

impl From<Vec<u8>> for Bytes {
	fn from(bytes: Vec<u8>) -> Bytes {
		Bytes(bytes)
	}
}

impl Into<Vec<u8>> for Bytes {
	fn into(self) -> Vec<u8> {
		self.0
	}
}

// impl Serialize for Bytes {
// 	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
// 	where S: Serializer {
// 		let mut serialized = "0x".to_owned();
// 		serialized.push_str(self.0.to_hex().as_ref());
// 		serializer.serialize_str(serialized.as_ref())
// 	}
// }

// impl<'a> Deserialize<'a> for Bytes {
// 	fn deserialize<D>(deserializer: D) -> Result<Bytes, D::Error>
// 	where D: Deserializer<'a> {
// 		deserializer.deserialize_any(BytesVisitor)
// 	}
// }

struct BytesVisitor;
//Helpers
/// Transaction request coming from RPC with default values filled in.
#[derive(Debug, Clone, Default, Eq, PartialEq, Hash)]
pub struct FilledTransactionRequest {
	/// Sender
	pub from: Address,
	/// Indicates if the sender was filled by default value.
	pub used_default_from: bool,
	/// Recipient
	pub to: Option<Address>,
	/// Gas Price
	pub gas_price: U256,
	/// Gas
	pub gas: U256,
	/// Value of transaction in wei
	pub value: U256,
	/// Additional data sent with transaction
	pub data: Bytes,
	/// Transaction's nonce
	pub nonce: Option<U256>

}

impl From<FilledTransactionRequest> for TransactionRequest {
	fn from(r: FilledTransactionRequest) -> Self {
		TransactionRequest {
			from: Some(r.from),
			to: r.to,
			gas_price: Some(r.gas_price),
			gas: Some(r.gas),
			value: Some(r.value),
			data: Some(r.data),
			nonce: r.nonce,

		}
	}
}

//traits
#[rpc(server)]
pub trait TransactionRPC {
	/// Returns current gas_price.
	#[rpc(name = "eth_gasPrice")]
	fn gas_price(&self) -> BoxFuture<U256>;
}

//impls
pub struct TransactionRPCImpl;
impl TransactionRPCImpl {
	pub fn new() -> Self {
		TransactionRPCImpl {}
	}
}

impl TransactionRPC for TransactionRPCImpl {
	fn gas_price(&self) -> BoxFuture<U256> {
		let trx_count = U256::zero();
		let result = Ok(trx_count);
        Box::new(future::done(result))
	}


}



//Service Rpc_apis

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum Api {
	/// Transaction methods
	Transaction
}

#[derive(Debug, PartialEq, Clone)]
pub enum ApiSet {
	List(HashSet<Api>),
}

impl Default for ApiSet {
	fn default() -> Self {
		ApiSet::List(vec![Api::Transaction].into_iter().collect())
	}
}


// impl FromStr for Api {
// 	type Err = String;
// 	fn from_str(s: &str) -> Result<Self, Self::Err> {
// 		use self::Api::*;
// 		match s {
// 			"transactionRPC" => Ok(Api::Transaction),
// 			api => Err(format!("Unknown api: {}", api)),
// 		}
// 	}
// }

impl ApiSet {
	pub fn list_apis(&self) -> HashSet<Api> {
		match *self {
			ApiSet::List(ref apis) => apis.clone(),
		}
	}
}

pub fn setup_rpc(mut handler: MetaIoHandler<()>, apis: ApiSet) -> MetaIoHandler<()> {
	for api in apis.list_apis() {
		match api {
			Api::Transaction => handler.extend_with(TransactionRPCImpl::new().to_delegate()),
		}
	}

	handler
}




pub const DAPPS_DOMAIN: &'static str = "web3.site";

#[derive(Debug, Clone, PartialEq)]
pub struct HttpConfiguration {
	/// Is RPC over HTTP enabled (default is true)?
	pub enabled: bool,
	/// The IP of the network interface used (default is 127.0.0.1).
	pub interface: String,
	/// The network port (default is 8545).
	pub port: u16,
	/// The categories of RPC calls enabled.
    pub apis: ApiSet,
	/// CORS headers
	pub cors: Option<Vec<String>>,
	/// Specify a list of valid hosts we accept requests from.
	pub hosts: Option<Vec<String>>,
	/// Number of HTTP server threads to use to handle incoming requests (default is 4).
	pub server_threads: usize,
	/// Sets the maximum size of a request body in megabytes (default is 5 MiB).
	pub max_payload: usize,
	/// Use keepalive messages on the underlying socket: SO_KEEPALIVE as well as the TCP_KEEPALIVE
	/// or TCP_KEEPIDLE options depending on your platform (default is true).
	pub keep_alive: bool,
}

impl Default for HttpConfiguration {
	fn default() -> Self {
		HttpConfiguration {
			enabled: true,
			interface: "127.0.0.1".into(),
			port: 8545,
			apis: ApiSet::default(),
			cors: Some(vec![]),
			hosts: Some(vec![]),
			server_threads: 4,
			max_payload: 5,
			keep_alive: true,
		}
	}
}

fn setup_rpc_server(apis: ApiSet) -> MetaIoHandler<()> {
	setup_rpc(MetaIoHandler::with_compatibility(Compatibility::Both), apis)
}


fn into_domains<T: From<String>>(items: Option<Vec<String>>) -> DomainsValidation<T> {
	items.map(|vals| vals.into_iter().map(T::from).collect()).into()
}

fn with_domain(items: Option<Vec<String>>, domain: &str, dapps_address: &Option<Host>) -> Option<Vec<String>> {
	fn extract_port(s: &str) -> Option<u16> {
		s.split(':').nth(1).and_then(|s| s.parse().ok())
	}

	items.map(move |items| {
		let mut items = items.into_iter().collect::<HashSet<_>>();
		{
			let mut add_hosts = |address: &Option<Host>| {
				if let Some(host) = address.clone() {
					items.insert(host.to_string());
					items.insert(host.replace("127.0.0.1", "localhost"));
					items.insert(format!("http://*.{}", domain)); //proxypac
					if let Some(port) = extract_port(&*host) {
						items.insert(format!("http://*.{}:{}", domain, port));
					}
				}
			};

			add_hosts(dapps_address);
		}
		items.into_iter().collect()
	})
}


fn main() {

}

fn main() {

}
