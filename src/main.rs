use std::net::{IpAddr, Ipv4Addr, SocketAddr};
use jsonrpc_core;
use jsonrpc_core::{Compatibility,MetaIoHandler,BoxFuture, Result};
use jsonrpc_http_server as http;
use std::fmt;
use serde_derive::{Serialize, Deserialize};
use ethereum_types::{U256};
use jsonrpc_derive::rpc;
use jsonrpc_core::futures::{future};
use std::collections::HashSet;
pub type HttpServer = http::Server;
pub use http::{
	hyper,
    Host
};
#[derive(Debug, Clone, PartialEq)]
pub struct HttpConfiguration {
    pub apis: ApiSet,
}
pub struct MetaExtractor<T> {extractor: T,}
pub struct RpcExtractor;
pub struct TransactionRPCImpl;

pub fn new_http(
	id: &str,
	options: &str,
	conf: HttpConfiguration,
) -> Result<Option<HttpServer>> {
	let addr = SocketAddr::new(IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)), 8080);
	let handler = setup_rpc_server(conf.apis);
	let start_result = start_http(
		&addr,
        handler,
		RpcExtractor,
	);
	match start_result {
		Ok(server) => Ok(Some(server)),

	}
}
/// Start http server asynchronously and returns result with `Server` handle on success or an error.
pub fn start_http<M, S, H, T>(
	addr: &SocketAddr,
	handler: H,
	extractor: T,
) -> ::std::io::Result<HttpServer> where
	M: jsonrpc_core::Metadata,
	S: jsonrpc_core::Middleware<M>,
	H: Into<jsonrpc_core::MetaIoHandler<M, S>>,
	T: HttpMetaExtractor<Metadata=M>,
{
	let extractor = MetaExtractor::new(extractor);
	Ok(http::ServerBuilder::with_meta_extractor(handler, extractor)
	.start_http(addr)?)
}

//cal the rpc_api fuc
fn setup_rpc_server(apis: ApiSet) -> MetaIoHandler<()> {
	setup_rpc(MetaIoHandler::with_compatibility(Compatibility::Both), apis)
}
//Http meta Extractor
pub trait HttpMetaExtractor: Send + Sync + 'static {
	type Metadata: jsonrpc_core::Metadata;
	fn read_metadata(&self, origin: Option<String>, user_agent: Option<String>) -> Self::Metadata;
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


//struct metadata
#[derive(Clone, Default, Debug)]
pub struct Metadata {
	pub origin: Origin,
}
impl jsonrpc_core::Metadata for Metadata {}

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
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
#[serde(rename_all = "kebab-case")]
pub enum Origin {
	/// RPC server (includes request origin)
	Rpc(String),
	/// Unknown
	Unknown,
}
//traits for rpc
#[rpc(server)]
pub trait TransactionRPC {
	#[rpc(name = "eth_gasPrice")]
	fn gas_price(&self) -> BoxFuture<U256>;
}

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

//Service rpc_apis
#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum Api {
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

fn main(){}