//! main.rs --- web-server
use std::{
  net::{IpAddr, Ipv6Addr, SocketAddr},
  path::PathBuf,
  str::FromStr,
};
use axum::{
  body::{boxed, Body},
  http::{Response, StatusCode},
  response::IntoResponse,
  routing::get,
  Router,
};
use tokio::fs;
use tower::{ServiceBuilder, ServiceExt};
use tower_http::{services::ServeDir,trace::TraceLayer};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};
use clap::Parser;

#[derive(Parser, Debug)]
#[clap(name = "web-server", about = "www.nas-t.net web services")]
struct Opt {
    /// set the log level
    #[clap(short = 'l', long = "log", default_value = "debug")]
    log_level: String,

    /// set the listen addr
    #[clap(short = 'a', long = "addr", default_value = "::1")]
    addr: String,

    /// set the listen port
    #[clap(short = 'p', long = "port", default_value = "13008")]
    port: u16,

    /// set the directory where static files are to be found
    #[clap(long = "static-dir", default_value = "../dist")]
    static_dir: String,
}

#[tokio::main]
async fn main() {
  let opt = Opt::parse();
  // enable console logging
  tracing_subscriber::registry()
    .with(
      tracing_subscriber::EnvFilter::try_from_default_env()
	.unwrap_or_else(|_| format!("{},hyper=info,mio=info", opt.log_level).into()))
    .with(tracing_subscriber::fmt::layer())
    .init();

  // Set up application state for use with with_state().
  // let user_set = Mutex::new(HashSet::new());
  // let (tx, _) = broadcast::channel(100);
  // let chat_state = Arc::new(ChatState{user_set, tx});

  let app = Router::new()
//    .route("/chat", get(ws_handler))
//    .with_state(chat_state)
   .route("/api/hello", get(hello))
    .fallback_service(get(|req| async move {
      match ServeDir::new(&opt.static_dir).oneshot(req).await {
        Ok(res) => {
          let status = res.status();
          match status {
            StatusCode::NOT_FOUND => {
              let index_path = PathBuf::from(&opt.static_dir).join("index.html");
              let index_content = match fs::read_to_string(index_path).await {
                Err(_) => {
                  return Response::builder()
                    .status(StatusCode::NOT_FOUND)
                    .body(boxed(Body::from("index file not found")))
                    .unwrap()
                }
                Ok(index_content) => index_content,
              };
	      
              Response::builder()
                .status(StatusCode::OK)
                .body(boxed(Body::from(index_content)))
                .unwrap()
            }
            _ => res.map(boxed),
          }
        }
        Err(err) => Response::builder()
          .status(StatusCode::INTERNAL_SERVER_ERROR)
          .body(boxed(Body::from(format!("error: {err}"))))
          .expect("error response"),
      }
    }))
    .layer(ServiceBuilder::new().layer(TraceLayer::new_for_http()));
  
  let sock_addr = SocketAddr::from((
    IpAddr::from_str(opt.addr.as_str()).unwrap_or(IpAddr::V6(Ipv6Addr::LOCALHOST)),
    opt.port,
  ));

  log::info!("listening on http://{}", sock_addr);

  axum::Server::bind(&sock_addr)
    .serve(app.into_make_service())
    .await
    .expect("Unable to start server");
}

async fn hello() -> impl IntoResponse {
    "hello from server!"
}
