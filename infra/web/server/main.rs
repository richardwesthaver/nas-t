//! main.rs --- nas-t web-server

//! This is a small micro-service designed to serve simple routes.

//! ROUTES:
//! - mailme :: verify and append email to a file

use axum::{
  routing::post,
  Router,
  Form,
};
use clap::Parser;
use std::{
  net::{IpAddr, Ipv6Addr, SocketAddr},
  str::FromStr,
  path::PathBuf,
};
use serde::Deserialize;
use validator::Validate;
use tower::ServiceBuilder;
use tower_http::trace::TraceLayer;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

#[derive(Debug,Validate,Deserialize)]
struct User {
  #[validate(email)]
  mail: String,
  #[validate(length(min=1,max=88))]
  name: String,
  #[validate(length(max=1024))]
  message: String,
}

async fn user(Form(user): Form<User>) -> &'static str {
  if let Ok(user) = user.validate() {
    log::info!("added user: {:?}", user);
    "OK" } else { "ERR" }}

#[derive(Parser, Debug)]
#[clap(name = "web-server", about = "nas-t.net micro-service")]
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

  #[clap(short = 'f', long = "file", default_value = "mailme.list")]
  file: PathBuf,
}

#[tokio::main]
async fn main() {
  let opt = Opt::parse();

  tracing_subscriber::registry()
    .with(
      tracing_subscriber::EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| format!("{},hyper=info,mio=info", opt.log_level).into()),
    )
    .with(tracing_subscriber::fmt::layer())
    .init();

  // Set up application state for use with with_state().
  // let user_set = Mutex::new(HashSet::new());
  // let (tx, _) = broadcast::channel(100);
  // let chat_state = Arc::new(ChatState{user_set, tx});

  let app = Router::new()
    //    .route("/chat", get(ws_handler))
    //    .with_state(chat_state)
    .route("/user", post(move |u| user(u)))
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
