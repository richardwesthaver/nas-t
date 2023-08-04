#![recursion_limit = "1024"]
// use gloo_net::http::Request;
// use wasm_bindgen_futures::spawn_local;
use browser_panic_hook::{CustomBody, IntoPanicHook};
// use wasm_bindgen::prelude::*;
use client::app;
#[cfg(not(debug_assertions))]
const LOG_LEVEL: log::Level = log::Level::Info;
#[cfg(debug_assertions)]
const LOG_LEVEL: log::Level = log::Level::Trace;
fn main() {
    wasm_logger::init(wasm_logger::Config::new(LOG_LEVEL));
    yew::set_custom_panic_hook(
        CustomBody(Box::new(|details| {
            format!(
                r#"
<div class="pf-v5-l-bullseye">
  <div class="pf-v5-l-bullseye__item">
    <div class="pf-v5-c-alert pf-m-danger" aria-label="app died">
      <div class="pf-v5-c-alert__icon">
        <i class="fas fa-fw fa-exclamation-circle" aria-hidden="true"></i>
      </div>
      <p class="pf-v5-c-alert__title">
        <span class="pf-v5-screen-reader">Panic alert:</span>
        Nas-T crashed :(
      </p>
      <div class="pf-v5-c-alert__description">
        <p>The application failed critically. Please Re-init.</p>
        <p>Reason: <pre>{message}</pre></p>
      </div>
    </div>
  </div>
</div>
"#,
              message = details.message()
            )
        }))
        .into_panic_hook(),
    );
    yew::Renderer::<app::App>::new().render();
}
