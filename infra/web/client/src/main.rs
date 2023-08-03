// use gloo_net::http::Request;
// use wasm_bindgen_futures::spawn_local;
use yew::prelude::*;
use yew_router::prelude::*;
use client::form::MessageText;
use ybc::TileCtx::{Ancestor, Child, Parent};
#[derive(Clone, Routable, PartialEq)]
enum Route {
  #[at("/")]
  Home,
  #[at("/contact")]
  Contact,
  // #[at("/chat")]
  // Chat,
}

fn switch(routes: Route) -> Html {
    match routes {
      Route::Home => html! {
	<>
	  <ybc::Navbar
	    classes={classes!("is-success")}
	    padded=true
	    navbrand={html!{
	      <ybc::NavbarItem>
	        <ybc::Title classes={classes!("has-text-white")} size={ybc::HeaderSize::Is4}>
	          {"Home | About | Contact"}
                </ybc::Title>
              </ybc::NavbarItem>
            }}
            navstart={html!{}}
	    navend={html!{
	      <>
	        <ybc::NavbarItem>
	          <ybc::ButtonAnchor
	            classes={classes!("is-black","is-outlined")}
	            rel-{String::from("noopener noreferrer")}
	            target={String::from("_blank")}
                    href="https://nas-t.net">
	            {"Home"}
	          </ybc::ButtonAnchor>
                </ybc::NavbarItem>
	        <ybc::NavbarItem>
	          <ybc::ButtonAnchor
	            classes={classes!("is-black","is-outlined")}
	            rel-{String::from("noopener noreferrer")}
	            target={String::from("_blank")}
                    href="/about">
	            {"About"}
	          </ybc::ButtonAnchor>
                </ybc::NavbarItem>
	        <ybc::NavbarItem>
	          <ybc::ButtonAnchor
	            classes={classes!("is-black","is-outlined")}
	            rel-{String::from("noopener noreferrer")}
	            target={String::from("_blank")}
                    href="/contact">
	            {"Contact"}
	          </ybc::ButtonAnchor>
                </ybc::NavbarItem>

	    }}
	/>
        <ybc::Hero
            classes={classes!("is-light")}
            size={ybc::HeroSize::FullheightWithNavbar}
            body={html!{
                <ybc::Container classes={classes!("is-centered")}>
                <ybc::Tile ctx={Ancestor}>
                    <ybc::Tile ctx={Parent} size={ybc::TileSize::Twelve}>
                        <ybc::Tile ctx={Parent}>
                            <ybc::Tile ctx={Child} classes={classes!("notification", "is-success")}>
                                <ybc::Subtitle size={ybc::HeaderSize::Is3} classes={classes!("has-text-white")}>{"Trunk"}</ybc::Subtitle>
                                <p>{"Trunk is a WASM web application bundler for Rust."}</p>
                            </ybc::Tile>
                        </ybc::Tile>
                        <ybc::Tile ctx={Parent}>
                            <ybc::Tile ctx={Child} classes={classes!("notification", "is-success")}>
                                <ybc::Icon size={ybc::Size::Large} classes={classes!("is-pulled-right")}><img src="yew.svg"/></ybc::Icon>
                                <ybc::Subtitle size={ybc::HeaderSize::Is3} classes={classes!("has-text-white")}>
                                    {"Yew"}
                                </ybc::Subtitle>
                                <p>{"Yew is a modern Rust framework for creating multi-threaded front-end web apps with WebAssembly."}</p>
                            </ybc::Tile>
                        </ybc::Tile>
                        <ybc::Tile ctx={Parent}>
                            <ybc::Tile ctx={Child} classes={classes!("notification", "is-success")}>
                                <ybc::Subtitle size={ybc::HeaderSize::Is3} classes={classes!("has-text-white")}>{"YBC"}</ybc::Subtitle>
                                <p>{"A Yew component library based on the Bulma CSS framework."}</p>
                            </ybc::Tile>
                        </ybc::Tile>
                    </ybc::Tile>
                </ybc::Tile>
                </ybc::Container>
            }}>
        </ybc::Hero>
      </>
      },
      Route::Contact => html! { <MessageText /> },
    }
}

#[function_component(App)]
fn app() -> Html {
    html! {
        <BrowserRouter>
            <Switch<Route> render={switch} />
        </BrowserRouter>
    }
}

fn main() {
    wasm_logger::init(wasm_logger::Config::new(log::Level::Trace));
    yew::Renderer::<App>::new().render();
}
