use yew::prelude::*;
use yewdux::prelude::*;
use yewdux_input::InputDispatch;

#[derive(Store,Default,PartialEq,Clone)]
pub struct ContactForm {
  message: String,
  email: String,
  name: String,
}

#[function_component]
pub fn MessageText() -> Html {
  let (store, dispatch) = use_store::<ContactForm>();
  let oninput = dispatch.input(|s,message| {
    ContactForm {
      message,
      ..s.as_ref().clone()
    }.into()
  });
  html! {
    <>
      <p>{&store.message}</p>
      <input {oninput} />
    </>
  }  
}
