use yew::prelude::*;
use patternfly_yew::prelude::*;
#[function_component(MessageText)]
pub fn message_text() -> Html {
  let value = use_state_eq(String::default);
  let onchange = {
    let value = value.clone();
    Callback::from(move |data| value.set(data))};
  html!{<TextInput value={(*value).clone()}/>}
}
