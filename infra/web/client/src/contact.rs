use yew::prelude::*;
use patternfly_yew::prelude::*;

#[function_component(Contact)]
pub fn contact() -> Html {
  let value = use_state_eq(String::default);
  let onchange = use_callback(|new,value|value.set(new), value.clone());
  let submit = use_callback(|_,value| log::info!("submitted value: {:?}", value), value.clone());
  let validator = Validator::from(|ctx: ValidationContext<String>| {
    if ctx.initial {
      ValidationResult::default()
    } else if ctx.value.is_empty() {
      ValidationResult::error("Must not be empty")
    } else {
      ValidationResult::default()
    }});
  html!{
    <Form>
      <FormGroupValidated<TextInput>
      label="E-mail"
      validator={validator.clone()}
    >
      <TextInput value={(*value).clone()}
    onchange={onchange.clone()}
    placeholder="jane.doe@example.com"
    autofocus=true/>
      </FormGroupValidated<TextInput>>
      <FormGroupValidated<TextInput>
      label="Message"
      {validator}
    >
      <TextInput value={(*value).clone()}
    {onchange}
    placeholder="Optional Message"
    autofocus=true/>
      </FormGroupValidated<TextInput>>
      <ActionGroup>
      <Button variant={ButtonVariant::Primary} label="Submit" onclick={submit}/>
      </ActionGroup>
      </Form>
  }
}
