use patternfly_yew::prelude::*;
use yew::prelude::*;
use crate::{ contact::Contact,
  index::*, about::About,
};

#[hook]
pub fn use_open<IN>(url: impl Into<String>, target: impl Into<String>) -> Callback<IN, ()>
where
    IN: 'static,
{
    use_callback(
        |_, (url, target)| {
            let _ = gloo_utils::window().open_with_url_and_target(&url, &target);
        },
        (url.into(), target.into()),
    )
}

#[function_component(App)]
pub fn app() -> Html {
    html! {
        <BackdropViewer>
            <ToastViewer>
	<AppPage><Index/></AppPage>
            </ToastViewer>
        </BackdropViewer>
    }
}

#[derive(Clone, Debug, PartialEq, Properties)]
pub struct PageProps {
    pub children: Children,
}

#[function_component(AppPage)]
fn page(props: &PageProps) -> Html {
  let callback_home = use_open(
    "https://nas-t.net","_self");
  let callback_lab = use_open(
    "https://lab.rwest.io/comp/nas-t",
    "_blank"
  );

  let callback_github = use_open(
    "https://github.com/richardwesthaver/nas-t",
    "_blank",
  );

  let brand = html!(
    <MastheadBrand  onclick={callback_home}>
      <Brand src="logo.svg" alt="NAS-T Logo"/>
      </MastheadBrand>);

  let backdropper = use_backdrop();

  let onabout = use_callback(
    |_,bd|
    if let Some(bd) = bd {
      bd.open(html!(<About/>))},
    backdropper.clone());

  let oncontact = use_callback(
    |_,bd|
    if let Some(bd) = bd {
      bd.open(html!(<Contact/>))},
    backdropper);

  let onthemeswitch = Callback::from(|state| match state {
    true => gloo_utils::document_element().set_class_name("pf-v5-theme-dark"),
    false => gloo_utils::document_element().set_class_name(""),
  });

  let tools = html!(
    <Toolbar full_height=true>
      <ToolbarContent>
      <ToolbarGroup
      modifiers={ToolbarElementModifier::Right.all()}
    variant={GroupVariant::IconButton}
    >
      <ToolbarItem>
      <patternfly_yew::prelude::Switch onchange={onthemeswitch} label="Dark Theme" />
      </ToolbarItem>
      <ToolbarItem>
      <Button variant={ButtonVariant::Plain} icon={Icon::Gitlab} onclick={callback_lab}/>
      <Button variant={ButtonVariant::Plain} icon={Icon::Github} onclick={callback_github}/>
      </ToolbarItem>
      <ToolbarItem>
      <Dropdown
      position={Position::Right}
    icon={Icon::QuestionCircle}
    variant={MenuToggleVariant::Plain}>
      <MenuAction onclick={onabout}>{"About"}</MenuAction>
      <MenuAction onclick={oncontact}>{"Contact"}</MenuAction>
      </Dropdown>
      </ToolbarItem>
      </ToolbarGroup>
      </ToolbarContent>
      </Toolbar>
  );
  
  html! (
    <Page {brand} {tools}>
    { for props.children.iter() }
    </Page>
  )
}
