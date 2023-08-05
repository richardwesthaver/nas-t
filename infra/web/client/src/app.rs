use patternfly_yew::prelude::*;
use yew::prelude::*;
use yew_nested_router::prelude::{Switch as RouterSwitch, *};
use yew_nested_router::Target;
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

#[derive(Debug, Default, Clone, PartialEq, Eq, Target)]
pub enum AppRoute {
//    Component(Component),
//    #[target(rename = "fullpage")]
//    FullPageExample(FullPage),
//    Layout(Layout),
  #[default]
  Index,
  About,
  Contact,
}

#[function_component(App)]
pub fn app() -> Html {
    html! {
        <BackdropViewer>
            <ToastViewer>
                <Router<AppRoute> default={AppRoute::Index}>
                    <RouterSwitch<AppRoute> render={switch_app_route} />
                </Router<AppRoute>>
            </ToastViewer>
        </BackdropViewer>
    }
}

fn switch_app_route(target:AppRoute) -> Html {
  match target {
    AppRoute::Index => html!{<AppPage><Index /></AppPage>},
    AppRoute::About => html!{<AppPage><About /></AppPage>},
    AppRoute::Contact => html!{<AppPage><Contact /></AppPage>},
  }
}

#[derive(Clone, Debug, PartialEq, Properties)]
pub struct PageProps {
    pub children: Children,
}

#[function_component(AppPage)]
fn page(props: &PageProps) -> Html {
    let callback_lab = use_open(
        "https://lab.rwest.io/comp/nas-t",
        "_blank",
    );

    let callback_github = use_open(
        "https://github.com/richardwesthaver/nas-t",
        "_blank",
    );

    let backdropper = use_backdrop();

    let onabout = Callback::from(move |_| {
        if let Some(backdropper) = &backdropper {
            backdropper.open(html!(<About/>));
        }
    });

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
        <Button variant={ButtonVariant::Link} icon={Icon::InfoCircle} onclick={onabout}>{"About"}</Button>
        </ToolbarItem>
        </ToolbarGroup>
        </ToolbarContent>
        </Toolbar>
    );

    html! (
        <Page {tools}>
            { for props.children.iter() }
        </Page>
    )
}
