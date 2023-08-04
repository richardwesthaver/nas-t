use yew::prelude::*;
use patternfly_yew::prelude::*;
#[macro_export]
macro_rules! page {
    ($title:expr => $file:expr) => {{

        html! {
            <>
                <$crate::index::Page title={$title} code={include_str!($file)}>{{include!($file)}}</$crate::index::Page>
            </>
        }
    }};
}

#[derive(Clone, Debug, Properties, PartialEq)]
pub struct Props {
    pub title: AttrValue,
    #[prop_or_default]
    pub subtitle: Children,
    #[prop_or_default]
    pub children: Children,
}

#[function_component(Page)]
pub fn page(props: &Props) -> Html {
    html! (
        <PageSectionGroup>
            <PageSection
                r#type={PageSectionType::Default}
                variant={PageSectionVariant::Light}
                limit_width=true
                sticky={[PageSectionSticky::Top]}
            >
                <Content>
                    <Title size={Size::XXXXLarge}>
                        { &props.title }
                    </Title>
                    { for props.subtitle.iter() }
                </Content>
            </PageSection>
            { for props.children.iter().map(|child|{
                html!(<PageSection>{child}</PageSection>)
            })}
        </PageSectionGroup>
    )
}

#[function_component(Index)]
pub fn index() -> Html {
    let subtitle = Html::from_html_unchecked(
        r#"<div>
<p>
This project acts both as a showcase for <strong>PatternFly Yew</strong>, as well as a quick-start project template.
</p>
</div>"#
            .into(),
    );
    html! {
        <>
            <Page title="Patternfly Yew Quickstart" {subtitle}>
                {"Pick an example on the left to learn more."}
            </Page>
        </>
    }
}
