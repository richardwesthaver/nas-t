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
                variant={PageSectionVariant::Dark}
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
      "<div><code>The NAS Technology Company</code></div>"
        .into(),
    );
  let content = Html::from_html_unchecked(crate::CONTENT.into());
    html! {
      <>
        <Page title="NAS-T" {subtitle}>
      {content}
      </Page>
        </>
    }
}
