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
  let about = Html::from_html_unchecked(crate::ABOUT.into());
  let motivation = Html::from_html_unchecked(crate::MOTIVATION.into());
  let solution = Html::from_html_unchecked(crate::SOLUTION.into());
  let mission = Html::from_html_unchecked(crate::MISSION.into());
  let products = Html::from_html_unchecked(crate::PRODUCTS.into());
  let services = Html::from_html_unchecked(crate::SERVICES.into());
    html! {
      <>
        <Page title="NAS-T" {subtitle}>
	<Grid gutter=true>
	<GridItem cols={[10]}><Card><CardBody>{about}</CardBody></Card></GridItem>
	<GridItem rows={[2]}>{""}</GridItem>
	<GridItem rows={[2]}>{""}</GridItem>
	<GridItem cols={[1]} rows={[3]}>{""}</GridItem>
	<GridItem cols={[8]} rows={[3]}><Card><CardBody>{motivation}</CardBody></Card></GridItem>
	<GridItem cols={[3]} rows={[3]}>{"TODO"}</GridItem>
	<GridItem rows={[2]}>{""}</GridItem>
	<GridItem cols={[1]} rows={[3]}>{""}</GridItem>
	<GridItem cols={[8]} rows={[3]}><Card><CardBody>{solution}</CardBody></Card></GridItem>
	<GridItem cols={[3]} rows={[3]}>{"TODO"}</GridItem>
	<GridItem rows={[2]}>{""}</GridItem>
	<GridItem cols={[12]} rows={[4]}>{""}</GridItem>
	<GridItem cols={[2]} rows={[4]}>{"TODO"}</GridItem>
	<GridItem cols={[8]} rows={[4]}><Card><CardBody>{mission}</CardBody></Card></GridItem>
	<GridItem cols={[2]} rows={[4]}>{"TODO"}</GridItem>
	<GridItem cols={[12]} rows={[2]}>{""}</GridItem>
	<GridItem cols={[6]}><Card><CardBody>{products}</CardBody></Card></GridItem>
	<GridItem cols={[6]}><Card><CardBody>{services}</CardBody></Card></GridItem>

      </Grid>
      </Page>
        </>
    }
}
