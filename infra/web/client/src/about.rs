use patternfly_yew::prelude::*;
use yew::prelude::*;

#[function_component(About)]
pub fn about() -> Html {
    html!(
      <Bullseye
	plain=true>
        <AboutModal
        background_image_src="nas-t-cloud-w.png"
        brand_image_src="logo.png"
        brand_image_alt="NAS-T logo"
        product_name="The NAS Technology Company"
        trademark="Copyright © 2023 NAS-T">
        <p>{crate::ABOUT}</p>
        <br />
        <DescriptionList mode={DescriptionListMode::Horizontal}>
        <DescriptionGroup term="Version">{env!("CARGO_PKG_VERSION")}</DescriptionGroup>
        <DescriptionGroup term="License">{env!("CARGO_PKG_LICENSE")}</DescriptionGroup>
        if let Some(value) = option_env!("BUILD_COMMIT") {
          <DescriptionGroup term="Build commit">{value}</DescriptionGroup>
        }
      if let Some(value) = option_env!("BUILD_TIMESTAMP") {
        <DescriptionGroup term="Build timestamp">{value}</DescriptionGroup>
      }
      </DescriptionList>
        </AboutModal>
        </Bullseye>
    )
}
