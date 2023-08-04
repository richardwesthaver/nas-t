use patternfly_yew::prelude::*;
use yew::prelude::*;
#[function_component(About)]
pub fn about() -> Html {
    html!(
        <Bullseye>
            <AboutModal
                background_image_src="/assets/images/pfbg-icon.svg"
                brand_image_src="logo.png"
                brand_image_alt="NAS-T logo"
                product_name="NAS-T"
                trademark="Copyright © 2023 NAS-T"
            >
                    <p>{ "NAS-T is a grassroots Network Area Server (NAS) technology and service
provider based in New London, CT. We are a group of IT and Data
specialists offering a replacement for managed cloud storage services
such as Google Drive, DropBox, iCloud, and OneDrive.

At NAS-T we value your privacy /and/ your right to own your data
without exception. We aim to provide the best /private/ cloud storage
service software using cutting-edge technology which the large,
entrenched service providers aren't capable of implementing, at no
cost. We aim to build a community of NAS enthusiasts and experts who
can share their experience and empower their local communities. We
will sell hardware and services at fair and transparent costs to
support our mission and encourage everyone to take back their data."
 }</p>
                    <br />
                    <DescriptionList mode={DescriptionListMode::Horizontal}>
                        <DescriptionGroup term="Version">{env!("CARGO_PKG_VERSION")}</DescriptionGroup>
                        <DescriptionGroup term="Name">{"NAS-T"}</DescriptionGroup>
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
