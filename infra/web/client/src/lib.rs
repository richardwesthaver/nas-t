// pub mod err;
// pub use err::{Error, Result};
pub mod form;
pub mod index;
pub mod about;
pub mod app;

pub const CONTENT: &'static str = include_str!("../content.html");
pub const ABOUT: &'static str = include_str!("../about.html");
pub const MOTIVATION: &'static str = include_str!("../motivation.html");
pub const SOLUTION: &'static str = include_str!("../solution.html");
pub const MISSION: &'static str = include_str!("../mission.html");
pub const PRODUCTS: &'static str = include_str!("../products.html");
pub const SERVICES: &'static str = include_str!("../services.html");
