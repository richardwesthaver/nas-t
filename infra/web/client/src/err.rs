#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
  Unauthorized,
  Forbidden,
  NotFound,
  UnprocessableEntity(String),
  InternalServerError,
  DeserializeError,
  RequestError,
}
