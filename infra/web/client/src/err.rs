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

pub type Result<T> = Result<T,Error>;
