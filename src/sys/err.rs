//! sys/err.rs --- System Errors

//! Code:
use std::os::raw::c_char;
use std::ffi::CStr;
use btrfsutil_sys::{
  btrfs_util_error,
  BTRFS_UTIL_OK,
  BTRFS_UTIL_ERROR_NO_MEMORY,
  BTRFS_UTIL_ERROR_INVALID_ARGUMENT,
  BTRFS_UTIL_ERROR_STOP_ITERATION,
  BTRFS_UTIL_ERROR_NOT_BTRFS,
  BTRFS_UTIL_ERROR_NOT_SUBVOLUME,
  BTRFS_UTIL_ERROR_SUBVOLUME_NOT_FOUND,
  BTRFS_UTIL_ERROR_OPEN_FAILED,
  BTRFS_UTIL_ERROR_RMDIR_FAILED,
  BTRFS_UTIL_ERROR_UNLINK_FAILED,
  BTRFS_UTIL_ERROR_STAT_FAILED,
  BTRFS_UTIL_ERROR_STATFS_FAILED,
  BTRFS_UTIL_ERROR_SEARCH_FAILED,
  BTRFS_UTIL_ERROR_INO_LOOKUP_FAILED,
  BTRFS_UTIL_ERROR_SUBVOL_GETFLAGS_FAILED,
  BTRFS_UTIL_ERROR_SUBVOL_SETFLAGS_FAILED,
  BTRFS_UTIL_ERROR_SUBVOL_CREATE_FAILED,
  BTRFS_UTIL_ERROR_SNAP_CREATE_FAILED,
  BTRFS_UTIL_ERROR_SNAP_DESTROY_FAILED,
  BTRFS_UTIL_ERROR_DEFAULT_SUBVOL_FAILED,
  BTRFS_UTIL_ERROR_SYNC_FAILED,
  BTRFS_UTIL_ERROR_START_SYNC_FAILED,
  BTRFS_UTIL_ERROR_WAIT_SYNC_FAILED,
  BTRFS_UTIL_ERROR_GET_SUBVOL_INFO_FAILED,
  BTRFS_UTIL_ERROR_GET_SUBVOL_ROOTREF_FAILED,
  BTRFS_UTIL_ERROR_INO_LOOKUP_USER_FAILED,
  BTRFS_UTIL_ERROR_FS_INFO_FAILED,
};

pub type SysResult<T> = std::result::Result<T, SysError>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SysError {
  Btrfs(BtrfsSysError),
  BtrfsUtil(BtrfsUtilSysError),
  Utf8(std::str::Utf8Error),
  Dl(DlError),
  UnknownErrorCode(u32),
}

impl std::fmt::Display for SysError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      SysError::Btrfs(ref e) => e.fmt(f),
      SysError::BtrfsUtil(ref e) => e.fmt(f),
      SysError::Utf8(ref e) => e.fmt(f),
      SysError::Dl(ref e) => e.fmt(f),
      SysError::UnknownErrorCode(ref n) => f.write_fmt(format_args!("invalid error code: {}", n)),
    }
  }
}

impl std::error::Error for SysError {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    match *self {
      SysError::Btrfs(ref e) => Some(e),
      SysError::BtrfsUtil(ref e) => Some(e),
      SysError::Utf8(ref e) => Some(e),
      SysError::Dl(ref e) => Some(e),
      SysError::UnknownErrorCode(_) => None,
    }
  }
}

impl From<std::str::Utf8Error> for SysError {
  fn from(e:std::str::Utf8Error) -> Self {
    SysError::Utf8(e)
  }
}

impl From<BtrfsSysError> for SysError {
  fn from(e:BtrfsSysError) -> Self {
    SysError::Btrfs(e)
  }
}

impl From<BtrfsUtilSysError> for SysError {
  fn from(e:BtrfsUtilSysError) -> Self {
    SysError::BtrfsUtil(e)
  }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum BtrfsSysError {
  Unknown(String),
}

impl std::fmt::Display for BtrfsSysError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      BtrfsSysError::Unknown(s) => s.fmt(f),
    }
  }
}

impl std::error::Error for BtrfsSysError {}


#[derive(Clone, Debug, Eq, PartialEq)]
pub enum BtrfsUtilSysError {
  Ok = BTRFS_UTIL_OK as isize,
  StopIteration = BTRFS_UTIL_ERROR_STOP_ITERATION as isize,
  NoMemory = BTRFS_UTIL_ERROR_NO_MEMORY as isize,
  InvalidArgument = BTRFS_UTIL_ERROR_INVALID_ARGUMENT as isize,
  NotBtrfs = BTRFS_UTIL_ERROR_NOT_BTRFS as isize,
  NotSubvolume = BTRFS_UTIL_ERROR_NOT_SUBVOLUME as isize,
  SubvolumeNotFound = BTRFS_UTIL_ERROR_SUBVOLUME_NOT_FOUND as isize,
  OpenFailed = BTRFS_UTIL_ERROR_OPEN_FAILED as isize,
  RmdirFailed = BTRFS_UTIL_ERROR_RMDIR_FAILED as isize,
  UnlinkFailed = BTRFS_UTIL_ERROR_UNLINK_FAILED as isize,
  StatFailed = BTRFS_UTIL_ERROR_STAT_FAILED as isize,
  StatfsFailed = BTRFS_UTIL_ERROR_STATFS_FAILED as isize,
  SearchFailed = BTRFS_UTIL_ERROR_SEARCH_FAILED as isize,
  InoLookupFailed = BTRFS_UTIL_ERROR_INO_LOOKUP_FAILED as isize,
  SubvolGetflagsFailed = BTRFS_UTIL_ERROR_SUBVOL_GETFLAGS_FAILED as isize,
  SubvolSetflagsFailed = BTRFS_UTIL_ERROR_SUBVOL_SETFLAGS_FAILED as isize,
  SubvolCreateFailed = BTRFS_UTIL_ERROR_SUBVOL_CREATE_FAILED as isize,
  SnapCreateFailed = BTRFS_UTIL_ERROR_SNAP_CREATE_FAILED as isize,
  SnapDestroyFailed = BTRFS_UTIL_ERROR_SNAP_DESTROY_FAILED as isize,
  DefaultSubvolFailed = BTRFS_UTIL_ERROR_DEFAULT_SUBVOL_FAILED as isize,
  SyncFailed = BTRFS_UTIL_ERROR_SYNC_FAILED as isize,
  StartSyncFailed = BTRFS_UTIL_ERROR_START_SYNC_FAILED as isize,
  WaitSyncFailed = BTRFS_UTIL_ERROR_WAIT_SYNC_FAILED as isize,
  GetSubvolInfoFailed = BTRFS_UTIL_ERROR_GET_SUBVOL_INFO_FAILED as isize,
  GetSubvolRootrefFailed = BTRFS_UTIL_ERROR_GET_SUBVOL_ROOTREF_FAILED as isize,
  InoLookupUserFailed = BTRFS_UTIL_ERROR_INO_LOOKUP_USER_FAILED as isize,
  FsInfoFailed = BTRFS_UTIL_ERROR_FS_INFO_FAILED as isize,
}

impl BtrfsUtilSysError {
  pub fn strerror(&self) -> SysResult<&'static str> {
    let err_str_ptr: *const c_char;
    let errno = self.clone() as u32;
    let errno = errno as u32;
    unsafe {
      err_str_ptr = btrfsutil_sys::btrfs_util_strerror(errno as u32);
    }
    let cstr: &CStr = unsafe { CStr::from_ptr(err_str_ptr) };
    match cstr.to_str() {
      Ok(val) => Ok(val),
      Err(e) => Err(e.into()),
    }
  }
}

impl TryFrom<btrfs_util_error> for BtrfsUtilSysError {
  type Error = SysError;
  fn try_from(errno: btrfs_util_error) -> SysResult<Self> {
    match errno {
      BTRFS_UTIL_OK => Ok(BtrfsUtilSysError::Ok),
      BTRFS_UTIL_ERROR_STOP_ITERATION => Ok(BtrfsUtilSysError::StopIteration),
      BTRFS_UTIL_ERROR_NO_MEMORY => Ok(BtrfsUtilSysError::NoMemory),
      BTRFS_UTIL_ERROR_INVALID_ARGUMENT => Ok(BtrfsUtilSysError::InvalidArgument),
      BTRFS_UTIL_ERROR_NOT_BTRFS => Ok(BtrfsUtilSysError::NotBtrfs),
      BTRFS_UTIL_ERROR_NOT_SUBVOLUME => Ok(BtrfsUtilSysError::NotSubvolume),
      BTRFS_UTIL_ERROR_SUBVOLUME_NOT_FOUND => Ok(BtrfsUtilSysError::SubvolumeNotFound),
      BTRFS_UTIL_ERROR_OPEN_FAILED => Ok(BtrfsUtilSysError::OpenFailed),
      BTRFS_UTIL_ERROR_RMDIR_FAILED => Ok(BtrfsUtilSysError::RmdirFailed),
      BTRFS_UTIL_ERROR_UNLINK_FAILED => Ok(BtrfsUtilSysError::UnlinkFailed),
      BTRFS_UTIL_ERROR_STAT_FAILED => Ok(BtrfsUtilSysError::StatFailed),
      BTRFS_UTIL_ERROR_STATFS_FAILED => Ok(BtrfsUtilSysError::StatfsFailed),
      BTRFS_UTIL_ERROR_SEARCH_FAILED => Ok(BtrfsUtilSysError::SearchFailed),
      BTRFS_UTIL_ERROR_INO_LOOKUP_FAILED => Ok(BtrfsUtilSysError::InoLookupFailed),
      BTRFS_UTIL_ERROR_SUBVOL_GETFLAGS_FAILED => Ok(BtrfsUtilSysError::SubvolGetflagsFailed),
      BTRFS_UTIL_ERROR_SUBVOL_SETFLAGS_FAILED => Ok(BtrfsUtilSysError::SubvolSetflagsFailed),
      BTRFS_UTIL_ERROR_SUBVOL_CREATE_FAILED => Ok(BtrfsUtilSysError::SubvolCreateFailed),
      BTRFS_UTIL_ERROR_SNAP_CREATE_FAILED => Ok(BtrfsUtilSysError::SnapCreateFailed),
      BTRFS_UTIL_ERROR_SNAP_DESTROY_FAILED => Ok(BtrfsUtilSysError::SnapDestroyFailed),
      BTRFS_UTIL_ERROR_DEFAULT_SUBVOL_FAILED => Ok(BtrfsUtilSysError::DefaultSubvolFailed),
      BTRFS_UTIL_ERROR_SYNC_FAILED => Ok(BtrfsUtilSysError::SyncFailed),
      BTRFS_UTIL_ERROR_START_SYNC_FAILED => Ok(BtrfsUtilSysError::StartSyncFailed),
      BTRFS_UTIL_ERROR_WAIT_SYNC_FAILED => Ok(BtrfsUtilSysError::WaitSyncFailed),
      BTRFS_UTIL_ERROR_GET_SUBVOL_INFO_FAILED => Ok(BtrfsUtilSysError::GetSubvolInfoFailed),
      BTRFS_UTIL_ERROR_GET_SUBVOL_ROOTREF_FAILED => Ok(BtrfsUtilSysError::GetSubvolRootrefFailed),
      BTRFS_UTIL_ERROR_INO_LOOKUP_USER_FAILED => Ok(BtrfsUtilSysError::InoLookupUserFailed),
      BTRFS_UTIL_ERROR_FS_INFO_FAILED => Ok(BtrfsUtilSysError::FsInfoFailed),
      _ => Err(SysError::UnknownErrorCode(errno)),
    }
  }
}

impl std::fmt::Display for BtrfsUtilSysError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    self.strerror().unwrap().fmt(f)
  }
}

impl std::error::Error for BtrfsUtilSysError {}

/// A dynamic-loader error.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DlError {
  DlOpen(String),
  DlOpenUnknown,
  DlSym(String),
  DlSymUnknown,
  DlClose(String),
  DlCloseUnknown,
  IncompatibleSize,
  CreateCString,
  CreateCStringWithTrailing,
}

impl std::fmt::Display for DlError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self, f)
    }
}

impl std::error::Error for DlError {}


