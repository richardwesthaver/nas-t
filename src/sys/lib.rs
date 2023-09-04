//! sys/lib.rs --- NAS-T System Library

//! Code:
#[macro_use]
extern crate bitflags;

mod err;
pub use err::{SysError, BtrfsSysError, BtrfsUtilSysError, SysResult};
mod sv;
pub use sv::{Subvolume, SubvolumeIterator, SubvolumeInfo, SubvolumeIteratorFlags, SnapshotFlags};
use std::ffi::CStr;
use std::ffi::CString;
use std::ffi::OsString;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::ffi::OsStringExt;
use std::path::Path;
use std::path::PathBuf;

use libc::c_void;
use libc::free;

pub use btrfs_sys::{
  // constants
  BTRFS_FS_TREE_OBJECTID,
  BTRFS_IOCTL_MAGIC,
};

use btrfsutil_sys::{
  // qgroups
  btrfs_util_create_qgroup_inherit,
  btrfs_util_destroy_qgroup_inherit,     
  btrfs_util_qgroup_inherit,             
  btrfs_util_qgroup_inherit_add_group,   
  btrfs_util_qgroup_inherit_get_groups,  
  // sync
  btrfs_util_start_sync,
  btrfs_util_wait_sync,
};

pub const BTRFS_LIB_MAJOR: u32 = 0;
pub const BTRFS_LIB_MINOR: u32 = 1;
pub const BTRFS_LIB_PATCHLEVEL: u32 = 2;
pub const BTRFS_LIB_VERSION: u32 = 102;
pub const BTRFS_BUILD_VERSION: &[u8; 12] = b"Btrfs v6.3.3";

#[macro_export]
macro_rules! btrfs_util_wrap {
    ($unsafe_block: block) => {{
      let errcode: btrfsutil_sys::btrfs_util_error = unsafe { $unsafe_block };
        match errcode {
          btrfsutil_sys::BTRFS_UTIL_OK => SysResult::Ok(()),
          err => {
            #[allow(unused_imports)]
            use std::convert::TryFrom;
            let err = crate::BtrfsUtilSysError::try_from(err).unwrap();
            SysResult::Err(err.into())
          }
        }
    }};
}

/// Convert a Path into a CString safely.
#[inline]
pub(crate) fn path_to_cstr(path: &Path) -> CString {
    // unwrapping here is safe since on unix systems strings are natively held inside cstrings
    CString::new(path.as_os_str().as_bytes()).unwrap()
}

/// Convert a Path into a CString safely.
#[inline]
pub(crate) fn cstr_to_path(path: &CStr) -> PathBuf {
    PathBuf::from(OsString::from_vec(path.to_bytes().into()))
}

/// Qgroup inheritance specifier.
///
/// Wrapper around [btrfs_util_qgroup_inherit].
///
/// [btrfs_util_qgroup_inherit]: ../bindings/struct.btrfs_util_qgroup_inherit.html
#[derive(Clone, Debug)]
pub struct QgroupInherit(*mut btrfs_util_qgroup_inherit);

impl QgroupInherit {
    /// Create a quota group inheritance specifier.
    pub fn create() -> SysResult<Self> {
        let mut qgroup_ptr: *mut btrfs_util_qgroup_inherit = std::ptr::null_mut();

        btrfs_util_wrap!({btrfs_util_create_qgroup_inherit(0, &mut qgroup_ptr) })?;

        Ok(Self(qgroup_ptr))
    }

    /// Add inheritance from a qgroup to a qgroup inheritance specifier.
    pub fn add<U>(&mut self, qgroup_id: U) -> SysResult<()>
    where
        U: Into<u64>,
    {
        self.add_impl(qgroup_id.into())
    }

    fn add_impl(&mut self, qgroup_id: u64) -> SysResult<()> {
        let qgroup_ptr_initial: *mut btrfs_util_qgroup_inherit = self.as_ptr();
        let mut qgroup_ptr: *mut btrfs_util_qgroup_inherit = self.as_ptr();

        btrfs_util_wrap!({ btrfs_util_qgroup_inherit_add_group(&mut qgroup_ptr, qgroup_id) })?;

        if qgroup_ptr != qgroup_ptr_initial {
            self.0 = qgroup_ptr;
        }

        Ok(())
    }

    /// Get the qgroup ids contained by this inheritance specifier.
    pub fn get_groups(&self) -> SysResult<Vec<u64>> {
        let qgroup_ptr: *const btrfs_util_qgroup_inherit = self.as_ptr();
        let mut qgroup_ids_ptr: *const u64 = std::ptr::null();
        let mut qgroup_ids_count: usize = 0;

        unsafe {
            btrfs_util_qgroup_inherit_get_groups(
                qgroup_ptr,
                &mut qgroup_ids_ptr,
                &mut qgroup_ids_count,
            );
        }

        if qgroup_ids_count == 0 {
            return Ok(Vec::new());
        }

        let ids: Vec<u64> = {
            let slice = unsafe { std::slice::from_raw_parts(qgroup_ids_ptr, qgroup_ids_count) };
            let vec = slice.to_vec();
            unsafe { free(qgroup_ids_ptr as *mut c_void) };
            vec
        };
        Ok(ids)
    }

    #[inline]
    pub(crate) fn as_ptr(&self) -> *mut btrfs_util_qgroup_inherit {
        self.0
    }
}

impl Drop for QgroupInherit {
    fn drop(&mut self) {
        unsafe {
            btrfs_util_destroy_qgroup_inherit(self.0);
        }
    }
}

/// Start syncing on a btrfs filesystem.
pub fn sync<'a, P>(path: P) -> SysResult<()>
where
    P: Into<&'a Path>,
{
    sync_impl(path.into())
}

fn sync_impl(path: &Path) -> SysResult<()> {
    let path_cstr = path_to_cstr(path);
    let async_transid: u64 = {
        let mut async_transid: u64 = 0;
        btrfs_util_wrap!({ btrfs_util_start_sync(path_cstr.as_ptr(), &mut async_transid) })?;
        async_transid
    };
    btrfs_util_wrap!({ btrfs_util_wait_sync(path_cstr.as_ptr(), async_transid) })?;
    Ok(())
}

