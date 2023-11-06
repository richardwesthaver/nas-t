//! src/sys/sv.rs --- subvolumes

//! Code:
use crate::{BtrfsUtilSysError, SysError};
use crate::SysResult;
use crate::{btrfs_util_wrap, path_to_cstr, cstr_to_path};
use crate::QgroupInherit;
use std::convert::TryFrom;
use std::ffi::CString;
use std::path::{Path, PathBuf};
use std::convert::TryInto;
use btrfsutil_sys::btrfs_util_subvolume_info;

use chrono::DateTime;
use chrono::Local;
use chrono::TimeZone;
use chrono::Timelike;

use uuid::Uuid;

use btrfsutil_sys::btrfs_util_create_subvolume_iterator;
use btrfsutil_sys::btrfs_util_destroy_subvolume_iterator;
use btrfsutil_sys::btrfs_util_subvolume_iterator;
use btrfsutil_sys::btrfs_util_subvolume_iterator_next;
use btrfsutil_sys::btrfs_util_create_snapshot;
use btrfsutil_sys::btrfs_util_create_subvolume;
use btrfsutil_sys::btrfs_util_delete_subvolume;
use btrfsutil_sys::btrfs_util_deleted_subvolumes;
use btrfsutil_sys::btrfs_util_get_default_subvolume;
use btrfsutil_sys::btrfs_util_get_subvolume_read_only;
use btrfsutil_sys::btrfs_util_is_subvolume;
use btrfsutil_sys::btrfs_util_set_default_subvolume;
use btrfsutil_sys::btrfs_util_set_subvolume_read_only;
use btrfsutil_sys::btrfs_util_subvolume_id;
use btrfsutil_sys::btrfs_util_subvolume_path;
use btrfsutil_sys::btrfs_util_wait_sync;
use libc::{c_void, free};

bitflags! {
    /// [Subvolume] delete flags.
    ///
    /// [Subvolume]:struct.Subvolume.html
    pub struct DeleteFlags: i32 {
        /// Recursive.
        const RECURSIVE = btrfsutil_sys::BTRFS_UTIL_DELETE_SUBVOLUME_RECURSIVE as i32;
    }
}
bitflags! {
    /// [Subvolume] snapshot flags.
    ///
    /// [Subvolume]:struct.Subvolume.html
    pub struct SnapshotFlags: i32 {
        /// Read-only.
        const READ_ONLY	= btrfsutil_sys::BTRFS_UTIL_CREATE_SNAPSHOT_READ_ONLY as i32;
        /// Recursive.
        const RECURSIVE = btrfsutil_sys::BTRFS_UTIL_CREATE_SNAPSHOT_RECURSIVE as i32;
    }
}

/// A Btrfs subvolume.
#[derive(Clone, Debug, PartialEq)]
pub struct Subvolume {
    id: u64,
    path: PathBuf,
}

impl Subvolume {
    /// Get a subvolume.
    ///
    /// The path must point to the root of a subvolume.
    pub fn get<'a, P>(path: P) -> SysResult<Self>
    where
        P: Into<&'a Path>,
    {
        Self::get_impl(path.into())
    }

    fn get_impl(path: &Path) -> SysResult<Self> {
        Self::is_subvolume(path)?;

        let path_cstr = path_to_cstr(path);
        let id: u64 = {
            let mut id: u64 = 0;
            btrfs_util_wrap!({ btrfs_util_subvolume_id(path_cstr.as_ptr(), &mut id) })?;
            id
        };

        Ok(Subvolume::new(id, path.into()))
    }

    /// Get a subvolume anyway.
    ///
    /// If the path is not the root of a subvolume, attempts to use btrfs_util_subvolume_path to
    /// get it, which requires **CAP_SYS_ADMIN**.
    ///
    /// ![Requires **CAP_SYS_ADMIN**](https://img.shields.io/static/v1?label=Requires&message=CAP_SYS_ADMIN&color=informational)
    pub fn get_anyway<'a, P>(path: P) -> SysResult<Self>
    where
        P: Into<&'a Path>,
    {
        Self::get_anyway_impl(path.into())
    }

    fn get_anyway_impl(path: &Path) -> SysResult<Self> {
        if let Ok(subvol) = Self::get_impl(path) {
            return Ok(subvol);
        }

        let path_cstr = path_to_cstr(path);
        let id: u64 = {
            let mut id: u64 = 0;
            btrfs_util_wrap!({ btrfs_util_subvolume_id(path_cstr.as_ptr(), &mut id) })?;
            id
        };

        let mut path_ret_ptr: *mut std::os::raw::c_char = std::ptr::null_mut();

        btrfs_util_wrap!({ btrfs_util_subvolume_path(path_cstr.as_ptr(), id, &mut path_ret_ptr) })?;

        let path_ret: CString = unsafe { CString::from_raw(path_ret_ptr) };

        Ok(Self::new(id, cstr_to_path(&path_ret)))
    }

    /// Create a new subvolume.
    pub fn create<'a, P, Q>(path: P, qgroup: Q) -> SysResult<Self>
    where
        P: Into<&'a Path>,
        Q: Into<Option<QgroupInherit>>,
    {
        Self::create_impl(path.into(), qgroup.into())
    }

    fn create_impl(path: &Path, qgroup: Option<QgroupInherit>) -> SysResult<Self> {
        let path_cstr = path_to_cstr(path);
        let qgroup_ptr = qgroup.map(|v| v.as_ptr()).unwrap_or(std::ptr::null_mut());

        let transid: u64 = {
            let mut transid: u64 = 0;
            btrfs_util_wrap!({
                btrfs_util_create_subvolume(path_cstr.as_ptr(), 0, &mut transid, qgroup_ptr)
            })?;
            transid
        };

        btrfs_util_wrap!({ btrfs_util_wait_sync(path_cstr.as_ptr(), transid) })?;

        Self::get(path)
    }

    /// Delete a subvolume.
    pub fn delete<D>(self, flags: D) -> SysResult<()>
    where
        D: Into<Option<DeleteFlags>>,
    {
        Self::delete_impl(self, flags.into())
    }

    fn delete_impl(self, flags: Option<DeleteFlags>) -> SysResult<()> {
        let path_cstr = path_to_cstr(&self.path);
        let flags_val = flags.map(|v| v.bits()).unwrap_or(0);

        btrfs_util_wrap!({ btrfs_util_delete_subvolume(path_cstr.as_ptr(), flags_val) })?;

        Ok(())
    }

    /// Get a list of subvolumes which have been deleted but not yet cleaned up.
    ///
    /// ![Requires **CAP_SYS_ADMIN**](https://img.shields.io/static/v1?label=Requires&message=CAP_SYS_ADMIN&color=informational)
    pub fn deleted<'a, F>(fs_root: F) -> SysResult<Vec<Self>>
    where
        F: Into<&'a Path>,
    {
        Self::deleted_impl(fs_root.into())
    }

    fn deleted_impl(fs_root: &Path) -> SysResult<Vec<Subvolume>> {
        // fixme 16/09/2020: you should probably just return the ids
        // since the subvolumes have been deleted, they should probably not have a path.

        let path_cstr = path_to_cstr(fs_root);
        let mut ids_ptr: *mut u64 = std::ptr::null_mut();
        let mut ids_count: usize = 0;

        btrfs_util_wrap!({
            btrfs_util_deleted_subvolumes(path_cstr.as_ptr(), &mut ids_ptr, &mut ids_count)
        })?;

        if ids_count == 0 {
            return Ok(Vec::new());
        }

        let subvolume_ids: Vec<u64> = unsafe {
            let slice = std::slice::from_raw_parts(ids_ptr, ids_count);
            let vec = slice.to_vec();
            free(ids_ptr as *mut c_void);
            vec
        };

        let subvolumes: Vec<Subvolume> = {
            let mut subvolumes: Vec<Subvolume> = Vec::with_capacity(ids_count);
            for id in subvolume_ids {
                subvolumes.push(Subvolume::try_from(id)?);
            }
            subvolumes
        };

        Ok(subvolumes)
    }

    /// Get the default subvolume.
    ///
    /// ![Requires **CAP_SYS_ADMIN**](https://img.shields.io/static/v1?label=Requires&message=CAP_SYS_ADMIN&color=informational)
    pub fn get_default<'a, P>(path: P) -> SysResult<Self>
    where
        P: Into<&'a Path>,
    {
        Self::get_default_impl(path.into())
    }

    fn get_default_impl(path: &Path) -> SysResult<Self> {
        let path_cstr = path_to_cstr(path);
        let mut id: u64 = 0;

        btrfs_util_wrap!({ btrfs_util_get_default_subvolume(path_cstr.as_ptr(), &mut id) })?;

        Ok(Subvolume::new(id, path.into()))
    }

    /// Set this subvolume as the default subvolume.
    ///
    /// ![Requires **CAP_SYS_ADMIN**](https://img.shields.io/static/v1?label=Requires&message=CAP_SYS_ADMIN&color=informational)
    pub fn set_default(&self) -> SysResult<()> {
        let path_cstr = path_to_cstr(&self.path);

        btrfs_util_wrap!({ btrfs_util_set_default_subvolume(path_cstr.as_ptr(), self.id) })?;

        Ok(())
    }

    /// Check whether this subvolume is read-only.
    pub fn is_ro(&self) -> SysResult<bool> {
        let path_cstr = path_to_cstr(&self.path);
        let ro: bool = {
            let mut ro = false;
            btrfs_util_wrap!({ btrfs_util_get_subvolume_read_only(path_cstr.as_ptr(), &mut ro) })?;
            ro
        };

        Ok(ro)
    }

    /// Set whether this subvolume is read-only or not.
    ///
    /// ![Requires **CAP_SYS_ADMIN**](https://img.shields.io/static/v1?label=Requires&message=CAP_SYS_ADMIN&color=informational)
    pub fn set_ro(&self, ro: bool) -> SysResult<()> {
        let path_cstr = path_to_cstr(&self.path);

        btrfs_util_wrap!({ btrfs_util_set_subvolume_read_only(path_cstr.as_ptr(), ro) })?;

        Ok(())
    }

    /// Check if a path is a Btrfs subvolume.
    ///
    /// Returns Ok if it is a subvolume or Err if otherwise.
    pub fn is_subvolume<'a, P>(path: P) -> SysResult<()>
    where
        P: Into<&'a Path>,
    {
        Self::is_subvolume_impl(path.into())
    }

    fn is_subvolume_impl(path: &Path) -> SysResult<()> {
        let path_cstr = path_to_cstr(path);

        btrfs_util_wrap!({ btrfs_util_is_subvolume(path_cstr.as_ptr()) })
    }

    /// Get information about this subvolume.
    pub fn info(&self) -> SysResult<SubvolumeInfo> {
        SubvolumeInfo::try_from(self)
    }

    /// Create a snapshot of this subvolume.
    pub fn snapshot<'a, P, F, Q>(&self, path: P, flags: F, qgroup: Q) -> SysResult<Self>
    where
        P: Into<&'a Path>,
        F: Into<Option<SnapshotFlags>>,
        Q: Into<Option<QgroupInherit>>,
    {
        self.snapshot_impl(path.into(), flags.into(), qgroup.into())
    }

    fn snapshot_impl(
        &self,
        path: &Path,
        flags: Option<SnapshotFlags>,
        qgroup: Option<QgroupInherit>,
    ) -> SysResult<Self> {
        let path_src_cstr = path_to_cstr(&self.path);
        let path_dest_cstr = path_to_cstr(path);
        let flags_val = flags.map(|v| v.bits()).unwrap_or(0);
        let qgroup_ptr = qgroup.map(|v| v.as_ptr()).unwrap_or(std::ptr::null_mut());

        let transid: u64 = {
            let mut transid: u64 = 0;
            btrfs_util_wrap!({
                btrfs_util_create_snapshot(
                    path_src_cstr.as_ptr(),
                    path_dest_cstr.as_ptr(),
                    flags_val,
                    &mut transid,
                    qgroup_ptr,
                )
            })?;
            transid
        };

        btrfs_util_wrap!({ btrfs_util_wait_sync(path_dest_cstr.as_ptr(), transid) })?;

        Self::get(path)
    }

    /// Get the id of this subvolume.
    #[inline]
    pub fn id(&self) -> u64 {
        self.id
    }

    /// Get the path of this subvolume.
    #[inline]
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Create a new subvolume from an id and a path.
    ///
    /// Restricted to the crate.
    #[inline]
    pub(crate) fn new(id: u64, path: PathBuf) -> Self {
        Self { id, path }
    }
}

impl From<&Subvolume> for u64 {
    /// Returns the id of the subvolume.
    #[inline]
    fn from(subvolume: &Subvolume) -> u64 {
        subvolume.id
    }
}

impl TryFrom<u64> for Subvolume {
  type Error = SysError;

    /// Attempts to get a subvolume from an id.
    ///
    /// This function will panic if it cannot retrieve the current working directory.
    ///
    /// ![Requires **CAP_SYS_ADMIN**](https://img.shields.io/static/v1?label=Requires&message=CAP_SYS_ADMIN&color=informational)
    fn try_from(src: u64) -> SysResult<Subvolume> {
        let path_cstr: CString = path_to_cstr(
            std::env::current_dir()
                .expect("Could not get the current working directory")
                .as_ref(),
        );
        let mut path_ret_ptr: *mut std::os::raw::c_char = std::ptr::null_mut();

        btrfs_util_wrap!({ btrfs_util_subvolume_path(path_cstr.as_ptr(), src, &mut path_ret_ptr) })?;

        let path_ret: CString = unsafe { CString::from_raw(path_ret_ptr) };

        Ok(Self::new(src, cstr_to_path(&path_ret)))
    }
}

impl From<&Subvolume> for PathBuf {
    /// Returns the path of the subvolume.
    #[inline]
    fn from(subvolume: &Subvolume) -> Self {
        subvolume.path.clone()
    }
}

impl<'lifetime> From<&'lifetime Subvolume> for &'lifetime Path {
    /// Returns the path of the subvolume.
    #[inline]
    fn from(subvolume: &'lifetime Subvolume) -> Self {
        subvolume.path.as_ref()
    }
}

impl TryFrom<&Path> for Subvolume {
  type Error = SysError;

    /// Attempts to get a subvolume from a path.
    #[inline]
    fn try_from(src: &Path) -> SysResult<Subvolume> {
        Subvolume::get_impl(src)
    }
}

impl TryFrom<PathBuf> for Subvolume {
    type Error = SysError;

    /// Attempts to get a subvolume from a path.
    #[inline]
    fn try_from(src: PathBuf) -> SysResult<Subvolume> {
        Subvolume::get_impl(src.as_ref())
    }
}

bitflags! {
    /// Subvolume iterator options
    pub struct SubvolumeIteratorFlags: i32 {
        /// Post order
        const POST_ORDER = btrfsutil_sys::BTRFS_UTIL_SUBVOLUME_ITERATOR_POST_ORDER as i32;
    }
}

/// A subvolume iterator.
pub struct SubvolumeIterator(*mut btrfs_util_subvolume_iterator);

impl SubvolumeIterator {
    /// Create a new subvolume iterator.
    pub fn new<'a, P, F>(path: P, flags: F) -> SysResult<Self>
    where
        P: Into<&'a Path>,
        F: Into<Option<SubvolumeIteratorFlags>>,
    {
        Self::new_impl(path.into(), flags.into())
    }

    fn new_impl(path: &Path, flags: Option<SubvolumeIteratorFlags>) -> SysResult<Self> {
        let path_cstr = path_to_cstr(path);
        let flags_val = if let Some(val) = flags { val.bits() } else { 0 };

        let raw_iterator_ptr: *mut btrfs_util_subvolume_iterator = {
            let mut raw_iterator_ptr: *mut btrfs_util_subvolume_iterator = std::ptr::null_mut();
            btrfs_util_wrap!({
                btrfs_util_create_subvolume_iterator(
                    path_cstr.as_ptr(),
                    0, // read below
                    flags_val,
                    &mut raw_iterator_ptr,
                )
            })?;
            // using 0 instead of an id is intentional
            // https://github.com/kdave/btrfs-progs/blob/11acf45eea6dd81e891564967051e2bb10bd25f7/libbtrfsutil/subvolume.c#L971
            // if we specify an id then libbtrfsutil will use elevated privileges to search for
            // subvolumes
            // if we don't, then it will use elevated privileges only if the current user is root
            raw_iterator_ptr
        };

        Ok(Self(raw_iterator_ptr))
    }
}

impl Iterator for SubvolumeIterator {
    type Item = SysResult<Subvolume>;

    fn next(&mut self) -> Option<SysResult<Subvolume>> {
        let mut cstr_ptr: *mut std::os::raw::c_char = std::ptr::null_mut();
        let mut id: u64 = 0;

      if let Err(e) =
        btrfs_util_wrap!({ btrfs_util_subvolume_iterator_next(self.0, &mut cstr_ptr, &mut id) })
      {
        if e == SysError::BtrfsUtil(BtrfsUtilSysError::StopIteration) {
          None
        } else {
          Err(e).into()
        }
      } else if !cstr_ptr.is_null() {
        let path = cstr_to_path(unsafe { CString::from_raw(cstr_ptr).as_ref() });
        Subvolume::get(path.as_path()).into()
      } else if id != 0 {
        Subvolume::try_from(id).into()
      } else {
        panic!("subvolume iterator returned both a null path")
      }
    }
}

impl Drop for SubvolumeIterator {
    fn drop(&mut self) {
        unsafe {
            btrfs_util_destroy_subvolume_iterator(self.0);
        }
    }
}

impl TryFrom<&Subvolume> for SubvolumeIterator {
    type Error = SysError;

    /// Same as SubvolumeIterator::new with no flags.
    #[inline]
    fn try_from(src: &Subvolume) -> SysResult<SubvolumeIterator> {
        SubvolumeIterator::new_impl(src.path(), None)
    }
}

impl TryInto<Vec<Subvolume>> for SubvolumeIterator {
    type Error = SysError;

    /// Same as SubvolumeIterator.`collect::<Result<Vec<Subvolume>>>`.
    #[inline]
    fn try_into(self) -> SysResult<Vec<Subvolume>> {
        self.collect::<SysResult<Vec<Subvolume>>>()
    }
}

/// Information about a Btrfs subvolume.
///
/// Contains everything from [btrfs_util_subvolume_info] plus the path of the subvolume.
///
/// [btrfs_util_subvolume_info]: https://docs.rs/btrfsutil-sys/1.2.1/btrfsutil_sys/struct.btrfs_util_subvolume_info.html
#[derive(Clone, Debug, PartialEq)]
pub struct SubvolumeInfo {
    /// ID of this subvolume, unique across the filesystem.
    pub id: u64,
    /// The path of the subvolume.
    pub path: PathBuf,
    /// ID of the subvolume which contains this subvolume, or zero for the root subvolume
    /// ([BTRFS_FS_TREE_OBJECTID]) or orphaned subvolumes (i.e., subvolumes which have been
    /// deleted but not yet cleaned up).
    ///
    /// [BTRFS_FS_TREE_OBJECTID]: https://github.com/kdave/btrfs-progs/blob/471b4cf7e3a46222531a895f90228ea164b1b857/libbtrfsutil/btrfs_tree.h#L34
    pub parent_id: Option<u64>,
    /// Inode number of the directory containing this subvolume in the parent subvolume, or zero
    /// for the root subvolume ([BTRFS_FS_TREE_OBJECTID]) or orphaned subvolumes.
    ///
    /// [BTRFS_FS_TREE_OBJECTID]: https://github.com/kdave/btrfs-progs/blob/471b4cf7e3a46222531a895f90228ea164b1b857/libbtrfsutil/btrfs_tree.h#L34
    pub dir_id: Option<u64>,
    /// On-disk root item flags.
    pub flags: u64,
    /// UUID of this subvolume.
    pub uuid: Uuid,
    /// UUID of the subvolume this subvolume is a snapshot of, or all zeroes if this subvolume is
    /// not a snapshot.
    pub parent_uuid: Option<Uuid>,
    /// UUID of the subvolume this subvolume was received from, or all zeroes if this subvolume was
    /// not received. Note that this field, [stransid](#structfield.stransid),
    /// [rtransid](#structfield.rtransid), [stime](#structfield.stime), and
    /// [rtime](#structfield.rtime) are set manually by userspace after a subvolume is received.
    pub received_uuid: Option<Uuid>,
    /// Transaction ID of the subvolume root.
    pub generation: u64,
    /// Transaction ID when an inode in this subvolume was last changed.
    pub ctransid: u64,
    /// Transaction ID when this subvolume was created.
    pub otransid: u64,
    /// Transaction ID of the sent subvolume this subvolume was received from, or zero if this
    /// subvolume was not received. See the note on [received_uuid](#structfield.received_uuid).
    pub stransid: Option<u64>,
    /// Transaction ID when this subvolume was received, or zero if this subvolume was not
    /// received. See the note on [received_uuid](#structfield.received_uuid).
    pub rtransid: Option<u64>,
    /// Time when an inode in this subvolume was last changed.
    pub ctime: DateTime<Local>,
    /// Time when this subvolume was created.
    pub otime: DateTime<Local>,
    /// Not well-defined, usually zero unless it was set otherwise. See the note on
    /// [received_uuid](#structfield.received_uuid).
    pub stime: Option<DateTime<Local>>,
    /// Time when this subvolume was received, or zero if this subvolume was not received. See the
    /// [received_uuid](#structfield.received_uuid).
    pub rtime: Option<DateTime<Local>>,
}

impl From<&SubvolumeInfo> for Subvolume {
    fn from(info: &SubvolumeInfo) -> Self {
        Self::new(info.id, info.path.clone())
    }
}

impl TryFrom<&Subvolume> for SubvolumeInfo {
    type Error = SysError;

    fn try_from(src: &Subvolume) -> SysResult<Self> {
        let path_cstr = crate::path_to_cstr(src.path());
        let btrfs_subvolume_info_ptr: *mut btrfs_util_subvolume_info =
            Box::into_raw(Box::from(btrfs_util_subvolume_info {
                id: 0,
                parent_id: 0,
                dir_id: 0,
                flags: 0,
                uuid: [0; 16],
                parent_uuid: [0; 16],
                received_uuid: [0; 16],
                generation: 0,
                ctransid: 0,
                otransid: 0,
                stransid: 0,
                rtransid: 0,
                ctime: btrfsutil_sys::timespec {
                    tv_nsec: 0 as btrfsutil_sys::__time_t,
                    tv_sec: 0 as btrfsutil_sys::__syscall_slong_t,
                },
                otime: btrfsutil_sys::timespec {
                    tv_nsec: 0 as btrfsutil_sys::__time_t,
                    tv_sec: 0 as btrfsutil_sys::__syscall_slong_t,
                },
                stime: btrfsutil_sys::timespec {
                    tv_nsec: 0 as btrfsutil_sys::__time_t,
                    tv_sec: 0 as btrfsutil_sys::__syscall_slong_t,
                },
                rtime: btrfsutil_sys::timespec {
                    tv_nsec: 0 as btrfsutil_sys::__time_t,
                    tv_sec: 0 as btrfsutil_sys::__syscall_slong_t,
                },
            }));

        btrfs_util_wrap!({
            btrfs_util_subvolume_info(path_cstr.as_ptr(), src.id(), btrfs_subvolume_info_ptr)
        })?;

        let info: Box<btrfs_util_subvolume_info> =
            unsafe { Box::from_raw(btrfs_subvolume_info_ptr) };

        // process the retrieved info struct
        let uuid: Uuid = Uuid::from_slice(&info.uuid).expect("Failed to get uuid from C");
        let parent_uuid_val: Uuid =
            Uuid::from_slice(&info.parent_uuid).expect("Failed to get parent uuid from C");
        let received_uuid_val: Uuid =
            Uuid::from_slice(&info.received_uuid).expect("Failed to get received uuid from C");
        let ctime: DateTime<Local> = Local
            .timestamp_opt(info.ctime.tv_sec, info.ctime.tv_nsec as u32)
            .single()
            .expect("Failed to generate timestamp from C");
        let otime: DateTime<Local> = Local
            .timestamp_opt(info.otime.tv_sec, info.otime.tv_nsec as u32)
            .single()
            .expect("Failed to generate timestamp from C");
        let stime_val: DateTime<Local> = Local
            .timestamp_opt(info.stime.tv_sec, info.stime.tv_nsec as u32)
            .single()
            .expect("Failed to generate timestamp from C");
        let rtime_val: DateTime<Local> = Local
            .timestamp_opt(info.rtime.tv_sec, info.rtime.tv_nsec as u32)
            .single()
            .expect("Failed to generate timestamp from C");
        let parent_id: Option<u64> = if info.parent_id == 0 {
            None
        } else {
            Some(info.parent_id)
        };
        let dir_id: Option<u64> = if info.dir_id == 0 {
            None
        } else {
            Some(info.dir_id)
        };
        let parent_uuid: Option<Uuid> = if parent_uuid_val.is_nil() {
            None
        } else {
            Some(parent_uuid_val)
        };
        let received_uuid: Option<Uuid> = if received_uuid_val.is_nil() {
            None
        } else {
            Some(received_uuid_val)
        };
        let stransid: Option<u64> = if info.stransid == 0 {
            None
        } else {
            Some(info.stransid)
        };
        let rtransid: Option<u64> = if info.rtransid == 0 {
            None
        } else {
            Some(info.rtransid)
        };
        let stime: Option<DateTime<Local>> =
            if stime_val.nanosecond() == 0 && stime_val.second() == 0 {
                None
            } else {
                Some(stime_val)
            };
        let rtime: Option<DateTime<Local>> =
            if rtime_val.nanosecond() == 0 && rtime_val.second() == 0 {
                None
            } else {
                Some(rtime_val)
            };

        Ok(Self {
            id: info.id,
            path: src.path().to_path_buf(),
            parent_id,
            dir_id,
            flags: info.flags,
            uuid,
            parent_uuid,
            received_uuid,
            generation: info.generation,
            ctransid: info.ctransid,
            otransid: info.otransid,
            stransid,
            rtransid,
            ctime,
            otime,
            stime,
            rtime,
        })
    }
}
