pub const BTRFS_LIB_MAJOR: u32 = 0;
pub const BTRFS_LIB_MINOR: u32 = 1;
pub const BTRFS_LIB_PATCHLEVEL: u32 = 2;
pub const BTRFS_LIB_VERSION: u32 = 102;
pub const BTRFS_BUILD_VERSION: &[u8; 12] = b"Btrfs v6.3.3";
pub use btrfs_sys::*;
pub use btrfsutil_sys::*;
