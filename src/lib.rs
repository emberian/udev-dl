#![allow(bad_style)]

extern crate libc;
extern crate dylib;

use dylib::*;
use std::path::Path;
use libc::{c_void, c_int, c_char, size_t, c_ulonglong, c_uint, dev_t};
use std::error::Error;
use std::fmt::{
  Display,
  Formatter,
};


pub enum udev { }
pub enum udev_list_entry { }
pub enum udev_device { }
pub enum udev_monitor { }
pub enum udev_enumerate { }
pub enum udev_queue { }
pub enum udev_hwdb { }


// stolen from https://github.com/Daggerbot/x11-rs/blob/master/x11-dl/src/link.rs
macro_rules! udev_link {
  { $struct_name:ident, [$($lib_name:expr),*],
    $(pub fn $fn_name:ident ($($param_name:ident : $param_type:ty),*) -> $ret_type:ty,)*
  } => {
    pub struct $struct_name {
      #[allow(dead_code)]
      lib: ::dylib::DynamicLibrary,
      $(pub $fn_name: unsafe extern "C" fn ($($param_type),*) -> $ret_type,)*
    }

    impl $struct_name {
      pub fn open () -> Result<$struct_name, OpenError> {
        unsafe {
          let lib = try!(open_lib(&[$($lib_name),*]));
          $(let $fn_name = try!(get_sym(&lib, stringify!($fn_name)));)*
          return Ok($struct_name {
            lib: lib,
            $($fn_name: ::std::mem::transmute($fn_name),)*
          });
        }
      }
    }
  };

  { $struct_name:ident, [$($lib_name:expr),*],
    $(pub fn $fn_name:ident ($($param_name:ident : $param_type:ty),*) -> $ret_type:ty,)*
    variadic:
    $(pub fn $vfn_name:ident ($($vparam_name: ident : $vparam_type:ty),+) -> $vret_type:ty,)*
  } => {
    pub struct $struct_name {
      #[allow(dead_code)]
      lib: ::dylib::DynamicLibrary,
      $(pub $fn_name: unsafe extern "C" fn ($($param_type),*) -> $ret_type,)*
      $(pub $vfn_name: unsafe extern "C" fn ($($vparam_type),+, ...) -> $vret_type,)*
    }

    impl $struct_name {
      pub fn open () -> Result<$struct_name, OpenError> {
        unsafe {
          let lib = try!(open_lib(&[$($lib_name),*]));
          $(let $fn_name = try!(get_sym(&lib, stringify!($fn_name)));)*
          $(let $vfn_name = try!(get_sym(&lib, stringify!($vfn_name)));)*
          return Ok($struct_name {
            lib: lib,
            $($fn_name: ::std::mem::transmute($fn_name),)*
            $($vfn_name: ::std::mem::transmute($vfn_name),)*
          });
        }
      }
    }
  };
}


pub fn get_sym (lib: &DynamicLibrary, name: &str) -> Result<*mut (), OpenError> {
  unsafe {
    match lib.symbol(name) {
      Ok(sym) => Ok(sym),
      Err(msg) => Err(OpenError::new(OpenErrorKind::Symbol, msg)),
    }
  }
}

pub fn open_lib (names: &[&'static str]) -> Result<DynamicLibrary, OpenError> {
  assert!(!names.is_empty());
  let mut msgs = Vec::new();

  for name in names.iter() {
    match DynamicLibrary::open(Some(Path::new(*name))) {
      Ok(lib) => { return Ok(lib); },
      Err(err) => { msgs.push(err); },
    }
  }

  let mut detail = String::new();
  for i in 0..msgs.len() {
    if i != 0 {
      detail.push_str(", ");
    }
    detail.push_str("\"");
    detail.push_str(msgs[i].as_ref());
    detail.push_str("\"");
  }

  return Err(OpenError::new(OpenErrorKind::Library, detail));
}



//
// OpenError
//


#[derive(Clone, Debug)]
pub struct OpenError {
  kind: OpenErrorKind,
  detail: String,
}

impl OpenError {
  pub fn detail (&self) -> &str {
    self.detail.as_ref()
  }

  pub fn new (kind: OpenErrorKind, detail: String) -> OpenError {
    OpenError {
      kind: kind,
      detail: detail,
    }
  }
}

impl Display for OpenError {
  fn fmt (&self, f: &mut Formatter) -> Result<(), ::std::fmt::Error> {
    try!(f.write_str(self.kind.as_str()));
    if !self.detail.is_empty() {
      try!(f.write_str(" ("));
      try!(f.write_str(self.detail.as_ref()));
      try!(f.write_str(")"));
    }
    return Ok(());
  }
}

impl Error for OpenError {
  fn description (&self) -> &str {
    self.kind.as_str()
  }
}


//
// OpenErrorKind
//


#[derive(Clone, Copy, Debug)]
pub enum OpenErrorKind {
  Library,
  Symbol,
}

impl OpenErrorKind {
  pub fn as_str (self) -> &'static str {
    match self {
      OpenErrorKind::Library => "opening library failed",
      OpenErrorKind::Symbol => "loading symbol failed",
    }
  }
}

udev_link! { Udev, ["libudev.so", "libudev.so.1"],
    pub fn udev_ref(udev: *mut udev) -> *mut udev,
    pub fn udev_unref(udev: *mut udev) -> *mut udev,
    pub fn udev_new() -> *mut udev,
    //pub fn udev_set_log_fn(udev: *mut udev, log_fn: ::std::option::Option<extern "C" fn(udev: *mut udev, priority: c_int, file: *const c_char, line: c_int, _fn: *const c_char, format: *const c_char, args: va_list) -> ()>),
    pub fn udev_get_log_priority(udev: *mut udev) -> c_int,
    pub fn udev_set_log_priority(udev: *mut udev, priority: c_int) -> (),
    pub fn udev_get_userdata(udev: *mut udev) -> *mut c_void,
    pub fn udev_set_userdata(udev: *mut udev, userdata: *mut c_void) -> (),
    pub fn udev_list_entry_get_next(list_entry: *mut udev_list_entry) -> *mut udev_list_entry,
    pub fn udev_list_entry_get_by_name(list_entry: *mut udev_list_entry, name: *const c_char) -> *mut udev_list_entry,
    pub fn udev_list_entry_get_name(list_entry: *mut udev_list_entry) -> *const c_char,
    pub fn udev_list_entry_get_value(list_entry: *mut udev_list_entry) -> *const c_char,
    pub fn udev_device_ref(udev_device: *mut udev_device) -> *mut udev_device,
    pub fn udev_device_unref(udev_device: *mut udev_device) -> *mut udev_device,
    pub fn udev_device_get_udev(udev_device: *mut udev_device) -> *mut udev,
    pub fn udev_device_new_from_syspath(udev: *mut udev, syspath: *const c_char) -> *mut udev_device,
    pub fn udev_device_new_from_devnum(udev: *mut udev, _type: c_char, devnum: dev_t) -> *mut udev_device,
    pub fn udev_device_new_from_subsystem_sysname(udev: *mut udev, subsystem: *const c_char, sysname: *const c_char) -> *mut udev_device,
    pub fn udev_device_new_from_device_id(udev: *mut udev, id: *const c_char) -> *mut udev_device,
    pub fn udev_device_new_from_environment(udev: *mut udev) -> *mut udev_device,
    pub fn udev_device_get_parent(udev_device: *mut udev_device) -> *mut udev_device,
    pub fn udev_device_get_parent_with_subsystem_devtype(udev_device: *mut udev_device, subsystem: *const c_char, devtype: *const c_char) -> *mut udev_device,
    pub fn udev_device_get_devpath(udev_device: *mut udev_device) -> *const c_char,
    pub fn udev_device_get_subsystem(udev_device: *mut udev_device) -> *const c_char,
    pub fn udev_device_get_devtype(udev_device: *mut udev_device) -> *const c_char,
    pub fn udev_device_get_syspath(udev_device: *mut udev_device) -> *const c_char,
    pub fn udev_device_get_sysname(udev_device: *mut udev_device) -> *const c_char,
    pub fn udev_device_get_sysnum(udev_device: *mut udev_device) -> *const c_char,
    pub fn udev_device_get_devnode(udev_device: *mut udev_device) -> *const c_char,
    pub fn udev_device_get_is_initialized(udev_device: *mut udev_device) -> c_int,
    pub fn udev_device_get_devlinks_list_entry(udev_device: *mut udev_device) -> *mut udev_list_entry,
    pub fn udev_device_get_properties_list_entry(udev_device: *mut udev_device) -> *mut udev_list_entry,
    pub fn udev_device_get_tags_list_entry(udev_device: *mut udev_device) -> *mut udev_list_entry,
    pub fn udev_device_get_sysattr_list_entry(udev_device: *mut udev_device) -> *mut udev_list_entry,
    pub fn udev_device_get_property_value(udev_device: *mut udev_device, key: *const c_char) -> *const c_char,
    pub fn udev_device_get_driver(udev_device: *mut udev_device) -> *const c_char,
    pub fn udev_device_get_devnum(udev_device: *mut udev_device) -> dev_t,
    pub fn udev_device_get_action(udev_device: *mut udev_device) -> *const c_char,
    pub fn udev_device_get_seqnum(udev_device: *mut udev_device) -> c_ulonglong,
    pub fn udev_device_get_usec_since_initialized(udev_device: *mut udev_device) -> c_ulonglong,
    pub fn udev_device_get_sysattr_value(udev_device: *mut udev_device, sysattr: *const c_char) -> *const c_char,
    pub fn udev_device_set_sysattr_value(udev_device: *mut udev_device, sysattr: *const c_char, value: *mut c_char) -> c_int,
    pub fn udev_device_has_tag(udev_device: *mut udev_device, tag: *const c_char) -> c_int,
    pub fn udev_monitor_ref(udev_monitor: *mut udev_monitor) -> *mut udev_monitor,
    pub fn udev_monitor_unref(udev_monitor: *mut udev_monitor) -> *mut udev_monitor,
    pub fn udev_monitor_get_udev(udev_monitor: *mut udev_monitor) -> *mut udev,
    pub fn udev_monitor_new_from_netlink(udev: *mut udev, name: *const c_char) -> *mut udev_monitor,
    pub fn udev_monitor_enable_receiving(udev_monitor: *mut udev_monitor) -> c_int,
    pub fn udev_monitor_set_receive_buffer_size(udev_monitor: *mut udev_monitor, size: c_int) -> c_int,
    pub fn udev_monitor_get_fd(udev_monitor: *mut udev_monitor) -> c_int,
    pub fn udev_monitor_receive_device(udev_monitor: *mut udev_monitor) -> *mut udev_device,
    pub fn udev_monitor_filter_add_match_subsystem_devtype(udev_monitor: *mut udev_monitor, subsystem: *const c_char, devtype: *const c_char) -> c_int,
    pub fn udev_monitor_filter_add_match_tag(udev_monitor: *mut udev_monitor, tag: *const c_char) -> c_int,
    pub fn udev_monitor_filter_update(udev_monitor: *mut udev_monitor) -> c_int,
    pub fn udev_monitor_filter_remove(udev_monitor: *mut udev_monitor) -> c_int,
    pub fn udev_enumerate_ref(udev_enumerate: *mut udev_enumerate) -> *mut udev_enumerate,
    pub fn udev_enumerate_unref(udev_enumerate: *mut udev_enumerate) -> *mut udev_enumerate,
    pub fn udev_enumerate_get_udev(udev_enumerate: *mut udev_enumerate) -> *mut udev,
    pub fn udev_enumerate_new(udev: *mut udev) -> *mut udev_enumerate,
    pub fn udev_enumerate_add_match_subsystem(udev_enumerate: *mut udev_enumerate, subsystem: *const c_char) -> c_int,
    pub fn udev_enumerate_add_nomatch_subsystem(udev_enumerate: *mut udev_enumerate, subsystem: *const c_char) -> c_int,
    pub fn udev_enumerate_add_match_sysattr(udev_enumerate: *mut udev_enumerate, sysattr: *const c_char, value: *const c_char) -> c_int,
    pub fn udev_enumerate_add_nomatch_sysattr(udev_enumerate: *mut udev_enumerate, sysattr: *const c_char, value: *const c_char) -> c_int,
    pub fn udev_enumerate_add_match_property(udev_enumerate: *mut udev_enumerate, property: *const c_char, value: *const c_char) -> c_int,
    pub fn udev_enumerate_add_match_sysname(udev_enumerate: *mut udev_enumerate, sysname: *const c_char) -> c_int,
    pub fn udev_enumerate_add_match_tag(udev_enumerate: *mut udev_enumerate, tag: *const c_char) -> c_int,
    pub fn udev_enumerate_add_match_parent(udev_enumerate: *mut udev_enumerate, parent: *mut udev_device) -> c_int,
    pub fn udev_enumerate_add_match_is_initialized(udev_enumerate: *mut udev_enumerate) -> c_int,
    pub fn udev_enumerate_add_syspath(udev_enumerate: *mut udev_enumerate, syspath: *const c_char) -> c_int,
    pub fn udev_enumerate_scan_devices(udev_enumerate: *mut udev_enumerate) -> c_int,
    pub fn udev_enumerate_scan_subsystems(udev_enumerate: *mut udev_enumerate) -> c_int,
    pub fn udev_enumerate_get_list_entry(udev_enumerate: *mut udev_enumerate) -> *mut udev_list_entry,
    pub fn udev_queue_ref(udev_queue: *mut udev_queue) -> *mut udev_queue,
    pub fn udev_queue_unref(udev_queue: *mut udev_queue) -> *mut udev_queue,
    pub fn udev_queue_get_udev(udev_queue: *mut udev_queue) -> *mut udev,
    pub fn udev_queue_new(udev: *mut udev) -> *mut udev_queue,
    pub fn udev_queue_get_kernel_seqnum(udev_queue: *mut udev_queue) -> c_ulonglong,
    pub fn udev_queue_get_udev_seqnum(udev_queue: *mut udev_queue) -> c_ulonglong,
    pub fn udev_queue_get_udev_is_active(udev_queue: *mut udev_queue) -> c_int,
    pub fn udev_queue_get_queue_is_empty(udev_queue: *mut udev_queue) -> c_int,
    pub fn udev_queue_get_seqnum_is_finished(udev_queue: *mut udev_queue, seqnum: c_ulonglong) -> c_int,
    pub fn udev_queue_get_seqnum_sequence_is_finished(udev_queue: *mut udev_queue, start: c_ulonglong, end: c_ulonglong) -> c_int,
    pub fn udev_queue_get_fd(udev_queue: *mut udev_queue) -> c_int,
    pub fn udev_queue_flush(udev_queue: *mut udev_queue) -> c_int,
    pub fn udev_queue_get_queued_list_entry(udev_queue: *mut udev_queue) -> *mut udev_list_entry,
    pub fn udev_hwdb_new(udev: *mut udev) -> *mut udev_hwdb,
    pub fn udev_hwdb_ref(hwdb: *mut udev_hwdb) -> *mut udev_hwdb,
    pub fn udev_hwdb_unref(hwdb: *mut udev_hwdb) -> *mut udev_hwdb,
    pub fn udev_hwdb_get_properties_list_entry(hwdb: *mut udev_hwdb, modalias: *const c_char, flags: c_uint) -> *mut udev_list_entry,
    pub fn udev_util_encode_string(str: *const c_char, str_enc: *mut c_char, len: size_t) -> c_int,
}
