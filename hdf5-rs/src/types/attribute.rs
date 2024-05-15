use crate::h5sys::*;
use crate::utils::*;
use crate::types::{DataSpace, DataSpaceOwner, DataType, DataTypeL, DataTypeOwner};
use crate::error::Error;

pub struct Attr {
    name: String,
    aid: i64,
}

pub trait AttributeFillable<T> {
    fn from_attribute(attribute: &Attr) -> Result<T, Error>;
}

impl Attr {
    pub fn open(group: i64, name: &str) -> Result<Self, Error> {
        let aid;
        unsafe {
            aid = H5Aopen(group, str_to_cchar!(name), H5P_DEFAULT);
            if aid <=0 {
                return Err(Error::attribute_open(name));
            }
        }
        
        Ok(Attr {
            name: name.to_string(),
            aid,
        })
    }

    pub fn get_aid(&self) -> i64 {
        self.aid
    }
}

impl Drop for Attr {
    fn drop(&mut self) {
        if self.aid > 0 {
            #[cfg(debug_assertions)] {
                println!("Closing attribute: {}", self.aid);
            }
            unsafe { H5Aclose(self.aid); }
        }
    }
}

impl std::fmt::Display for Attr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        writeln!(f, "H5Attribute")?;
        writeln!(f, "  name: {}", self.name)?;
        writeln!(f, "  aid: {}", self.aid)?;
        write!(f, "  value: ")?;
        if let Ok(datatype) = self.get_type() {
            match datatype.get_dtype() {
                DataTypeL::StringStatic => {
                    writeln!(f, "{}", String::from_attribute(self).unwrap())?;
                }
                DataTypeL::Signed64 => {
                    writeln!(f, "{}", i64::from_attribute(self).unwrap())?;
                }
                _ => { writeln!(f, "not yet implemented")?; }
            }
        } else {
            writeln!(f, "Failed to get attribute datatype")?;
        }
        Ok(())
    }
}

pub trait AttrOpener {
    fn open_attr(&self, name: &str) -> Result<Attr, Error>;
}

impl DataTypeOwner for Attr {
    fn get_type(&self) -> Result<DataType, Error> {
        DataType::parse(unsafe { H5Aget_type(self.aid) })
    }
}

impl DataSpaceOwner for Attr {
    fn get_space(&self) -> Result<DataSpace, Error> {
        DataSpace::parse(unsafe { H5Aget_space(self.get_aid()) })
    }
}

impl AttributeFillable<String> for String {
    fn from_attribute(attribute: &Attr) -> Result<String, Error> {
        let data_type = attribute.get_type()?;
        match data_type.get_dtype() {
            DataTypeL::StringStatic => {
                let mut data = vec![0; data_type.size()];
                unsafe {
                    H5Aread(
                        attribute.get_aid(),
                        data_type.get_tid(),
                        data
                        .as_mut_ptr()
                        .cast()
                    );
                    return if let Ok(s) = CStr::from_ptr(data.as_ptr().cast()).to_str() {
                        Ok(s.to_string())
                    } else {
                        Err(Error::attribute_fill_fail("String Static", "String"))
                    }
                }
            },
            DataTypeL::StringDynamic => {
                let mut str_ptr = 0usize;
                unsafe {
                    H5Aread(attribute.get_aid(),
                            data_type.get_tid(),
                            &mut str_ptr as *mut usize as *mut c_void);
                    if let Ok(string) = CStr::from_ptr(str_ptr as *const i8).to_str() {
                            Ok(string.to_string())
                    } else {
                        Err(Error::attribute_fill_fail("String Dynamic", "String"))
                    }
                }
            },
            _ => {
                        Err(Error::attribute_fill_not_available("String"))
            }
        }
    }
}

impl AttributeFillable<i64> for i64 {
    fn from_attribute(attribute: &Attr) -> Result<i64, Error> {
        let data_type = attribute.get_type()?;
        match data_type.get_dtype() {
            DataTypeL::Signed64 => {
                let mut data = 0i64;
                unsafe {
                    H5Aread(
                        attribute.get_aid(),
                        data_type.get_tid(),
                        &mut data as *mut i64 as *mut c_void
                    );
                    Ok(data)
                }
            },
            _ => {
                Err(Error::attribute_fill_not_available("i64"))
            }
        }
    }
}
