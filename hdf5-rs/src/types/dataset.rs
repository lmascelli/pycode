use crate::h5sys::*;
use crate::types::{DataSpace, DataSpaceOwner, DataType, DataTypeL, DataTypeOwner};
use crate::{cchar_to_string, str_to_cchar};
use std::any::type_name;

pub struct Dataset {
    did: i64,
    path: String,
    dataspace: Option<DataSpace>,
    datatype: Option<DataType>,
}

pub trait DatasetFillable<T> {
    fn from_dataset(_dataset: &Dataset) -> Result<Vec<T>, String> {
        Err(format!(
            "Dataset::from_dataset: cannot retrieve the whole dataset from type {}",
            type_name::<T>()
        ))
    }

    fn from_dataset_subspace(
        _dataset: &Dataset,
        _start: &[usize],
        _offset: &[usize],
    ) -> Result<Vec<Vec<T>>, String> {
        Err(format!(
            "Dataset::from_dataset_subspace: cannot retrieve a subspace from type {}",
            type_name::<T>()
        ))
    }

    fn from_dataset_row(_dataset: &Dataset, _row: usize) -> Result<Vec<T>, String> {
        Err(format!(
            "Dataset::from_dataset_row: cannot retrieve a subspace from type {}",
            type_name::<T>()
        ))
    }
}

pub trait DatasetOwner {
    fn get_dataset(&self, name: &str) -> Result<Dataset, String>;
}

impl Dataset {
    pub fn get_path(&self) -> String {
        self.path.clone()
    }

    pub fn get_did(&self) -> i64 {
        self.did
    }

    pub fn get_dataspace(&self) -> Result<&DataSpace, String> {
        if let Some(ref dataspace) = self.dataspace {
            Ok(dataspace)
        } else {
            Err(format!(
                "Dataset::get_dataspace: {} dataset does not contain a dataspace",
                self.get_path()
            ))
        }
    }

    pub fn get_datatype(&self) -> Option<&DataType> {
        if let Some(ref datatype) = self.datatype {
            Some(datatype)
        } else {
            None
        }
    }

    #[allow(unused_unsafe)]
    pub fn open(parent: i64, name: &str) -> Result<Self, String> {
        let did;
        let path;
        let dataspace;
        let datatype;
        unsafe {
            did = H5Dopen2(parent, str_to_cchar!(name), H5P_DEFAULT);
            if did <= 0 {
                return Err(format!("Failed to open the dataspace {}", name));
            }

            let path_len = H5Iget_name(did, null_mut(), 0) as usize;
            let buffer = vec![0usize; path_len + 1];
            H5Iget_name(did, buffer.as_ptr() as _, buffer.len());
            path = cchar_to_string!(buffer.as_ptr().cast());
        }
        let mut ret = Dataset {
            did,
            path,
            dataspace: None,
            datatype: None,
        };
        dataspace = ret.get_space()?;
        datatype = ret.get_type()?;
        ret.dataspace = Some(dataspace);
        ret.datatype = Some(datatype);

        Ok(ret)
    }

    pub fn fill_memory<T>(&self, memory_datatype: i64, data: &mut Vec<T>) -> Result<(), String> {
        let res;
        unsafe {
            res = H5Dread(
                self.did,
                memory_datatype,
                H5S_ALL,
                H5S_ALL,
                H5P_DEFAULT,
                data.as_ptr() as *mut T as *mut c_void,
            );
        }
        if res >= 0 {
            Ok(())
        } else {
            Err(format!(
                "Dataset::fill_memory: failed to fill memory from dataset {}",
                self.get_path()
            ))
        }
    }
}

impl std::fmt::Display for Dataset {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        writeln!(f, "H5Dataset")?;
        writeln!(f, "  path: {}", self.path)?;
        writeln!(f, "  did {}", self.did)?;
        writeln!(
            f,
            "  dataspace:\n {}",
            if let Some(ref d) = self.dataspace {
                format!("{d}").to_string()
            } else {
                "None".to_string()
            }
        )?;
        writeln!(
            f,
            "  datatype:\n {}",
            if let Some(ref d) = self.datatype {
                format!("{d}").to_string()
            } else {
                "None".to_string()
            }
        )?;
        Ok(())
    }
}

impl Drop for Dataset {
    fn drop(&mut self) {
        if self.did > 0 {
            #[cfg(debug_assertions)]
            {
                println!("Closing dataset {}", self.path);
            }
            unsafe {
                H5Dclose(self.did);
            }
        }
    }
}

impl DataSpaceOwner for Dataset {
    fn get_space(&self) -> Result<DataSpace, String> {
        let did = unsafe { H5Dget_space(self.did) };
        if did <= 0 {
            Err(format!(
                "Dataset::get_space: Failed to retrieve the DataSpace for {} dataset",
                self.path
            ))
        } else {
            DataSpace::parse(did)
        }
    }
}

impl DataTypeOwner for Dataset {
    fn get_type(&self) -> Result<DataType, String> {
        let did = unsafe { H5Dget_type(self.did) };
        if did <= 0 {
            Err(format!(
                "Dataset::get_space: Failed to retrieve the DataType for {} dataset",
                self.path
            ))
        } else {
            Ok(DataType::parse(did))
        }
    }
}

impl DatasetFillable<i32> for i32 {
    fn from_dataset_row(dataset: &Dataset, row: usize) -> Result<Vec<i32>, String> {
        let mut ret = vec![];
        match dataset
            .get_datatype()
            .ok_or(format!(
                "Dataset::from_dataset_row: the dataset has no DataType associated {}",
                dataset.get_path()
            ))?
            .get_dtype()
        {
            DataTypeL::Signed32 => {
                if let Ok(dataspace) = dataset.get_dataspace() {
                    let dims = dataspace.get_dims();

                    // set subspace
                    let memory_dataspace = dataspace.select_row(row)?;

                    // create memory dataspace

                    ret.resize(dims[1], 0);

                    // read the data
                    unsafe {
                        H5Dread(
                            dataset.get_did(),
                            H5T_NATIVE_INT_g,
                            memory_dataspace.get_did(),
                            dataspace.get_did(),
                            H5P_DEFAULT,
                            ret.as_ptr() as _,
                        )
                    };

                    // reset original space
                    dataspace.reset_selection();
                } else {
                    return Err(format!(
                        "Dataset::from_dataset_row: dataset {} has no associated dataspace",
                        dataset.get_path()
                    ));
                }
            }
            DataTypeL::Unsigned32 => {
                todo!()
            }
            DataTypeL::Signed64 => {
                todo!()
            }
            DataTypeL::Unsigned64 => {
                todo!()
            }
            _ => {
                return Err(format!(
                    "Dataset::from_dataset: cannot read i32 from dataset {}",
                    dataset.get_path()
                ));
            }
        };

        Ok(ret)
    }
}
