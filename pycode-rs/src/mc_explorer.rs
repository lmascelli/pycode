use mc_explorer::H5Content;
use pyo3::prelude::*;

#[pyclass(name = "MCExplorer")]
pub struct PyMCExplorer {
    content: Option<H5Content>,
}

#[pymethods]
impl PyMCExplorer {
    #[new]
    pub fn new(filename: &str) -> Self {
        let content = if let Ok(content) = H5Content::open(filename) {
            println!("{} file loaded", filename);
            Some(content)
        } else {
            println!("Error loading PyMCExplorer: {}", filename);
            None
        };
        Self { content }
    }

    pub fn list_recordings(&self) -> Vec<(usize, String)> {
        if let Some(ref content) = self.content {
            content.list_recordings()
        } else {
            vec![]
        }
    }

    pub fn list_analogs(&self, recording_index: usize) -> Option<Vec<(usize, String)>> {
        if let Some(ref content) = self.content {
            match content.get_recording(recording_index) {
                Ok(recording) => {
                    Some(recording.list_analogs())
                },
                Err(err) => {
                    eprintln!("{err}");
                    None
                }
            }
        } else {
            eprintln!("MCExplorer: no content set");
            None
        }
    }

    pub fn list_analog_channels(&self, recording_index: usize, analog_index: usize
                                ) -> Option<Vec<String>> {
        if let Some(ref content) = self.content {
            match content.get_recording(recording_index) {
                Ok(recording) => {
                    match recording.get_analog(analog_index) {
                        Ok(analog) => {
                            Some(analog.get_labels())
                        },
                        Err(err) => {
                            eprintln!("{err}");
                            None
                        }
                    }
                },
                Err(err) => {
                    eprintln!("{err}");
                    None
                }
            }
        } else {
            None
        }
    }

    pub fn get_channel_data(&self,
                            recording_index: usize,
                            analog_index: usize,
                            channel_label: &str) -> Option<Vec<f32>> {
        if let Some(ref content) = self.content {
            match content.get_recording(recording_index) {
                Ok(recording) => {
                    match recording.get_analog(analog_index) {
                        Ok(analog) => {
                            match analog.get_channel(channel_label) {
                                Ok(data) => Some(data.clone()),
                                Err(err) => {
                                    eprintln!("{err}");
                                    None
                        }
                            }
                        },
                        Err(err) => {
                            eprintln!("{err}");
                            None
                        }
                    }
                },
                Err(err) => {
                    eprintln!("{err}");
                    None
                }
            }
        } else {
            None
        }
    }
}
