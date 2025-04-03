use super::*;

#[test]
fn open_file() {
    py_init();
    let filename = "/home/leonardo/Documents/unige/data/12-04-2024/39480_DIV77/raw/2_basale.h5";
    let phase = PyPhase::new(filename);
    py_close();
}
