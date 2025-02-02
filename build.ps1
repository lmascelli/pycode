# PLEASE USE ABSOLUTE PATHS
if ($IsLinux) {
    $env:HDF5_LIB_DIR = "/home/leonardo/Documents/unige/hdf5/1.14.5/lib"
    $env:HDF5_INCLUDE_DIR = "/home/leonardo/Documents/unige/hdf5/1.14.5/include"
    $env:HDF5_BIN_DIR = "/home/leonardo/Documents/unige/hdf5/1.14.5/lib"
} else {
    $env:HDF5_LIB_DIR = "C:/Users/Leonardo/Documents/unige/hdf5/1.14.5/lib"
    $env:HDF5_INCLUDE_DIR = "C:/Users/Leonardo/Documents/unige/hdf5/1.14.5/include"
    $env:HDF5_BIN_DIR = "C:/Users/Leonardo/Documents/unige/hdf5/1.14.5/bin"
}

function Script:PrintHelp {
    $PrintText = @'
================================================================================
                                    PYCODE
================================================================================

USAGE:
./build.ps1 COMMAND

AVAILABLE COMMANDS:

create-venv                   create a new virtual environment with the necessary
                              packages installed

develop                       build and install the library in the current venv

build                         build the pycode library

build-gui                     build the python gui of pycode

================================================================================
                                    
================================================================================
'@

    Write-Host $PrintText
}

$bin_dir = "Scripts"
if ($IsLinux) {
    $bin_dir = "bin"
}

switch($args[0]) {
    "create-venv" {
        python -m venv .venv
        Invoke-Expression ".venv/$bin_dir/pip install maturin matplotlib PySide6"
    }

    "develop" {
        Invoke-Expression "./.venv/$bin_dir/maturin develop"
    }

    "build" {
        Invoke-Expression "./.venv/$bin_dir/maturin build --release"
    }

    "build-gui" {
        $files = Get-ChildItem -Path "./pycode/gui/forms"
        foreach ($file in $files) {
            $fileWithoutExtension = [System.IO.Path]::GetFileNameWithoutExtension($file.FullName)
            $command = "./.venv/$bin_dir/pyside6-uic $file -o pycode/gui/${fileWithoutExtension}.py"
            Invoke-Expression $command
        }

        $files = Get-ChildItem -Path "./pycode/gui/resources" -Filter "*.qrc"
        foreach ($file in $files) {
            $fileWithoutExtension = [System.IO.Path]::GetFileNameWithoutExtension($file.FullName)
            $command = "./.venv/$bin_dir/pyside6-rcc $file -o pycode/gui/${fileWithoutExtension}.py"
            Invoke-Expression $command
        }
    }

    default {
        Script:PrintHelp
    }
}

Write-Output ""
