# File Format to Meta Converter
![Architecture](https://img.shields.io/badge/Architecture-x86-green)
![OS](https://img.shields.io/badge/Linux-64Bit-green)
![version](https://img.shields.io/badge/version-2.0.0-green)
![Contributors](https://img.shields.io/badge/HLRS-NUM-blue)

Program to convert a *file* into the raw/meta format of my doctoral research project.

Currently, only *.vtk structured point files are accepted since it is the only required format. 

The program essentially introduces the meta file to the process chain.
##  Meta File Format
The restart handling is not adopted for this program as the need for compute time and the probability of failure are negligible.
## Usage:
The program currently only accepts *.vtk files with a proper meta basename.
```./xtom_v2.0.0_x86_64 <basename>.vtk```

## [semantic versioning](https://semver.org):
Given a version number MAJOR.MINOR.PATCH, increment the:

* MAJOR version when you major Features (i.e. new way of image processing),
* MINOR version when you extend functionality (i.e. new kernels), and
* PATCH version when you make bug fixes.

## Requirements
* x86 64bit Hardware
* Linux x86 64Bit Installation with Bash or Zsh
* GNU Compiler Collection (GCC), especially with gfortran
* An installation of Open-MPI

The program must be compiled with:
* Global integer kind=64Bit, signed
* Meta-format integer kind=64Bit, signed
* MPI integer kind=32Bit

The installation of Open MPI is simplified with the install script of the repository "Overview" of the biomechanics-hlrs-gebert organization @GitHub.
### Optional: Gnu debugging
* [gdb](https://www.gnu.org/software/gdb/)
* [tmpi](https://github.com/Azrael3000/tmpi)
* [tmux](https://github.com/tmux/tmux/wiki)
## External Sources
Plain text headers are parsed via a [strings module](https://gbenthien.net/strings/index.html) by George Benthien from San Diego.
## Arbitrary
Use this program at your own risk.

