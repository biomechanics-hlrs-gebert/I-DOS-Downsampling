# File Format to Meta Converter
![Architecture](https://img.shields.io/badge/Architecture-x86-green)
![OS](https://img.shields.io/badge/Linux-64Bit-green)
![version](https://img.shields.io/badge/version-2.0.0-green)
![Contributors](https://img.shields.io/badge/HLRS-NUM-blue)

Program to convert a *file* into the raw/meta format of my doctoral research project.

Currently, only *.vtk structured point files are accepted since it is the only required format. 

The program essentially introduces the meta file to the process chain.

## Usage:
The program currently only accepts *.vtk files with a proper meta basename.
```./xtom_v2.0.0_x86_64 <basename>.vtk```