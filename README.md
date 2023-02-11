# Downscaling
[![DOI](https://zenodo.org/badge/448135831.svg)](https://zenodo.org/badge/latestdoi/448135831)
[![License: MIT](https://img.shields.io/badge/License-MIT-success.svg)](https://opensource.org/licenses/MIT)
![Architecture](https://img.shields.io/badge/Architecture-x86_64-blue)
![OS](https://img.shields.io/badge/OS-Linux-blue)
![version](https://img.shields.io/badge/version-1.0.0-blue)

Downscaling of computed tomography images. For comparison between computing schemes as well as visualization.

## Meta Template
Located in: 
```
./datasets/I-DOS.meta.template
```
For use with previously used data sets:
```
cat ./datasets/I-DOS.meta.template >> Your_Meta_File.meta
```
## Usage
For example for testing on julius:
```
./bin/dos_v1.5.0_x86_64 <basename>.meta```
```
## Datasets
... are transfered via file exchange and are not pushed into the repository. 
## Requirements
* x86 64bit Hardware
* Linux x86 64Bit Installation with Bash or Zsh
* GNU Compiler Collection (GCC), especially with gfortran

The program must be compiled with:
* Global integer kind=64Bit, signed
* Meta-format integer kind=64Bit, signed

## Build
It's tested and therefore recommended to build and run the program as follows.
### Set up the Environment
```vim ./auxiliaries/system_environments/<system>.env```
```source ./environment.source <system>``` 

### Run make:
Build the program:    ```make```
Create documentation: ```make docs```

### Uninstall:
```make clean && rm -r <your program directory>```
## Acknowledgements 
Plain text parsed via [strings module](https://gbenthien.net/strings/index.html) by George Benthien from San Diego.
## Arbitrary
Use this program at your own risk.
