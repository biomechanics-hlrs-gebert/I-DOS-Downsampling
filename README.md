# Downscaling

Downscaling of computed tomography images. For comparison between computing schemes as well as visualization.

## Meta Template
Located in: 
```
./datasets/I-DOSC.meta.template
```
For use with previously used data sets:
```
cat ./datasets/I-DOSC.meta.template >> Your_Meta_File.meta
```

## Requirements
* x86 64bit Hardware
* Linux x86 64Bit with Bash
* GNU Compiler Collection (GCC)
* An installation of Open-MPI

The installation of Open MPI can be done by the install script in the repository "Overview" of the biomechanics-hlrs-gebert organization @GitHub.

## Build
It's tested and recommended to build and run the program as follows. For developing the program on a laptop, "Julius" is the appropriate system.
### Set up the Environment
```vim ./central_src/auxiliaries/system_environments/<system>.sh```
```source ./environment.sh <system>``` 

* Set an architecture/a system
  * Give the absolute base path of your mpi-installation
  * Alternatively give the proper module names of your compute cluster

### Run make:
Build the program:    ```make```
Create documentation: ```make docs```

### Uninstall:
```make clean && rm -r <your program directory>```

## Usage
For example for testing on julius:
```
mpirun ./bin/dosc_v1.0.0_x86_64 -np 4 ./datasets/SC00-0_tc_Dev_ctif_G3S11Sig10.meta
```
### Datasets
... are transfered via file exchange and are not pushed into the repository. 

### External Sources
Plain text headers are parsed via a [strings module](https://gbenthien.net/strings/index.html) by George Benthien from San Diego.
### Arbitrary
Use this program at your own risk.
