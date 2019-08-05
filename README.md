# CN-Stream
Computation of nonlinear regular ocean waves using stream function

[![Build Status](https://travis-ci.org/Gjacquenot/CN-Stream.svg?branch=master)](https://travis-ci.org/Gjacquenot/CN-Stream)
[![codecov](https://codecov.io/gh/Gjacquenot/CN-Stream/branch/master/graph/badge.svg)](https://codecov.io/gh/Gjacquenot/CN-Stream)

## Installation/Compilation

The code can be compiled on any computer architecture.
One only needs a Fortran compiler (for instance gfortran, the GNU Fortran compiler, part of GCC).
A makefile is provided but the recommended procedure is to use cmake.
The following commands can be executed in the root folder where `CMakeLists.txt` is located, to compile the dependency, the executable and the shared library:

- `cmake -H. -Bbuild`
- `cmake --build build`

Compilation has been tested with gfortran on different Unix/Linux platforms as well as in Windows environment.

For Windows environment, compilation using Intel Visual Studio has also been tested.
The program is provided with the corresponding project file `CN_Stream.vfproj` allowing a straightforward compilation of the code.

## Execution

### Use of CN-Stream as a stand-alone program

CN-Stream has been developed for command-line run with an input file located in the input folder containing all specifications needed.
All output files will be created in the directory `output`, but other specifications can be given.
Details of inputs and outputs are provided hereafter.
The executable can be run with the command `./mainCNS`.
The name of the dictionary can be specified as an argument.

### CN-Stream - library

There are two ways to use CN-Stream as a library.

- Use the declarations of the variables of the `variables_CN_Stream.h` and `variables_output_CN_Stream.h` and call the functions described in the source file `lib_CN_Stream.f90`.
- Use the subroutines indicated in the communication module `mod_CN_Stream.f90`.

## CN-Stream source files

### Project organisation and dependency

The main folder consists in:

- `CMakeLists.txt`
- `example` Folder with examples of using the library through the communication module from Fortran and C++
- `src` Folder with source files (include also the sources of libFyMc)
- `input` Folder with input file example
- `output` Default folder output

The code use the library libFyMc to read the “dictionary” input file.
The library is provided with the sources.

### CN-Stream - variables and types

The different Fortran types ([`RF_type`](src/libCNStream/variables_CN_Stream.h),
[`Option_type`](src/libCNStream/variables_CN_Stream.h),
[`Output_type`](src/libCNStream/variables_output_CN_Stream.h)) are defined explicitly in
[`variables_CN_Stream.h`](src/libCNStream/variables_CN_Stream.h) and
[`variables_output_CN_Stream.h`](src/libCNStream/variables_output_CN_Stream.h)
and are included when needed in CN-Stream.
It allows the user to include them easily into CN-Stream but also in another code, which makes use of the CN-Stream library.

In more details, those types include:

- `RF_type`
    * definition of the parameters of the wave, corresponding to the input parameters specified in the input file.
    * Modal amplitudes of the free surface elevation η and of the velocity potential φ (or equivalently the stream function ψ).
    * If needed, free surface elevation and slope in the spatial domain.
- `option_type`: all options relative to the solution method, specified in input file. This type also includes the optimal number of points N1 and N2 resulting from the dedicated procedure.
- `output_type`: defines for one location (X, Y, Z) the free surface elevation, the pressure and velocity components together with the necessary time and/or spatial derivatives of those components. The possible existence of a Y -component is associated to the definition of an angle of propagation as input, referenced as θ.

### CN-Stream - main program

The set of Fortran files needed in order to compile CN-Stream is listed hereafter with a brief description of the purpose of each of the source file.

- [**`mod_CN_Stream.f90`**](src/libCNStream/mod_CN_Stream.f90): Module allowing communication without using complex datatypes (C++)
- [**`main_CN_Stream.f90`**](src/libCNStream/main_CN_Stream.f90): Main program for CN-Stream computations
- [**`modSolve.f90`**](src/libCNStream/modSolve.f90): Solves the equation of the problem described above
- [**`modCNinitialize.f90`**](src/libCNStream/modCNinitialize.f90): Initialization of CN-Stream computation
- [**`modUtils.f90`**](src/libCNStream/modUtils.f90): Useful functions
- [**`modMatrix.f90`**](src/libCNStream/modMatrix.f90): Computes the inverse matrix from the least square method
- [**`modType.f90`**](src/libCNStream/modType.f90): Definition of types and useful constants
- [**`modModal.f90`**](src/libCNStream/modModal.f90): Useful functions used in modSolve.f90
- [**`HOS_modlinear_wave.f90`**](src/libCNStream/HOS_modlinear_wave.f90): Computation of linear dispersion relation
- [**`HOS_modmaths.f90`**](src/libCNStream/HOS_modmaths.f90): Useful mathematical functions
- [**`modSetupNameList.f90`**](src/libCNStream/modSetupNameList.f90): Read the input NML file
- [**`modReconstrucVol.f90`**](src/libCNStream/modReconstrucVol.f90): Evaluate wave elevation, pressure, velocity and its derivatives
- [**`modReconstruction.f90`**](src/libCNStream/modReconstruction.f90): Recompute wave elevation, pressure, velocity and wave slope from Fourier coefficients and the other way around
- [**`modOutputs.f90`**](src/libCNStream/modOutputs.f90): Write outputs on files
- [**`variables_CN_Stream.h`**](src/libCNStream/variables_CN_Stream.h): Definition of Fortran types: RF_type and option_type
- [**`variables_output_CN_Stream.h`**](src/libCNStream/variables_output_CN_Stream.h): Definition of Fortran type: output_type

## Inputs and Outputs

CN-Stream needs as input the characteristics of the wave, together with some informations relative to the numerical solution of the problem (target accuracy, etc.).
The wave can be described in dimensional or non-dimensional form.
As a matter of clarity, the wave parameters to provide as input are detailed in the next paragraphs depending on the need of the user.
Note that those parameters are provided within an input file which content is also detailed.

In CN-Stream, the non-linear regular water wave is characterized by:

- the water depth h, possibly infinite,
- the wave length λ or the wave period T,
- the wave height H (distance from crest to trough),
- the constant current superimposed to the wave (under the form of a Eulerian current or a given transport of mass).

### Input file

The input file is assumed to be named [`CN_Stream_input.dict`](input/CN_Stream_input.dict).
An example is provided in the [`input`](input/CN_Stream_input.dict) folder and details are provided hereafter.
The `Options_solver` parameters are useful for an advanced user, in order to obtain solutions with a controlled accuracy and/or to look for waves close to the wave breaking limit.

- - -
waveInput: Label (waveStream in example file) and Definition of the characteristics of the simulated wave
- - -
**`GeneralDimension`**: Dimensional (=1) or Non-dimensional (=0)

**`GeneralDepth`**: h if GeneralDimension=1 / h’ if GeneralDimension=0

**`GeneralModes`**: Number of modes for first evaluation

**`WaveInput`**: Period / Wavelength (if GeneralDimension=0 only Wavelengthis possible)

**`Period`**: period value if WaveInput set to Period

**`Wavelength`**: Wavelength value if WaveInput set accordingly

**`WaveHeight`**: H if GeneralDimension=1 / H’ if GeneralDimension=0

**`CurrentValue`**: value of current ; dimensional if input: GeneralDimension=1 / non-dimensional if input: GeneralDimension=1

**`CurrentType`**: type of current ; 1 mass transport / 0 eulerian current
- - -
Options for the numerical solution of the problem
- - -
**`n_H`**: Number of steps in wave height

**`err_type`**: Error type: 0 absolute ; 1 relative

**`eps_err`**: Tolerance on the equations

**`err_max`**: Error value over which computation is considered divergent

**`eps_inc`**: Convergence criteria on the unknowns

**`eps_N1`**: Decision criteria on the modes for the automatic adjustmentof N1

**`itermax`**: Maximum number of iterations

**`increment_type`**: choose between a linear or exponential incrementation: Increment type for wave height / 0 linear ; 1 exponential

**`printonscreen`**: print the intermediate results of the simulation on the command prompt: Print on screen =1 / do not print on screen = 0

**`writeoutput`**: Write output files =1 / do not Write output files = 0

**`subdict`**: Standard way to include dictionnary (here “Output” in another using libFyMc)
- - -
Outputs: supplementary info for outputs
- - -
**`Path`**: Specify output path (default: “./output/”)

**`x/y/z/time`**: Specify position to evaluate quantities for local outputs (default:“0.0”)

**`theta`**: Incident angle of waves (default:“0.0”)

### Output files

Depending on the choices made in the input file, different output files are created.
They are located at the root of the folder.
Input file also defines if outputs are dimensional or non-dimensional quantities.

Following files may be created:

- `waverf.cof` gives the main important parameters of the simulation, namely λ, H, k, T, c, cS, cE, N1 + 1, N2 + 1, R, h (in dimensional or non-dimensional form depending on the value of input: GeneralDimension in the input file) as well as the modal amplitudes an and bn,
- `waverf.dat` gives the modal amplitudes an and bn.
In complement, different subroutines may be called to write the necessary outputs needed by the user. They are available inside the source files and a simple call in the main program will enable the corresponding outputs:
- `WriteOutput`: this subroutine creates the file `resultsOutput.txt` containing at a given location and time all spatial quantities computed by CN-Stream (free surface elevation, velocities, pressures, derivatives, etc.).
- `TecplotOutput_Modes`: this subroutine creates the file `Modes_CN_Stream.dat` containing the modal description of the free surface elevation and velocity potential, for use with Tecplot.
- `TecplotOutput_VelocityPressure`: this subroutine creates the file `VP_card_fitted.dat` containing the velocity and pressure field under the simulated wave, for use with Tecplot.
- `TecplotOutput_FreeSurface`: this subroutine creates the file `FreeSurface_CN_Stream.dat`, which provides the free surface elevation and slope.
