program RF_Rienecker_Fenton
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!    Copyright (C) 2019 - LHEEA Res. Dept, Ecole Centrale de Nantes, UMR CNRS 6598
!
!    This program is part of CN-Stream
!
!    CN-Stream is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
    Implicit none

    Call orgProgram()
    !Call checkCNStream()

end program RF_Rienecker_Fenton

Subroutine orgProgram

!> main program of CN-Stream
use modCNInitialize
use modSetupNameList
use modSolve
use modReconstrucVol
use modOutputs

type(Input_type)  :: input
type(Option_type) :: option
type(RF_type)     :: RF
type(Output_type) :: output

type(typDictionaryPtr) :: outputsDict

character(len=StringLength) :: ConfigFile
character(len=5) :: ExtDictOrNml
real(RP) :: x, y, z !x, y, z have to be dimensional
real(RP)           :: time, thetaincident !time has to be dimensional
logical          :: hydrostatic = .FALSE.
logical :: WriteAscii = .TRUE.

!
!Define input parameters from a file
if (iargc()>0) then
   call getarg(1, ConfigFile)
else
   ConfigFile="./input/CN_Stream_input.dict"
endif

ExtDictOrNml = ConfigFile(len_trim(ConfigFile)-4:len_trim(ConfigFile))


if (ExtDictOrNml(2:) == '.nml') then
  write(*,*)'Extensionfrom config file '//ExtDictOrNml(2:)
  write(*,*) "CN-Stream can not setup from namelist anymore"
  !call setupFromNamelistFile(ConfigFile, input, option)
elseif (ExtDictOrNml == '.dict') then
  write(*,*)'Extensionfrom config file '//ExtDictOrNml
  write(*,*) "CN-Stream will setup from dict"
  call setupFromFymcFile(ConfigFile, input, option)
else
  write(*,*) "Please give .dict extension to your input file"
  read(*,*)
endif

CALL printSetupFile(input,option)

!
! Initialize all necessary values
call RF_initialize(input, RF, option)
!
! Solution of the problem
call RF_solve_auto(RF,option)
!
! Eulerian current
RF%c_E = RF%c + b_g(1);
!  mass transport
RF%c_S = RF%c_E - RF%Q/RF%hdepth;
!
! Write the coefficients for use in complementary programs
if(option%writeoutput==1) then
  call RF_write_waverfcof(RF, option)
endif
! Check the solution by reconstructing on the free surface
call Check_FSBC(RF, option)
!
! Check the evaluation at a given location
outputsDict = param_optionDict%subDict("Outputs")
x=outputsDict%getRealOrDefault("x",0.0_rp)
y=outputsDict%getRealOrDefault("y",0.0_rp)
z=outputsDict%getRealOrDefault("z",0.0_rp)
time=outputsDict%getRealOrDefault("time",0.0_rp)
thetaincident=outputsDict%getRealOrDefault("theta",0.0_rp)
! time=0.0_rp
! thetaincident=0.0_rp
!
call Evaluate_HPUVW(RF, option, x, y, z, time, thetaincident, hydrostatic, output)
if (WriteAscii) call WriteOutput(output, .TRUE., option)
!
! Tecplot output of the solution for visual checking
! Modal description of eta and phi
call TecplotOutput_Modes(RF,option)
! Velocity and pressure fields
call TecplotOutput_VelocityPressure(RF,option, y, time, thetaincident)
! Free surface description (in space)
call TecplotOutput_FreeSurface(RF,option, y, time, thetaincident)

End Subroutine

! subroutine checkCNStream
!     use mod_CN_Stream
!     use modType

!     character(len=StringLength) :: ConfigFile

!     ConfigFile="./input/CN_Stream_input.nml"
!     Call initializeRF(ConfigFile)

! End Subroutine
