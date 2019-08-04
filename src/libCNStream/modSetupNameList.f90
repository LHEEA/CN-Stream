module modSetupNameList
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
use modType
use modCNInitialize
use mfpGlobal, only : CHAR_LEN
use mfpGeneral, only : typDictionaryPtr, isStringEqual

contains

! subroutine setupFromNamelistFile(ConfigFile, input, option)
! implicit none
!
! character(len=StringLength), intent(in) :: ConfigFile
!
! integer :: ios
!
! type(Input_type), intent(out) :: input
! type(Option_type), intent(out) :: option
!
! namelist / Input_RF / input
! namelist / Options_solver / option
!
! WRITE(*,*) "I am here" //ConfigFile
! !
! ! input file for Rienecker and Fenton computation package
! open(11, file=ConfigFile,iostat=ios)
! !
! if(ios==0)then
!
!    write(*,*)'Read input from config file '//ConfigFile(:len_trim(ConfigFile))
!    rewind(11)
!
!    !===========================================================================
!    ! default values
!    !===========================================================================
!
!    read(11,nml=Input_RF,iostat=ios)
!    read(11,nml=Options_solver,iostat=ios)
!
!   WRITE(*,*) "input", input
! endif
!
! end subroutine setupFromNamelistFile


subroutine setupFromFymcFile(ConfigFile, input, option)
implicit none

type(typDictionaryPtr) :: dictPtr, inputDict, optionDict
character(len=CHAR_LEN) :: fileDir
character(len=CHAR_LEN) :: fileName
character(len=CHAR_LEN) :: fileExt

character(len=StringLength), intent(in) :: ConfigFile

character(len=StringLength) :: inputDictName, waveInputType

type(Input_type), intent(out) :: input
type(Option_type), intent(out) :: option

!namelist / Input_RF / input
!namelist / Options_solver / option


!!... Set Dictionary File Path
fileDir  = ""
fileName = ConfigFile
fileExt  = ""
!!... Intialize Dictionary
Call dictPtr%initialize(fileDir, fileName, fileExt)

!!... Show Dictionary Contents
Call dictPtr%print()

inputDictName = dictPtr%getChar("waveInput");
inputDict = dictPtr%subDict(inputDictName)

Call inputDict%print()

input%GeneralDimension=inputDict%getInt("GeneralDimension")     ! Dimensional (=1) or Non-dimensional (=0)
input%GeneralDepth=inputDict%getReal("GeneralDepth")     ! h if input%GeneralDimension=1 / h' (see report_R&F.pdf) if input%GeneralDimension=0
input%GeneralModes=inputDict%getInt("GeneralModes")        ! Number of modes for first evaluation

waveInputType = inputDict%getCharOrDefault("WaveInput","Period")            ! 0=period / 1=wavelength

if (isStringEqual(waveInputType,"Period")) then
  input%WaveValue=inputDict%getReal("Period") ! period or wavelength / be careful: if input%GeneralDimension=0 --> input%WaveValue is set to 1
  input%WaveInput= 0
else if (isStringEqual(waveInputType,"waveLength")) then
  input%WaveValue=inputDict%getReal("waveLength") ! period or wavelength / be careful: if input%GeneralDimension=0 --> input%WaveValue is set to 1
  input%WaveInput= 1
else
  write(*,*) "   [Error] Wrong waveInput is given. "
  write(*,*) "       Given Input Type : ", trim(waveInputType)
  stop
endif

input%WaveHeight=inputDict%getReal("WaveHeight")      ! H if input%GeneralDimension=1 / H' (see report_R&F.pdf) if input%GeneralDimension=0
input%CurrentValue=inputDict%getReal("CurrentValue")     ! value of current ; dimensional if input%GeneralDimension=1 / non-dimensional (see report_R&F.pdf) if input%GeneralDimension=1
input%CurrentType=inputDict%getInt("CurrentType")          ! type of current ; 1 mass transport / 0 eulerian current

optionDict = dictPtr%subDict("Options")
Call optionDict%print()

option%n_H = optionDict%getInt("n_H")           ! Number of steps in wave height
option%err_type = optionDict%getInt("err_type")        ! Error type / 0 absolute ; 1 relative
option%eps_err  = optionDict%getReal("eps_err")    ! Tolerance on the equations
option%err_max  = optionDict%getReal("err_max")  ! Divergence criteria
option%eps_inc  = optionDict%getReal("eps_inc")    ! Convergence criteria on unknowns
option%eps_N1   = optionDict%getReal("eps_N1")    ! Decision criteria on the modes
option%itermax  = optionDict%getInt("itermax")      ! Maximum number of iterations
option%increment_type = optionDict%getInt("increment_type")  ! Increment type for wave height / 0 linear ; 1 exponential
option%printonscreen = optionDict%getInt("printonscreen")   ! Print on screen =1 / do not print on screen = 0
option%writeoutput = optionDict%getInt("writeoutput")         ! Write output files =1 / do not Write output files = 0
param_optionDict = optionDict

end subroutine setupFromFymcFile

!-------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

end module modSetupNameList
