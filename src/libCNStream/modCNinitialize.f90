module modCNInitialize
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
use modVariablesRF
use modType
use HOS_modlinear_wave
!use mfpGlobal
use mfpGeneral, only : typDictionaryPtr

implicit none

integer, parameter :: DIVERGENCE = 0
integer, parameter :: MIN_DY = 1
integer, parameter :: NORMAL = 2

! type, public :: Input_type
!     integer  :: GeneralDimension
!     real(RP) :: GeneralDepth
!     integer  :: GeneralModes
!     integer  :: WaveInput
!     real(RP) :: WaveValue
!     real(RP) :: WaveHeight
!     real(RP) :: CurrentValue
!     integer  :: CurrentType
! end type Input_type

!#include "variables_CN_Stream.h"

contains

subroutine RF_initialize(input, RF, option)
!
! RF_initialize gives Rienecker and Fenton solution
! RF = RF_initialize(h, H, lambdaorT, value, N1, U, type)
!
! Description
! The RF_initialize function is part of Riencker and Fenton solution package.
! it is based on paper by M.M. Rienecker and J.D. Fenton,
! A Fourier approximation method for steady water waves.
! J. Fluid Mech., Vol. 104, pp. 119-137, 1981.
!
! the function runs without input (reads the RF.ini configuration file)
! or with all inputs
! h            dimensional depth
! H            dimensional wave height
! lambdaorT    input wavelength (=1) or period (=0)
! value        wavelength or period
! N1           number of modes for the stream function
! U            current velocity in m/s
! type         current type
!
! Output
! RF structure which contains 6 elements
!   RF.input    values read from RF.ini file
!   RF.g        gravity acceleration
!   RF.h        depth
!   RF.N1       number of modes for stream function
!   RF.H        wave height
!   RF.lorT     input wavelength (=1) or period (=0)
!   RF.lambda or RF.T wavelength or period
!
! implemented by Pierre FERRANT and modified by Felicien BONNEFOY - LHEEA - 2015
!
! See also RF_solve, RF_calcuvcs, RF_calcer, RF_calcA, RF_iterate.
!
type(Input_type), intent(in)     :: input
type(Option_type), intent(inout) :: option
type(RF_type) :: RF

RF%hdepth  = input%GeneralDepth ! be careful : RF%hdepth should be dimensional if input%GeneralDimension=1 and non-dimensional (kh) if input%GeneralDimension=0
RF%dimen   = input%GeneralDimension
option%N1  = input%GeneralModes

RF%lorT = input%WaveInput ! 1 for wavelength, 0 for period

if (RF%dimen == 0) then
    ! Ensure that when non-dimensional, this is solved with lambda
    if (RF%lorT == 1) then ! wavelength
        RF%lambda = 1
    else  ! period
        RF%T = 1
    end if
else
    if (RF%lorT == 1) then ! wavelength
        RF%lambda = input%WaveValue
    else ! period
        RF%T = input%WaveValue
    end if
end if
!
if (RF%hdepth < 0) then
    RF%hdepth = huge(RP)
end if
if (RF%dimen == 0) then
    RF%g = 1
else
    RF%g = g
end if
!
if (RF%lorT == 1) then ! wavelength is given
   RF%k = 2*pi/ RF%lambda ! here lambda is already non-dimensional if RF%adim =0
end if
!
! linear dispersion
if (RF%lorT == 1) then ! wavelength is given
   ! if non-dimensional input, this k*hdepth which is given... and not hdepth!
   if ((RF%dimen == 0).and.(RF%hdepth < infinite_depth)) then
      RF%hdepth = RF%hdepth/RF%k
   endif
   RF%T = 2*pi/sqrt(RF%g*RF%k*tanh(RF%k * RF%hdepth))  ! here k and h and g are already non-dimensional if RF%adim =0
else
   if (RF%hdepth > infinite_depth) then
      RF%k = (2*pi/RF%T)**2 / RF%g ! ! here T and g are already non-dimensional if RF%adim =0
   else
      ! if non-dimensional input, this hdepth/(gT**2) which is given... and not hdepth!
      if (RF%dimen == 0) then
         RF%hdepth = RF%hdepth*RF%g*RF%T**2
      endif
      RF%k = wave_number_r(1/RF%T, RF%hdepth,RF%g,option%eps_err) ! here T, h and g are already non-dimensional if RF%adim =0
   end if
end if
!
RF%c = 2*pi / (RF%k*RF%T) ! here T and k are already non-dimensional if RF%adim =0
!
if (RF%dimen == 0) then
    if (RF%lorT == 1) then ! wavelength is given
        RF%H = input%WaveHeight/RF%k               ! be careful : RF%H should be dimensional if input%GeneralDimension=1 and non-dimensional (kH) if input%GeneralDimension=0
        RF%U = input%CurrentValue*sqrt(RF%g/RF%k)  ! be careful : RF%U should be dimensional if input%GeneralDimension=1 and non-dimensional if input%GeneralDimension=0
    else
        RF%H = input%WaveHeight*RF%g*RF%T**2       ! be careful : RF%H should be dimensional if input%GeneralDimension=1 and non-dimensional (kH) if input%GeneralDimension=0
        RF%U = input%CurrentValue*RF%g*RF%T        ! be careful : RF%U should be dimensional if input%GeneralDimension=1 and non-dimensional if input%GeneralDimension=0
    endif
else
    RF%H = input%WaveHeight        ! be careful : RF%H should be dimensional if input%GeneralDimension=1 and non-dimensional (kH) if input%GeneralDimension=0
    RF%U = input%CurrentValue      ! be careful : RF%U should be dimensional if input%GeneralDimension=1 and non-dimensional if input%GeneralDimension=0
endif
RF%current = input%CurrentType

end subroutine RF_initialize

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
subroutine DeallocateRF()
!
IF (ALLOCATED (a_g)) DEALLOCATE (a_g)
IF (ALLOCATED (b_g)) DEALLOCATE (b_g)
IF (ALLOCATED (eta_g)) DEALLOCATE (eta_g)
IF (ALLOCATED (slope_g)) DEALLOCATE (slope_g)

end subroutine DeallocateRF
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
subroutine WriteInputOption(input, option, onFile)
type(Input_type), intent(in)  :: input
type(Option_type), intent(in) :: option
logical, intent(in)           :: onFile

integer :: out_unit

out_unit = 20
if (onFile) then
  open (unit=out_unit,file="results.txt",action="write",status="replace")
else
  out_unit = 6 ! Screen
end if

write(out_unit,*) "input%GeneralDimension", input%GeneralDimension
write(out_unit,*) "input%GeneralDepth", input%GeneralDepth
write(out_unit,*) "input%GeneralModes", input%GeneralModes
write(out_unit,*) "input%WaveInput", input%WaveInput
write(out_unit,*) "input%WaveValue", input%WaveValue
write(out_unit,*) "input%WaveHeight", input%WaveHeight
write(out_unit,*) "input%CurrentValue", input%CurrentValue
write(out_unit,*) "input%CurrentType", input%CurrentType

write(out_unit,*) "option%n_H", option%n_H
write(out_unit,*) "option%eps_err", option%err_type
write(out_unit,*) "option%eps_err", option%eps_err
write(out_unit,*) "option%err_max", option%err_max
write(out_unit,*) "option%eps_inc", option%eps_inc
write(out_unit,*) "option%eps_N1", option%eps_N1
write(out_unit,*) "option%itermax", option%itermax
write(out_unit,*) "option%increment_type", option%increment_type

if (onFile) then
   close (out_unit)
endif

end subroutine WriteInputOption

end module modCNInitialize
