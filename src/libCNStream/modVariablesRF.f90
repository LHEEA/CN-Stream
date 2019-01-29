MODULE modVariablesRF
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
use iso_c_binding
use mfpGeneral, only : typDictionaryPtr

real(RP), dimension (:), allocatable :: a_g
real(RP), dimension (:), allocatable :: b_g
real(RP), dimension (:), allocatable :: eta_g
real(RP), dimension (:), allocatable :: slope_g

type(typDictionaryPtr) :: param_optionDict

type, BIND(C) :: Output_type
    real(c_double) :: eta
    real(c_double) :: pressure
    real(c_double) :: Vx, Vy, Vz

    real(c_double) :: detadt, detadx,detady
    real(c_double) :: dVxdx, dVxdy, dVxdz
    real(c_double) :: dVydx, dVydy, dVydz
    real(c_double) :: dVzdx, dVzdy, dVzdz
end type Output_type

type, BIND(C)  :: RF_type
    integer(c_int) :: dimen          ! dimension
    integer(c_int) :: current
    integer(c_int) :: lorT           ! input wavelength (=1) or period (=0)

    real(c_double) :: g             ! gravity acceleration
    real(c_double) :: hdepth        ! depth
    real(c_double) :: H             ! wave height
    real(c_double) :: k             ! wave number
    real(c_double) :: lambda        ! wavelength
    real(c_double) :: T             ! period
    real(c_double) :: U             !
    real(c_double) :: C

    !real(RP), dimension (:), allocatable :: a
    !real(RP), dimension (:), allocatable :: b
    !real(RP), dimension (:), allocatable :: eta
    !real(RP), dimension (:), allocatable :: slope

    real(c_double) :: R
    real(c_double) :: Q
    real(c_double) :: C_E, C_S
end type RF_type

type, BIND(C) :: Option_type
    integer(c_int)  :: n_H
    integer(c_int)  :: err_type
    real(c_double) :: eps_err
    real(c_double) :: err_max
    real(c_double) :: eps_inc
    real(c_double) :: eps_N1
    integer(c_int)  :: itermax
    integer(c_int)  :: increment_type  ! 0 linear ; 1 exponentiel
    integer(c_int)  :: N1              ! number of modes for stream function
    integer(c_int)  :: N2
    integer(c_int)  :: N1_eff
    integer(c_int)  :: N2_eff
    integer(c_int)  :: modes
    integer(c_int)  :: printonscreen   ! 1 yes ; 0 no
    integer(c_int)  :: writeoutput   ! 1 yes ; 0 no
end type Option_type

type, BIND(C) :: Input_type
    integer  :: GeneralDimension
    real(c_double) :: GeneralDepth
    integer(c_int)  :: GeneralModes
    integer(c_int)  :: WaveInput
    real(c_double) :: WaveValue
    real(c_double) :: WaveHeight
    real(c_double) :: CurrentValue
    integer(c_int)  :: CurrentType
end type Input_type

END MODULE modVariablesRF
