module modReconstrucVol
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
!> Volume reconstruction
use modType
use modCNInitialize
use modUtils
use modReconstruction
use modSetupNameList

implicit none

!#include "variables_output_CN_Stream.h"

contains
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
subroutine Evaluate_hincidentRF(RF, option, x, y, time, thetaincident, eta, detadx, detadt)
!> Recompute wave elevation and its derivatives
Implicit None
type(RF_type), intent(inout)    :: RF
type(Option_type), intent(in)   :: option
REAL(RP), intent(in)            :: x, y, time, thetaincident ! x, y have to be dimensional
REAL(RP), intent(out)           :: eta
REAL(RP), optional, intent(out) :: detadx, detadt

REAL(RP) :: xoblique

xoblique= x*COS(thetaincident)+ y*SIN(thetaincident)
CALL RF_build_eta(RF, option, time, xoblique, 0.0_RP, eta, detadx, detadt)

end subroutine Evaluate_hincidentRF
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
subroutine Evaluate_HPUVW(RF, option, x, y, z, time, thetaincident, hydrostatic, output, max_exp)
! Recompute wave elevation, pressure, velocity and its derivatives
Implicit None
type(RF_type), intent(inout)   :: RF
type(Option_type), intent(in)  :: option
real(RP), intent(in)           :: x,y,z,time, thetaincident ! x, y, z have to be dimensional
type(Output_type), intent(out) :: output
logical, intent(in)            :: hydrostatic
real(RP), intent(in), optional           :: max_exp
real(RP) :: U, dUdx, dWdx
real(RP) :: xoblique, detax

xoblique= x*COS(thetaincident)+ y*SIN(thetaincident)

CALL RF_build_eta(RF, option, time, xoblique, 0.0_RP, output%eta, detax, output%detadt) ! detadx needs to be projected
output%detadx = detax * COS(thetaincident)
output%detady = detax * SIN(thetaincident)

CALL RF_build_UWP(RF, option, time, xoblique, z, 0.0_rp, hydrostatic, U, output%Vz, output%pressure, dUdx, dWdx, max_exp)
output%Vx = (U * COS(thetaincident))
output%Vy = (U * SIN(thetaincident))

output%dVxdx = (dUdx * COS(thetaincident) * COS(thetaincident))
output%dVydx = (dUdx * COS(thetaincident) * SIN(thetaincident))
output%dVzdx = (dWdx * COS(thetaincident))
output%dVxdy = output%dVydx !d(Vy)/dx = d(Vx)/dy   (since curl(uin)=0)
output%dVydy = (dUdx * SIN(thetaincident) * SIN(thetaincident))
output%dVzdy = (dWdx * SIN(thetaincident))
output%dVxdz = output%dVzdx !d(Vx)/dz = d(Vz)/dx   (since curl(uin)=0)
output%dVydz = output%dVzdy !d(Vy)/dz = d(Vz)/dy   (since curl(uin)=0)
output%dVzdz = - output%dVxdx - output%dVydy !d(Vx)/dx = - d(Vz)/dz - d(Vy)/dy (since div(uin) = 0)

end subroutine Evaluate_HPUVW

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
subroutine Check_FSBC(RF,option)
! Check the Free Surface Boundary Conditions in the whole domain
Implicit None
type(RF_type), intent(inout)      :: RF
type(Option_type), intent(in)  :: option

type(Output_type) :: output
real(RP)          :: time, xlocal, y, theta
real(RP)          :: eta, detadx, detadt, maxVz

real(RP), dimension (:), allocatable :: KFSBC, DFSBC

integer :: i

allocate(KFSBC(option%N2+1),DFSBC(option%N2+1))
!
time  = 0.0_rp
y     = 0.0_rp
theta = 0.0_rp
maxVz = 0.0_rp
!
do i = 1,option%N2+1
    xlocal = (i-1)*pi/RF%k/option%N2 ! only half a wavelength is discretized
    !
    call Evaluate_hincidentRF(RF, option, xlocal, y, time, theta, eta, detadx, detadt)
    call Evaluate_HPUVW(RF, option, xlocal, y, eta, time, theta, .TRUE., output)
    !
    KFSBC(i) = output%detadt - output%Vz + output%detadx*output%Vx
    DFSBC(i) = output%pressure
    maxVz    = MAX(maxVz, ABS(output%Vz))
    !
enddo

if (option%printonscreen==1) then
  print*, 'KFSBC (abs/rel)', maxval(abs(KFSBC)), maxval(abs(KFSBC))/maxVz
  print*, 'DFSBC (abs/rel)', maxval(abs(DFSBC)), maxval(abs(DFSBC))/maxval(abs(RF%g*eta_g))
endif

deallocate(KFSBC,DFSBC)

end subroutine Check_FSBC

end module modReconstrucVol
