module modReconstruction
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
use modUtils

implicit none

contains
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
subroutine RF_calc_eta(option)
! Recompute wave elevation and wave slope from fourier coefficients
type(Option_type), intent(in) :: option

integer :: i
real(RP), dimension(option%N2+1) ::  phase
!
! check if arrays are already allocated
if (allocated(eta_g))  then
   if (size(eta_g) /= option%N2+1) then
      deallocate(eta_g)
      allocate(eta_g(option%N2+1))
   endif
else
    allocate(eta_g(option%N2+1))
end if

do i=1,(option%N2+1)
   phase(i) = dble(i-1) *pi /option%N2
enddo
!
! build elevation
do i=1,(option%N2+1)
   eta_g(i)   =   sum(a_g(1:option%N2+1) * cos((i-1)*phase(:) ))
enddo

end subroutine RF_calc_eta
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
subroutine RF_calc_slope(RF, option)
! Recompute wave elevation and wave slope from fourier coefficients
type(RF_type), intent(inout)  :: RF
type(Option_type), intent(in) :: option

integer :: i
real(RP), dimension(option%N2+1) ::  phase, indexm

if (allocated(slope_g))  then
   if (size(slope_g) /= option%N2+1) then
      deallocate(slope_g)
      allocate(slope_g(option%N2+1))
   endif
else
   allocate(slope_g(option%N2+1))
end if

do i=1,(option%N2+1)
   phase(i) = dble(i-1) *pi /option%N2
   indexm(i) = dble(i-1)
enddo
!
! build slope
do i=1,(option%N2+1)
   slope_g(i) = - sum(a_g(1:option%N2+1) * sin((i-1)*phase(:) ) * indexm(:)) * RF%k
enddo

end subroutine RF_calc_slope
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
subroutine RF_calc_a(option)
! Compute fourier coefficients of the wave elevation
type(Option_type), intent(in) :: option

integer :: i

real(RP), dimension(option%N2+1) :: phase
real(RP), dimension(option%N2+1) :: array

! check if arrays are already allocated
if (allocated(a_g)) then
    if (size(a_g) /= option%N2+1) then
       deallocate(a_g)
       allocate(a_g(option%N2+1))
    endif
else
    allocate(a_g(option%N2+1))
end if

do i=1,(option%N2+1)
    phase(i) = dble(i-1) *pi /option%N2
enddo

do i=1,(option%N2+1)
   array = eta_g(1:option%N2+1) * cos((i-1)*phase(1:option%N2+1) )
   a_g(i) = simpletrapz(option%N2+1,array, real(1,RP), real(option%N2+1,RP))
enddo

a_g = 2.0_rp * a_g / option%N2

end subroutine RF_calc_a
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
subroutine RF_build_eta(RF, option, t_loc, x_loc, t_0, eta, detadx, detadt)
! Recompute wave elevation and its space and time derivatives from fourier coefficients
! Evaluation done at (t_loc, x_loc)
Implicit None
type(RF_type), intent(inout)  :: RF
type(Option_type), intent(in) :: option
real(RP), intent(in)  ::  t_loc, x_loc, t_0
real(RP), intent(out) ::  eta, detadx, detadt

integer  :: j
real(RP) :: X, jk

X= x_loc - RF%c*(t_loc-t_0)
!
! estimates free surface elevation at position x_loc and time t_loc
eta    = a_g(1)*0.5_rp
detadt = 0.0_rp
detadx = 0.0_rp

do j = 1,option%N2
    jk = j*RF%k
    eta  = eta  + a_g(j+1) * cos(jk*X)
    detadt = detadt + a_g(j+1) *jk * RF%c * sin(jk*X)
    detadx = detadx - a_g(j+1) *jk * sin(jk*X)
enddo

end subroutine RF_build_eta
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
subroutine RF_build_UWP(RF, option, t_loc, x_loc, z_loc, t_0, hydrostatic, U, W, P, dUdx, dWdx, max_exp)
! Estimates horizontal and vertical velocity and pressure at position (x_loc,z_loc) and time t_loc
! Here if a profiling was showing that this reconstruction had a noticeable cost,
! it could be worth enter with an array of x_loc and an array of z_loc.
! The case with infinite depth could also be treated simplifying the equation
type(RF_type), intent(inout)  :: RF
type(Option_type), intent(in) :: option
real(RP), intent(in)  :: t_loc, x_loc, z_loc, t_0
logical, intent(in)   :: hydrostatic
real(RP), intent(out) :: U, W, P, dUdx, dWdx
real(RP), intent(in), optional  :: max_exp
integer  :: j
real(RP) :: jk, X, Z, Fact, coshJKZ, sinhJKZ, cosJKX, sinJKX, FactcosJKX, FactsinJKX

X = x_loc - RF%c*(t_loc-t_0)
Z = z_loc + RF%hdepth

U    = RF%c + b_g(1) !- RF%c_s ! Eulerian current - mass transport CHECK
dUdx = 0.0_RP
W    = 0.0_RP
dWdx = 0.0_RP
P    = RF%R - 0.5*RF%c*RF%c

do j = 1,option%N1
    jk=j*RF%k
    cosJKX = cos(jk*X)
    sinJKX = sin(jk*X)
    !
    if ((RF%hdepth > infinite_depth).OR.(jk*RF%hdepth > 500.0_rp)) then
        IF (present(max_exp)) THEN
        Fact = b_g(j+1) * jk * MIN(exp(jk*z_loc),max_exp)
        ELSE
        Fact = b_g(j+1) * jk * exp(jk*z_loc)
        END IF
        FactcosJKX = Fact * cosJKX
        FactsinJKX = Fact * sinJKX
        U = U + FactcosJKX
        W = W + FactsinJKX

        dUdx = dUdx - FactsinJKX * (-jk)
        dWdx = dWdx + FactcosJKX * jk
        P = P + RF%c * FactcosJKX
    else
        Fact = b_g(j+1) * jk / cosh(jk*RF%hdepth)
        IF (present(max_exp)) THEN
        coshJKZ = MIN(cosh(jk*Z),max_exp)
        sinhJKZ = MIN(sinh(jk*Z),max_exp)
        ELSE
        coshJKZ = cosh(jk*Z)
        sinhJKZ = sinh(jk*Z)
        END IF
        FactcosJKX = Fact * cosJKX
        FactsinJKX = Fact * sinJKX

        U = U + FactcosJKX * coshJKZ
        W = W + FactsinJKX * sinhJKZ
        dUdx = dUdx + FactsinJKX * coshJKZ * (-jk)
        dWdx = dWdx + FactcosJKX * sinhJKZ * jk
        P = P + RF%c *FactcosJKX * coshJKZ
    endif
enddo

P = P - (U*U + W*W)*0.5 ! non-linear
if (hydrostatic) then
    P = P - RF%g*z_loc ! hydrostatic
endif

end subroutine RF_build_UWP

end module modreconstruction
