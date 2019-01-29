module modUtils
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

implicit none

contains

function findFirstValueOverTheshold(vector,fromstart, eps) result(ifound)
!
real(RP), dimension(:) ::  vector ! must match interface
logical  :: fromstart
real(RP) :: eps

integer :: i, upbound, lowbound
integer :: ifound

ifound = -1

upbound  = UBOUND(vector,1,KIND(1))
lowbound = LBOUND(vector,1,KIND(1))

if (fromstart .EQV. .TRUE.) then ! start from first element
   do i = lowbound, upbound
      ! COMPLETE
      if (vector(i) > eps) then
         ifound = i
         exit
      end if
   end do
else ! start from last element
   do i = upbound, lowbound, -1
      if (vector(i) > eps) then
         ifound = i
         exit
      end if
   end do
endif

end function findFirstValueOverTheshold
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
function simpletrapz(n,array, a, b) result(integral)
! Compute simple trapezoidal integration n+1 values n intervals
integer, intent(in) :: n
real(RP), dimension(n), intent(in) :: array
real(RP), intent(in) :: a
real(RP), intent(in) :: b
real(RP) ::  integral

integral = array(1) + array(n) + 2*sum(array(2:n-1))
integral = 0.5_RP*(b-a)/(n-1)* integral

return
end function simpletrapz
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
subroutine zeropad(array, N)
! Zero-padding of array above mode number N
integer, intent(in) :: N
real(RP), allocatable, dimension(:), intent(inout) :: array

real(RP), allocatable, dimension(:) :: arraytemp
integer ::  Ntemp
!
! check if arrays are already allocated
if (allocated(array)) then
    Ntemp= size(array)
    if (Ntemp/= N .AND. Ntemp>0) then
       allocate(arraytemp(Ntemp))
       arraytemp = array
       deallocate(array)
       allocate(array(N))
       array=0.0_RP
       if (Ntemp > N) then
          array(1:N) = arraytemp(1:N)
       else
          array(1:Ntemp) = arraytemp(1:Ntemp)
       endif
       deallocate(arraytemp)
    endif
else
    allocate(array(N))
    array = 0.0_RP
end if

end subroutine zeropad

end module modUtils
