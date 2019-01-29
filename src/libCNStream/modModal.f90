module modModal
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

implicit none

type, public :: Modal_type
    real(RP), dimension(:),  allocatable :: U, V
    real(RP), dimension(:,:),  allocatable :: C1, C2, S1, S2
end type Modal_type

contains
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
function RF_calcuvcs(RF, option) result(modal)
!
! RF_calcuvcs estimates the exponential terms required for Rienecker and
! Fenton computation
! [U,V,S1,C1,S2,C2] = RF_calcuvcs(RF)
!
! Description
! The RF_calcuvcs function is part of Riencker and Fenton solution package.
! it is based on paper by M.M. Rienecker and J.D. Fenton,
! A Fourier approximation method for steady water waves.
! J. Fluid Mech., Vol. 104, pp. 119-137, 1981.
!
! implemented by Pierre FERRANT and modified by Félicien BONNEFOY - LHEEA - 2015
!
! See also RF_solve, RF_calcA, RF_calcer.
!
type(Option_type), intent(in) :: option
type(RF_type), intent(in)     :: RF
type(Modal_type)              :: modal

real(RP) :: choverch, shoverch
integer :: M, J

allocate( modal%U(option%N2+1), modal%V(option%N2+1) )
allocate( modal%C1(option%N1+1,option%N2+1), modal%C2(option%N1+1,option%N2+1))
allocate( modal%S1(option%N1+1,option%N2+1), modal%S2(option%N1+1,option%N2+1))

modal%U=0_RP
modal%V=0_RP
modal%C1=0_RP
modal%C2=0_RP
modal%S1=0_RP
modal%S2=0_RP

if (RF%hdepth < infinite_depth) then
   modal%C1(1,1:option%N2+1) = 1.0_RP
end if

modal%C2(1,1:option%N2+1) = 1.0_RP

if (option%N1*RF%k*maxval(eta_g) > 650) then
   WRITE(*,*) 'Exponential argument is high'
   READ(*,*)
end if

do M=1,option%N2+1
   do J=1,option%N1
      if (RF%hdepth > infinite_depth) then!	deep water case
         modal%C2(J+1,M)=cos((M-1)*J*pi/option%N2) * exp(J*RF%k*eta_g(M))
         modal%S2(J+1,M)=sin((M-1)*J*pi/option%N2) * exp(J*RF%k*eta_g(M))
         modal%C1 = modal%C2
         modal%S1 = modal%S2
      else ! finite depth
         choverch = (exp(J*RF%k*eta_g(M)) + exp(-J*RF%k*(eta_g(M) + 2*RF%hdepth ))) / (1 + exp(-2*J*RF%k*RF%hdepth ))
         shoverch = (exp(J*RF%k*eta_g(M)) - exp(-J*RF%k*(eta_g(M) + 2*RF%hdepth ))) / (1 + exp(-2*J*RF%k*RF%hdepth ))
         modal%C2(J+1,M)=cos((M-1)*J*pi/option%N2) * choverch
         modal%C1(J+1,M)=cos((M-1)*J*pi/option%N2) * shoverch
         modal%S1(J+1,M)=sin((M-1)*J*pi/option%N2) * choverch
         modal%S2(J+1,M)=sin((M-1)*J*pi/option%N2) * shoverch
      end if

      modal%U(M) = modal%U(M) + J*b_g(J+1) * modal%C2(J+1,M)
      modal%V(M) = modal%V(M) + J*b_g(J+1) * modal%S2(J+1,M)

   end do

   modal%U(M) = modal%U(M) * RF%k + b_g(1)
   modal%V(M) = modal%V(M) * RF%k

end do

end function
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
subroutine RF_calcer(RF, option, modal, F)
!
! RF_calcer estimates the error terms in the Rienecker and
! Fenton computation
! F = RF_calcer(U,V,C1,RF)
!
! Description
! The RF_calcer function is part of Rienecker and Fenton solution package.
! it is based on paper by M.M. Rienecker and J.D. Fenton,
! A Fourier approximation method for steady water waves.
! J. Fluid Mech., Vol. 104, pp. 119-137, 1981.
!
! implemented by Pierre FERRANT and modified by Félicien BONNEFOY - LHEEA - 2015
!
! See also RF_solve, RF_calcA, RF_calcuvcs.
!
type(Option_type), intent(in) :: option
type(RF_type), intent(in)     :: RF
type(Modal_type)              :: modal

real(RP), dimension(:), allocatable, intent(out) :: F
integer :: I

allocate(F(option%N2+option%N2+6))
! KFSBC
do I = 1,option%N2+1
   ! KFSBC
   F(I) = sum(b_g(2:option%N1+1) * modal%C1(2:option%N1+1,I)) + b_g(1) * eta_g(I) + RF%Q
   ! DFSBC
   F(option%N2+1+I) = 0.5 * (modal%U(I)**2 + modal%V(I)**2) + RF%g*eta_g(I) - RF%R
end do
!
! Volume
F(option%N2+option%N2+3) = eta_g(1) + 2 * sum(eta_g(2:option%N2)) + eta_g(option%N2+1)
!
! Wave height
F(option%N2+option%N2+4) = eta_g(1) - eta_g(option%N2+1) - RF%H
!
! Current, equation for c
if (RF%current== 1) then
   ! U = mass transport velocity
   F(option%N2+option%N2+5) = RF%c - (RF%Q/RF%hdepth - b_g(1)) - RF%U
else
   ! U = eulerian current
   F(option%N2+option%N2+5) = RF%c + b_g(1) - RF%U
end if
!
! wavelength or period
F(option%N2+option%N2+6) = RF%k*RF%c*RF%T-2*pi ! equation for T if RF.lorT == 1 or k otherwise

end subroutine RF_calcer
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
subroutine RF_calcA(RF, option, modal, A)
!
! RF_calcA estimates the Jacobian matrix in the Rienecker and
! Fenton computation
! A = RF_calcA(U,V,S1,C1,S2,C2,RF)
!
! Description
! The RF_calcA function is part of Riencker and Fenton solution package.
! it is based on paper by M.M. Rienecker and J.D. Fenton,
! A Fourier approximation method for steady water waves.
! J. Fluid Mech., Vol. 104, pp. 119-137, 1981.
!
! implemented by Pierre FERRANT and modified by Félicien BONNEFOY - LHEEA - 2015
!
! See also RF_solve, RF_calcuvcs, RF_calcer.
!
type(Option_type), intent(in) :: option
type(RF_type), intent(in)     :: RF

type(Modal_type)              :: modal

integer  :: I, J, N1, N2
real(RP) :: AA, BB, sumTerm, sumterm2

real(RP), dimension(:,:), allocatable, intent(out) :: A

N1 = option%N1
N2 = option%N2

allocate( A(N2+N2+6,N1+N2+6) )
A = 0.0_RP
!
! KFSBC
do I = 1,N2+1
   A(I,I)    = modal%U(I)
   A(I,N2+2) = eta_g(I)
   do J=1,N1
      A(I,N2+2+J) = modal%C1(J+1,I)
   end do
   A(I,N1+N2+6) = 1
end do
!
! In this cycle, M has been replaced with I see matlab code look strange
if (RF%lorT == 0) then
  ! RF%k is unknown  N1+N2+5
   do I=1,N2+1! for both deep water and finite depth
      A(I,N1+N2+5) = eta_g(I) / RF%k * (modal%U(I)-b_g(1))
   end do

   if (RF%hdepth < infinite_depth) then
      do I=1,N2+1 ! for finite depth only
         sumTerm= 0.0_RP
         do J  = 1,N1
            sumTerm = sumTerm + J*b_g(J+1) * modal%C1(J+1,I) *tanh(J*RF%k*RF%hdepth)
         enddo
         A(I,N1+N2+5) = A(I,N1+N2+5) - RF%hdepth * sumTerm
      end do
   end if
end if
!
! DFSBC
do I = 1,N2+1
   AA=0.0_RP
   BB=0.0_RP

   do J  = 1,N1
      AA = AA + J*J * b_g(J+1) * modal%C1(J+1,I)
      BB = BB + J*J * b_g(J+1) * modal%S1(J+1,I)
      A(N2+1+I, N2+2+J) = J * RF%k * (modal%U(I)*modal%C2(J+1,I) + modal%V(I)*modal%S2(J+1,I))
   end do

   A(N2+1+I,I) = RF%g + RF%k*RF%k * (modal%U(I)*AA+modal%V(I)*BB)
   A(N2+1+I,N2+2) = modal%U(I);
   A(N2+1+I,N1+N2+3) = -1
end do

if (RF%lorT == 0) then
! RF%k is unknown  N1+N2+5
   do I=1,N2+1! for both deep water and finite depth
      A(N2+1+I,N1+N2+5) = (modal%U(I) * (modal%U(I)-b_g(1)) + modal%V(I)**2) / RF%k
   end do
   if (RF%hdepth < infinite_depth) then
      do I=1,N2+1 ! for finite depth only
         AA=0.0_RP
         BB=0.0_RP
         sumTerm= 0.0_RP
         sumTerm2= 0.0_RP
         do J = 1,N1
            AA = AA + J**2  * b_g(J+1) *modal%C1(J+1,I)
            BB = BB + J**2  * b_g(J+1) *modal%S1(J+1,I)
            sumTerm = sumTerm   + J**2 *b_g(J+1) *modal%C2(J+1,I)*tanh(J*RF%k*RF%hdepth)
            sumTerm2 = sumTerm2 + J**2 *b_g(J+1) *modal%S2(J+1,I)*tanh(J*RF%k*RF%hdepth)
         end do
         A(N2+1+I,N1+N2+5) = A(N2+1+I,N1+N2+5) +                    &
         RF%k*modal%U(I) *( eta_g(I)*AA - RF%hdepth*sumTerm) +     &
         RF%k*modal%V(I) *( eta_g(I)*BB - RF%hdepth*sumTerm2)
        end do
    end if
end if
!
! Volume
A(N2+N2+3,1)    = 1
A(N2+N2+3,2:N2) = 2
A(N2+N2+3,N2+1) = 1
!
! Wave height
A(N2+N2+4,1)    =  1
A(N2+N2+4,N2+1) = -1
!
! Current
if (RF%current==1) then ! mass transport velocity
    A(N2+N2+5,N1+N2+6) = -1/RF%hdepth
    A(N2+N2+5,N2+2)    = 1
else ! eulerian current
    A(N2+N2+5,N2+2)    = 1
end if
A(N2+N2+5,N1+N2+4) = 1
!
! wavelength or period
if (RF%lorT == 1) then ! equation for RF%T is required
   A(N2+N2+6,N1+N2+4) = RF%k*RF%T
   A(N2+N2+6,N1+N2+5) = RF%k*RF%c
else ! equation for k is required
   A(N2+N2+6,N1+N2+4) = RF%k*RF%T
   A(N2+N2+6,N1+N2+5) = RF%c*RF%T
end if

end subroutine RF_calcA
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
subroutine WriteModal(modal, onFile)
!
type(Modal_type), intent(in) :: modal
logical, intent(in)          :: onFile

integer, parameter :: out_unit=20

if (onFile .EQV. .true.) then
   open (unit=out_unit,file="resultsModal.txt",action="write",status="replace")
end if

if (allocated (modal%U)) then
   write(out_unit,'(a,I4)') "modal%U allocated with size", size(modal%U)
   write(out_unit,'(f12.6)') modal%U
else
   write(out_unit,'(a,f10.4)') "modal%U not allocated"
end if

if (allocated (modal%V)) then
   write(out_unit,'(a,I4)') "modal%V allocated with size", size(modal%V)
   write(out_unit,'(f12.6)') modal%V
else
   write(out_unit,'(a,f10.4)') "modal%V not allocated"
end if

if (allocated (modal%C1)) then
   write(out_unit,'(a,I4)') "modal%C1 allocated with size", size(modal%C1)
   write(out_unit,'(f12.6)') modal%C1
else
   write(out_unit,'(a,f10.4)') "modal%C1 not allocated"
end if

if (allocated (modal%C2)) then
   write(out_unit,'(a,I4)') "modal%C2 allocated with size", size(modal%C2)
   write(out_unit,'(f12.6)') modal%C2
else
   write(out_unit,'(a,f10.4)') "modal%C2 not allocated"
end if

if (allocated (modal%S1)) then
   write(out_unit,'(a,I4)') "modal%S1 allocated with size", size(modal%S1)
   write(out_unit,'(f12.6)') modal%S1
else
   write(out_unit,'(a,f10.4)') "modal%S1 not allocated"
end if

if (allocated (modal%S2)) then
   write(out_unit,'(a,I4)') "modal%S2 allocated with size", size(modal%S2)
   write(out_unit,'(f12.6)') modal%S2
else
   write(out_unit,'(a,f10.4)') "modal%S2 not allocated"
end if

if (onFile .EQV. .true.) then
   close (out_unit)
endif

end subroutine WriteModal
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
subroutine DeallocateModal(modal)

type(Modal_type), intent(inout) :: modal

if (allocated (modal%C1)) deallocate (modal%C1)
if (allocated (modal%C2)) deallocate (modal%C2)
if (allocated (modal%S1)) deallocate (modal%S1)
if (allocated (modal%S2)) deallocate (modal%S2)

end subroutine DeallocateModal

end module modModal
