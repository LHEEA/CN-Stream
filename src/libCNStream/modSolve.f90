module modSolve
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
use modModal
use modMatrix
use modUtils
use HOS_modlinear_wave
use modReconstruction
use modSetupNameList

implicit none

type, public :: Iter_type
    integer  :: N_iter
    real(RP) :: F_max
    real(RP) :: DY_max
    integer :: exit_type
end type Iter_type

contains

subroutine  RF_solve_auto(RF, Option)
!
! RF_solve_auto gives Rienecker and Fenton solution
! RF = RF_solve_auto(RF, display, N2, n_H)
!
! Description
! The RF_solve_auto function is part of Riencker and Fenton solution package.
! it is based on paper by M.M. Rienecker and J.D. Fenton,
! A Fourier approximation method for steady water waves.
! J. Fluid Mech., Vol. 104, pp. 119-137, 1981.
!
! Input
! RF structure
! display       1 or 0 when intermediate display is required or not
! N2            (optional) number of modes for eta
! n_H           (optional) number of intermediate steps in wave height
!
! implemented by Pierre FERRANT - SIREHNA - MARS 1993
! modified by F�licien BONNEFOY - LHEEA - 2015
!
! See also RF_solve, RF_calcuvcs, RF_calcer, RF_calcA.
!
type(Option_type), intent(inout) :: option
type(RF_type), intent(inout)     :: RF
type(iter_type)  :: iter

integer  :: auto_N2, i1
real(RP) :: H_target, H_start, dH

! character(len=StringLength) :: outputFile ! Only for debug

auto_N2 = 1;
option%N2 = max(option%N1+5, 2*option%N1)
!
! wave height increments
H_target = RF%H

if (option%increment_type == 1)  then ! exponential
   H_start  = 0.01 * H_target
   dH = (exp(1.0_RP)-1.0_rp) / option%n_H
else ! linear
   H_start  = H_target / (option%n_H+1)
   dH = H_start;
end if
!
! initial solution
CALL RF_initial_solution(RF, option, H_start)
!
! Loop on wave heights
do i1=1,(Option%n_H + 1)
   if (Option%n_H==0) then
      RF%H = H_target
   else
      if (option%increment_type == 1)  then
         RF%H = H_start + (H_target-H_start)*log(1+(i1-1)*dH)
      else
         RF%H = H_start + (i1-1)*dH
      end if
   end if

   option%modes = 0

   do while (option%modes < 1)

      call RF_solve_iterate(RF, option, iter)
      if (option%printonscreen==1) then
        WRITE(*,*) "it", i1, " Solved height", RF%H, " target is",  H_target
        print*, 'corresponding H/lambda=', RF%H*RF%k/(2.0_rp*pi)
      endif
      call RF_calc_a(RF, option)
      call RF_decide(RF, option, iter)

      !if (option%printonscreen==1) then
      !    call buildOutputFile("DetailedResults.txt",outputFile)
      !    call WriteRF(RF,outputFile)
      ! endif

      ! recompute useful quantities on refined modes
      ! necessary for correct size of dynamic tables (eta and slope)
      call RF_calc_eta(RF, option)
      call RF_calc_slope(RF, option)

   end do
end do

end subroutine RF_solve_auto
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
subroutine RF_initial_solution(RF, Option, H)
!
! RF_initial_solution gives second-order solution
! RF = RF_initial_solution(RF, H_start)
!
! Description
! The RF_initial_solution function is part of Riencker and Fenton solution package.
! it is based on paper by M.M. Rienecker and J.D. Fenton,
! A Fourier approximation method for steady water waves.
! J. Fluid Mech., Vol. 104, pp. 119-137, 1981.
!
! Input
! RF structure which contains 7 parameters
!   RF.g        dimensional gravity acceleration
!   RF.h        dimensional depth
!   RF.H        dimensional wave height
!   RF.N1       number of modes for stream function
!   RF.N2       number of modes for wave elevation
!   RF.lorT     input wavelength (=1) or period (=0)
!   RF.lambda or RF.T wavelength or period
!
! H             initial wave height
!
! Output
! RF structure with 7 more parameters
! RF.k      wavenumber
! RF.c      phase velocity
! RF.eta    free surface elevation
! RF.a      free surface modes
! RF.b      potential modes
! RF.R      Bernoulli constant
! RF.Q      mass flow
! RF.slope  wave slope
!
! implemented by Pierre FERRANT - SIREHNA - MARS 1993
! modified by F�licien BONNEFOY - LHEEA - 2015
!
! See also RF_solve, RF_solve_iterate, RF_solve_auto, RF_calcuvcs, RF_calcer, RF_calcA.
!
type(RF_type), intent(inout)  :: RF
type(Option_type), intent(in) :: option

real(RP), intent(in)  :: H
real(RP) :: sigma

if (allocated(a_g)) then
    deallocate(a_g)
end if
allocate(a_g(option%N2+1))

if (allocated(b_g)) then
    deallocate(b_g)
end if
allocate(b_g(option%N1+1))

a_g = 0.0_RP
b_g = 0.0_RP
!
!   second-order solution
if (RF%hdepth > infinite_depth) then
    sigma = 1
else
    sigma = tanh(RF%k * RF%hdepth)
end if

a_g(2) = H / 2.0_RP
a_g(3) = RF%k * a_g(2)**2 * (3.0_RP - sigma**2) / (4.0_RP * sigma**3)
!
! build potential modes
b_g(1) = - RF%c
b_g(2) = RF%g * a_g(2) / (RF%k * RF%c)
b_g(3) = 3.0_RP/4.0_RP * RF%k * a_g(2)**2 * (1.0_RP - sigma**4) / sigma**3
RF%Q = 0.0_RP
RF%R = RF%c**2/2.0_RP
!
! storing initial solution
call RF_calc_eta(RF, option)

end subroutine RF_initial_solution
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
subroutine RF_decide(RF,option,iter)

type(RF_type), intent(inout) :: RF
type(Option_type), intent(inout) :: option
type(iter_type), intent(inout) :: iter

integer :: idx_zero

!check that we don't have multiple maxima in eta_g
idx_zero = findFirstValueOverTheshold(-eta_g, .true., 0.0_rp)
if (maxval(eta_g(idx_zero:option%N2)) > 0.0_rp) then
    WRITE(*,*) "Starts to solve multiple wavelengths... Computations stops"
    WRITE(*,*) "Increase of n_H may solve the problem, current value n_H =",option%n_H
    STOP
endif

call RF_calc_slope(RF, option)

! find the useful modes
! Relative errors
if (option%err_type == 1) then
    option%N1_eff =  findFirstValueOverTheshold(abs(b_g(2:   )/maxval(abs(b_g(2:)))), .false., option%eps_N1) +1
    option%N2_eff =  findFirstValueOverTheshold(abs(a_g(2:   )/maxval(abs(a_g(2:)))), .false., option%eps_N1) +1
else ! absolute error
    option%N1_eff =  findFirstValueOverTheshold(abs(b_g(2:   )), .false., option%eps_N1) +1
    option%N2_eff =  findFirstValueOverTheshold(abs(a_g(2:   )), .false., option%eps_N1) +1
endif

if (option%printonscreen==1) then
  WRITE(*,*) "N1", option%N1, "N2", option%N2, "N1_eff", option%N1_eff, "N2_eff", option%N2_eff
endif

! decide what to do
if (iter%exit_type == DIVERGENCE) then
    if (option%printonscreen==1) then
      WRITE(*,*) "last it did not converged"
    endif
    option%modes = 1 ! quit the while loop (last solution was not converged)
elseif (option%N1_eff < 0.4*option%N1) then
    ! mode numbers are too large
    option%modes = -1
    option%N1 = option%N1 - 5
    if (option%printonscreen==1) then
      WRITE(*,*) "Reduce modes"
    endif
elseif (option%N1_eff < option%N1) then
    ! mode numbers are correct
    option%modes = 1 ! quit the while loop
!~         WRITE(*,*) "That's ok"
else ! increase the number of modes
    option%modes = -2
    option%N1 = option%N1 + 5
    if (option%printonscreen==1) then
      WRITE(*,*) "Increase modes"
    endif
end if

if (option%modes < 1) then
    option%N2 = floor(option%N1 * (1.5 + maxval(abs(slope_g)) / 0.3*1.5))  !!!! pb ici
    if (option%printonscreen==1) then
      WRITE(*,*) "modes modified", option%modes, " N1", option%N1, " N2",  option%N2
    endif
    call zeropad(b_g, option%N1+1)
    call zeropad(a_g, option%N2+1)
end if
! Relative errors
if (option%err_type == 1) then
    option%N1_eff =  findFirstValueOverTheshold(abs(b_g(2:)/maxval(abs(b_g(2:)))), .true., option%eps_N1)
    option%N2_eff =  findFirstValueOverTheshold(abs(a_g(2:)/maxval(abs(a_g(2:)))), .true., option%eps_N1)
else ! absolute error
    option%N1_eff =  findFirstValueOverTheshold(abs(b_g(2:)), .true., option%eps_N1)
    option%N2_eff =  findFirstValueOverTheshold(abs(a_g(2:)), .true., option%eps_N1)
endif

end subroutine RF_decide
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
subroutine  RF_solve_iterate(RF, option, iter)
!
! RF_solve_iterate gives Rienecker and Fenton solution
! RF = RF_solve_iterate(RF, display, N2, n_H)
!
! Description
! The RF_solve_iterate function is part of Riencker and Fenton solution package.
! it is based on paper by M.M. Rienecker and J.D. Fenton,
! A Fourier approximation method for steady water waves.
! J. Fluid Mech., Vol. 104, pp. 119-137, 1981.
!
! Input
! RF structure which contains 4 parameters
!   RF.h        dimensional depth
!   RF.H        dimensional wave height
!   RF.N1       number of modes for stream function
!   RF.lorT     input wavelength (=1) or period (=0)
!   RF.lambda or RF.T wavelength or period
!
! display       1 or 0 when intermediate display is required or not
! N2            (optional) number of modes for eta
! n_H           (optional) number of intermediate steps in wave height
!
! implemented by Pierre FERRANT - SIREHNA - MARS 1993
! modified by F�licien BONNEFOY - LHEEA - 2015
!
! See also RF_solve, RF_solve_auto, RF_calcuvcs, RF_calcer, RF_calcA.
!
type(RF_type), intent(inout)     :: RF
type(Option_type), intent(inout) :: option
type(iter_type), intent(inout)   :: iter
!
type(Modal_type) :: modal
!
! character(len=StringLength) :: outputFile
! initialisation
iter%N_iter = 0
iter%F_max  = 1
iter%DY_max = 1
!
do while (iter%F_max > option%eps_err .and. iter%DY_max > option%eps_inc .and. iter%N_iter < option%itermax)
    !
    ! find new solution
    call RF_solve(RF, option, iter, modal)
    !
    ! wave elevation, Fourier components
    call RF_calc_a(RF, option)
    !
    ! wave slope
    call RF_calc_slope(RF, option)
    !
    !
    iter%N_iter = iter%N_iter+1
    !
    ! for screen display (in RF_decide)
    if (iter%F_max > option%err_max ) then
        iter%exit_type = DIVERGENCE
        exit
    end if
    if (iter%DY_max <= option%eps_inc) then
        iter%exit_type = MIN_DY
        exit
    end if
    iter%exit_type =NORMAL
end do

end subroutine RF_solve_iterate
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
subroutine RF_solve(RF, option, iter, modal)
!
! RF_solve solve the equations in the Rienecker and
! Fenton computation
! [RF, F_max] = RF_solve(RF)
!
! Description
! The RF_solve function is part of Riencker and Fenton solution package.
! it is based on paper by M.M. Rienecker and J.D. Fenton,
! A Fourier approximation method for steady water waves.
! J. Fluid Mech., Vol. 104, pp. 119-137, 1981.
!
! implemented by Pierre FERRANT - SIREHNA - MARS 1993
! modified by F�licien BONNEFOY - LHEEA - 2015
!
! See also RF_calcA, RF_calcuvcs, RF_calcer.
!
!
type(RF_type), intent(inout)     :: RF
type(Option_type), intent(inout) :: option
type(iter_type), intent(inout)   :: iter
type(Modal_type), intent(inout)  :: modal

real(RP), dimension(:,:), allocatable :: A
real(RP), dimension(:), allocatable   :: F
real(RP), dimension(:), allocatable   :: DY, err_scale

integer :: I
!
! calculation of auxiliary tables
modal = RF_calcuvcs(RF, option)
!
! calculation of error vectors
call RF_calcer(RF, option, modal,F)

iter%F_max = maxval(abs(F))
!
! Calculation of derivatives matrix necessary for Newton's method
call RF_calcA(RF, option, modal,A);
!
! Calculation of correction and new estimate
allocate(DY(size(A,2)))
call leastSquareInv(size(A,1),size(A,2),A,F,DY)
DY=-DY
!
iter%DY_max = maxval(abs(DY))
!
! Redistribute the solution on natural variables
eta_g(1:option%N2+1) = eta_g(1:option%N2+1) + DY(1:option%N2+1)
b_g(1:option%N1+1)   = b_g(1:option%N1+1)   + DY((option%N2+1+1) : (option%N2+1+option%N1+1))
RF%R = RF%R + DY(option%N1+option%N2+3)
RF%c = RF%c + DY(option%N1+option%N2+4)

if (RF%lorT == 1) then
   RF%T = RF%T + DY(option%N1+option%N2+5)
else
   RF%k = RF%k + DY(option%N1+option%N2+5)
end if
RF%Q = RF%Q + DY(option%N1+option%N2+6)
!
! Relative errors
if (option%err_type == 1) then
   !
   ! For F error
   ALLOCATE(err_scale(option%N2+option%N2+6))
   !
   do I = 1,option%N2+1
      err_scale(I) = max(max(abs(sum(b_g(2:option%N1+1) * modal%C1(2:option%N1+1,I))), &
                        abs(b_g(1) * eta_g(I))),abs(RF%Q))
   enddo
   !
   err_scale((option%N2+1+1) : (option%N2+1+option%N2+1)) = RF%g*maxval(abs(eta_g(1:option%N2+1)))
   err_scale(option%N2+option%N2+3) = maxval(abs(eta_g(1:option%N2+1)))
   err_scale(option%N2+option%N2+4) = RF%H
   err_scale(option%N2+option%N2+5) = RF%c
   err_scale(option%N2+option%N2+6) = 2*pi
   !
   iter%F_max = maxval(abs(F/err_scale))
   !
   DEALLOCATE(err_scale)
   !
   ! For DY error
   ALLOCATE(err_scale(option%N1+option%N2+6))
   !
   err_scale(1:option%N2+1) = maxval(abs(eta_g(1:option%N2+1))) !maxval on the array or not?
   err_scale((option%N2+1+1) : (option%N2+1+option%N1+1)) = maxval(abs(b_g(1:option%N1+1))) !maxval on the array or not?
   err_scale(option%N1+option%N2+3) = abs(RF%R)
   err_scale(option%N1+option%N2+4) = RF%c
   if (RF%lorT == 1) then
       err_scale(option%N1+option%N2+5) = RF%T
   else
      err_scale(option%N1+option%N2+5) = RF%k
   end if
   err_scale(option%N1+option%N2+6) = abs(RF%Q)
   !
   iter%DY_max = maxval(abs(DY/err_scale))
   !
   DEALLOCATE(err_scale)
endif

end subroutine RF_solve
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
end module modSolve
