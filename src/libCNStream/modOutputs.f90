module modOutputs
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
use modReconstrucVol
!use mfpGlobal, only : StringLength
use mfpGeneral, only : typDictionaryPtr

contains

subroutine printSetupFile(input,option)
Implicit None
type(Input_type), intent(in) :: input
type(Option_type), intent(in) :: option

! input file for CN_Stream computation package
WRITE(*,*) "------Input------"
WRITE(*,*) "GeneralDimension ", input%GeneralDimension     ! Dimensional (=1) or Non-dimensional (=0)
WRITE(*,*) "GeneralDepth ", input%GeneralDepth     ! h if input%GeneralDimension=1 / h' (see report_R&F.pdf) if input%GeneralDimension=0
WRITE(*,*) "GeneralModes ", input%GeneralModes      ! Number of modes for first evaluation
WRITE(*,*) "WaveInput ", input%WaveInput            ! 0=period / 1=wavelength
WRITE(*,*) "WaveValue ", input%WaveValue             ! period or wavelength / be careful: if input%GeneralDimension=0 --> input%WaveValue is set to 1
WRITE(*,*) "WaveHeight ", input%WaveHeight       ! H if input%GeneralDimension=1 / H' (see report_R&F.pdf) if input%GeneralDimension=0
WRITE(*,*) "CurrentValue ",input%CurrentValue     ! value of current ; dimensional if input%GeneralDimension=1 / non-dimensional (see report_R&F.pdf) if input%GeneralDimension=1
WRITE(*,*) "CurrentType ", input%CurrentType          ! type of current ; 1 mass transport / 0 eulerian current


WRITE(*,*) "------Option------"
WRITE(*,*) "n_H ", option%n_H        ! Number of steps in wave height
WRITE(*,*) "err_type ",option%err_type        ! Error type / 0 absolute ; 1 relative
WRITE(*,*) "eps_err ",option%eps_err    ! Tolerance on the equations
WRITE(*,*) "err_max ", option%err_max     ! Divergence criteria
WRITE(*,*) "eps_inc ",option%eps_inc     ! Convergence criteria on unknowns
WRITE(*,*) "eps_N1 ",option%eps_N1       ! Decision criteria on the modes
WRITE(*,*) "itermax ",option%itermax       ! Maximum number of iterations
WRITE(*,*) "increment_type ",option%increment_type  ! Increment type for wave height / 0 linear ; 1 exponential
WRITE(*,*) "printonscreen ",option%printonscreen   ! Print on screen =1 / do not print on screen = 0
WRITE(*,*) "writeoutput ", option%writeoutput     ! Write output files =1 / do not Write output files = 0


end subroutine printSetupFile

subroutine buildOutputFile(option, namefileWoPath,outputFile)
type(Option_type), intent(in) :: option
character(len=*), intent(in) :: namefileWoPath
character(len=StringLength), intent(out) :: outputFile

character(len=StringLength) :: pathOutput
type(typDictionaryPtr) :: outputsDict

outputsDict = param_optionDict%subDict("Outputs")
pathOutput = outputsDict%getCharOrDefault("Path","./output/")
outputFile = trim(pathOutput)//trim(namefileWoPath)

end subroutine buildOutputFile


!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
subroutine WriteRF(RF, option, namefileWoPath)
!
type(RF_type), intent(in) :: RF
type(Option_type), intent(in) :: option
character(len=*) :: namefileWoPath
character(len=StringLength) :: outputFile
integer, parameter :: out_unit=20


call buildOutputFile(option, namefileWoPath,outputFile)
open (unit=out_unit,file=outputFile,action="write",status="replace")



write(out_unit, '(a,I7)')   "RF%dimen", RF%dimen       ! dimension
write(out_unit, '(a,I7)')   "RF%current", RF%current   ! current
write(out_unit, '(a,I7)')   "RF%lorT", RF%lorT         ! input wavelength (=1) or period (=0)
write(out_unit,'(a,f10.4)') "RF%g", RF%g               ! gravity acceleration

if (RF%hdepth > 100000) then
   write(out_unit,'(a)')       "RF%hdepth     inf"     ! depth
else
   write(out_unit,'(a,f10.4)') "RF%hdepth", RF%hdepth  ! depth
endif

write(out_unit,'(a,f10.4)') "RF%H", RF%H               ! wave height
write(out_unit,'(a,f10.4)') "RF%k", RF%k               ! wave number
write(out_unit,'(a,f10.4)') "RF%lambda", RF%lambda     ! wavelength
write(out_unit,'(a,f10.4)') "RF%T", RF%T               ! period
write(out_unit,'(a,f10.4)') "RF%U", RF%U               ! current value
write(out_unit,'(a,f10.4)') "RF%C", RF%C               ! phase velocity
write(out_unit,'(a,f10.4)') "RF%R", RF%R               ! Bernoulli's constant
write(out_unit,'(a,f10.4)') "RF%Q", RF%Q               ! volume flow rate

if (allocated (a_g)) then
   write(out_unit,'(a,I4)') "a_g allocated with size", size(a_g)
   write(out_unit,'(f12.6)') a_g
else
   write(out_unit,'(a,f10.4)') "a_g not allocated"
end if

if (allocated (b_g)) then
   write(out_unit,'(a,I4)') "b_g allocated with size", size(b_g)
   write(out_unit,'(f12.6)') b_g
else
   write(out_unit,'(a,f10.4)') "b_g not allocated"
end if

if (allocated (eta_g)) then
   write(out_unit,'(a,I4)') "eta_g allocated with size", size(eta_g)
   write(out_unit,'(f12.6)') eta_g
else
   write(out_unit,'(a,f10.4)') "eta_g not allocated"
end if

if (allocated (slope_g)) then
   write(out_unit,'(a,I4)') "slope_g allocated with size", size(slope_g)
   write(out_unit,'(f12.6)') slope_g
else
   write(out_unit,'(a,f10.4)') "slope_g not allocated"
end if


close (out_unit)


end subroutine WriteRF
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
subroutine RF_write_waverfcof(RF, option)
!
type(RF_type), intent(in)     :: RF
type(Option_type), intent(in) :: option

integer, parameter :: out_unit=13
integer  :: J
real(RP) :: XL_dim, XT_dim , XV_dim, C_S

character(len=StringLength) :: outputFile

XL_dim = 1.0_RP
XT_dim = 1.0_RP
XV_dim = XL_dim / XT_dim

call buildOutputFile(option, "waverf.cof",outputFile)
open(out_unit,file=outputFile,access="SEQUENTIAL",form="FORMATTED")

C_S = RF%c_S + RF%c

write(out_unit,*) DBLE(2*PI/RF%k) * XL_dim, DBLE(RF%H) * XL_dim, DBLE(RF%k) / XL_dim,&
   DBLE(RF%T)*XT_dim, DBLE(RF%C)*XV_dim,DBLE(C_S)*XV_dim, DBLE(RF%c_E)*XV_dim, &
   option%N1+1,option%N2+1, DBLE(RF%R)*(XV_dim)**2, DBLE(RF%hdepth)*XL_dim
J=1

write(out_unit,*)J,DBLE(b_g(J)) * XV_dim
do J=2,option%N1+1
   write(out_unit,*)J,DBLE(b_g(J)) * XL_dim * XV_dim
enddo

do J=1,option%N2+1
   write(out_unit,*)J,DBLE(a_g(J)) * XL_dim
enddo
close(out_unit)


call buildOutputFile(option, "waverf.DAT",outputFile)
open(out_unit,file=outputFile)
do J=1,option%N1+1
   write(out_unit,*)J,ABS(DBLE(b_g(J)))
enddo
do J=1,option%N2+1
   write(out_unit,*)J,ABS(DBLE(a_g(J)))
enddo
close(out_unit)

end subroutine RF_write_waverfcof

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

subroutine WriteOutput(output, onFile, option)
Implicit None
integer, parameter :: out_unit=20

type(Output_type), intent(in) :: output
logical, intent(in)           :: onFile
type(Option_type), intent(in) :: option
character(len=StringLength) :: outputFile

if (onFile .EQV. .true.) then
	call buildOutputFile(option, "resultsOutput.txt",outputFile)
   open (unit=out_unit,file=outputFile,action="write",status="replace")
end if

WRITE(out_unit,*) output%eta
WRITE(out_unit,*) output%pressure
WRITE(out_unit,*) output%Vx, output%Vy, output%Vz
WRITE(out_unit,*) output%detadt, output%detadx, output%detady
WRITE(out_unit,*) output%dVxdx, output%dVxdy, output%dVxdz
WRITE(out_unit,*) output%dVydx, output%dVydy, output%dVydz
WRITE(out_unit,*) output%dVzdx, output%dVzdy, output%dVzdz

if (onFile .EQV. .true.) then
   close (out_unit)
endif

end subroutine WriteOutput

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
subroutine TecplotOutput_Modes(RF,option)
! Create tecplot outputs of the velocity and pressure field under the wave
type(RF_type), intent(inout)      :: RF
type(Option_type), intent(in)  :: option
integer, parameter :: out_unit=10
integer            :: i
character(len=StringLength) :: outputFile

call buildOutputFile(option, "Modes_CN_Stream.dat",outputFile)

open (unit=out_unit,file=outputFile,action="write",status="replace")

WRITE(out_unit,'(A)')'TITLE=" Modal description "'
WRITE(out_unit,'(A)') 'VARIABLES="k","Modal amplitude (LOG)"'
WRITE(out_unit,103)'ZONE T = ','"eta"',', I=',option%N2
DO i=1,option%N2
	WRITE(out_unit,102) (i-1)*RF%k, LOG10(MAX(EPSILON(1.0_rp),ABS(a_g(i))))
ENDDO
WRITE(out_unit,103)'ZONE T = ','"phi"',', I=',option%N1
DO i=1,option%N1
	WRITE(out_unit,102) (i-1)*RF%k, LOG10(MAX(EPSILON(1.0_rp),ABS(b_g(i))))
ENDDO
103 FORMAT(A,A,A,I5,A,I5)
102 FORMAT(2(ES12.5,X))
end subroutine TecplotOutput_Modes
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
subroutine TecplotOutput_VelocityPressure(RF,option, y, time, theta)
! Create tecplot outputs of the velocity and pressure field under the wave
type(RF_type), intent(inout)      :: RF
type(Option_type), intent(in)  :: option

type(Output_type) :: output
type(typDictionaryPtr) :: outputsDict

integer, parameter :: out_unit=11
integer, parameter :: nz=20
integer            :: i1,i2

!real(rp), dimension (:), allocatable :: xvect, zvect
real(rp) :: xlocal,zlocal,y,time,theta,zmin
real(rp) :: eta,detadx,detadt

character(len=StringLength) :: outputFile

call buildOutputFile(option, "VP_card_fitted.dat",outputFile)



!allocate(xvect(2*option%N2), zvect(2*option%N2))
!y     = 0.0_rp
!time  = 0.0_rp
!theta = 0.0_rp
!
! Definition of zmin dependent on the test case
if (RF%k*RF%hdepth.GT.3.0_rp) then
    zmin = -pi/RF%k
else
    zmin = -RF%hdepth
endif

open (unit=out_unit,file=outputFile ,action="write",status="replace")

write(out_unit,'(A)') 'TITLE =" Velocity and pressure field "'
write(out_unit,'(A)') 'VARIABLES="x","z","eta","vitx","vitz","Press"'
WRITE(out_unit,103)'ZONE T = "',time,'", I=', 2*option%N2,', J=', nz
!
! Definition of the z vector, we use a boundary-fitted mesh
DO i2=1,nz
    DO i1=1,2*option%N2
        xlocal = (i1-1-option%N2)*pi/RF%k/option%N2
        !
        call Evaluate_hincidentRF(RF, option, xlocal, y, time, theta, eta, detadx, detadt)
        ! Linear description of zvect
        !zvect(ii1,ii2)=zmin+(-zmin+eta)*REAL(i2-1,RP)/REAL(nz-1,RP)
        ! Stretched grid of zvect
        zlocal = zmin+(-zmin+eta)*SIN(pio2*REAL(i2-1,RP)/REAL(nz-1,RP))
        call Evaluate_HPUVW(RF, option, xlocal, y, zlocal, time, theta, .FALSE., output)
        !
        write(out_unit,102) xlocal,zlocal,output%eta,output%Vx,output%Vz,output%pressure
    ENDDO
ENDDO
!
103 FORMAT(A,F9.2,A,I5,A,I5)
102 FORMAT(6(ES12.5,X))

end subroutine TecplotOutput_VelocityPressure

subroutine TecplotOutput_FreeSurface(RF,option, y, time, theta)
! Create tecplot outputs of the free surface elevation and its derivatives
type(RF_type), intent(inout)      :: RF
type(Option_type), intent(in)  :: option

integer, parameter :: out_unit=12
integer            :: i1,i2

!real(rp), dimension (:), allocatable :: xvect, zvect
real(rp) :: xlocal,y,time,theta,zmin
real(rp) :: eta,detadx,detadt

character(len=StringLength) :: outputFile

call buildOutputFile(option, "FreeSurface_CN_Stream.dat",outputFile)

open (unit=out_unit,file=outputFile,action="write",status="replace")

write(out_unit,'(A)') 'TITLE =" Free surface description "'
write(out_unit,'(A)') 'VARIABLES="x","eta","etax","etat"'
WRITE(out_unit,103)'ZONE T = "',time,'", I=', 2*option%N2
!

    DO i1=1,2*option%N2
        xlocal = (i1-1-option%N2)*pi/RF%k/option%N2
        !
        call Evaluate_hincidentRF(RF, option, xlocal, y, time, theta, eta, detadx, detadt)
        !
        write(out_unit,102) xlocal,eta,detadx,detadt
    ENDDO
!
103 FORMAT(A,F9.2,A,I5)
102 FORMAT(4(ES12.5,X))

end subroutine TecplotOutput_FreeSurface
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------


end module modOutputs
