!
MODULE modCNS
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
use iso_c_binding
implicit none

contains
  subroutine calcRF(C_ConfigFile, RF, option) &
  bind(c, name='__modcns_MOD_calcrf')

  use modCNInitialize
  use modSetupNameList
  use modSolve
  use modReconstrucVol
  use modOutputs

    character(kind=c_char, len=1), Dimension(StringLength), intent(in) :: C_ConfigFile
    character(StringLength) :: ConfigFile
    type(RF_type), intent(out)   :: RF
    type(Option_type), intent(out)  :: option

    type(Input_type)  :: input
    integer :: i

    ConfigFile = ''
    do i = 1, StringLength
      if (C_ConfigFile(i) == c_null_char ) then
          exit
      else
        ConfigFile(i:i) = C_ConfigFile(i)
      end if
    enddo
    !
    ! Define input parameters from a file
    call setupFromFymcFile(ConfigFile, input, option)
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
  end subroutine calcRF

  subroutine initAiry(C_ConfigFile, RF, option) &
    bind(c, name='__modcns_MOD_initairy')

    use modCNInitialize
    use modSetupNameList
    use modSolve
    use modReconstrucVol
    use modOutputs

      character(kind=c_char, len=1), Dimension(StringLength), intent(in) :: C_ConfigFile
      character(StringLength) :: ConfigFile
      type(RF_type), intent(out)   :: RF
      type(Option_type), intent(out)  :: option

      type(Input_type)  :: input
      integer :: i

      ConfigFile = ''
      do i = 1, StringLength
        if (C_ConfigFile(i) == c_null_char ) then
            exit
        else
          ConfigFile(i:i) = C_ConfigFile(i)
        end if
      enddo
      !
      ! Define input parameters from a file
      call setupFromFymcFile(ConfigFile, input, option)
      !
      ! Initialize all necessary values
      call RF_initialize(input, RF, option)
      !
  end subroutine initAiry



  subroutine recRF(RF, option, x, y, z, time, thetaincident, C_hydrostatic, output, max_exp) &
    bind(c, name='__modcns_MOD_recrf')

  use modCNInitialize
  use modSetupNameList
  use modSolve
  use modReconstrucVol
  use modOutputs

  Implicit None

    ! Recompute wave elevation, pressure, velocity and its derivatives
    type(RF_type), intent(inout)   :: RF
    type(Option_type), intent(in)  :: option
    real(c_double),  intent(in)          :: x, y, z !have to be dimensional
    real(c_double), intent(in)           :: time, thetaincident !,x, y, z have to be dimensional
    type(Output_type), intent(out) :: output
    real(c_double),  intent(in),optional         :: max_exp
    logical(c_bool), intent(in)            :: C_hydrostatic
    logical :: hydrostatic
    logical :: WriteAscii = .FALSE.

    if(C_hydrostatic) THEN
      hydrostatic = .TRUE.
    else
      hydrostatic = .False.
    endif
    !real(RP) :: U, dUdx, dWdx
    !real(RP) :: xoblique, detax

    ! Check the evaluation at a given location
    call Evaluate_HPUVW(RF, option, x, y, z, time, thetaincident, hydrostatic, output)
    if (WriteAscii) call WriteOutput(output, .TRUE., option)
    !
    ! Tecplot output of the solution for visual checking
    ! Modal description of eta and phi
    !!!call TecplotOutput_Modes(RF,option)
    ! Velocity and pressure fields
    !!!call TecplotOutput_VelocityPressure(RF,option)

  end subroutine recRF


  subroutine airy(RF, option, x, y, z, time, thetaincident, hydrostatic, output, max_exp) &
    bind(c, name='__modcns_MOD_airy')
    use modCNInitialize
    use modSetupNameList
    use modSolve
    use modReconstrucVol
    use modOutputs

    type(RF_type), intent(inout)   :: RF
    type(Option_type), intent(in)  :: option
    real(c_double),  intent(in)          :: x, y, z !have to be dimensional
    real(c_double), intent(in)           :: time, thetaincident !,x, y, z have to be dimensional
    type(Output_type), intent(out) :: output
    real(c_double),  intent(in),optional         :: max_exp
    logical(c_bool), intent(in)            :: hydrostatic
    logical :: WriteAscii = .FALSE.

    real(RP) ::omega,expkz, coshkzd, coshkd, sinhkzd, sinhkd, agk_w, agkk_w, aw, awk, rag, ak, coskxwt, sinkxwt, costheta, sintheta
    real(RP) :: U,W,ETA,P,dUdX,dUdZ,dWdX,dWdZ,dETAdt,dETAdX
    real(c_double) :: z_hydrostatic

    omega = (2*pi/RF%T)
    costheta = COS(thetaincident)
    sintheta = SIN(thetaincident)

    if (hydrostatic) then
      z_hydrostatic = 1.0
    else
      z_hydrostatic = 0.0
    end if

    if(RF%k*RF%hdepth < 10) THEN
      coshkzd = COSH(RF%k*(z-z_hydrostatic+RF%hdepth))
      coshkd =COSH(RF%k*(RF%hdepth))
      sinhkzd = sinH(RF%k*(z-z_hydrostatic+RF%hdepth))
      sinhkd =sinH(RF%k*(RF%hdepth))
    ELSE
      expkz = EXP(RF%k*(z-z_hydrostatic))
    END IF

    agk_w=RF%H/2_rp*RF%g*RF%k/omega
    agkk_w = RF%k*agk_w
    aw = RF%H/2_rp*omega
    awk = aw*RF%k
    rag = RF%H/2_rp*RF%g
    ak = RF%H/2_rp*RF%k

    coskxwt = COS(RF%k*(x*costheta+y*sintheta)-omega*time)
    sinkxwt = SIN(RF%k*(x*costheta+y*sintheta)-omega*time)

    if(RF%k*RF%hdepth < 10) THEN
      U = agk_w*coshkzd/coshkd*coskxwt
      W=aw*sinhkzd/sinhkd*sinkxwt
      P=rag*coshkzd/coshkd*coskxwt
      ETA=RF%H/2_rp*coskxwt
      dUdX = -agkk_w*coshkzd/coshkd*sinkxwt
      dUdZ = agkk_w*sinhkzd/coshkd*coskxwt
      dWdX = agkk_w*sinhkzd/sinhkd*coskxwt
      dWdZ = agkk_w*coshkzd/sinhkd*sinkxwt
    ELSE
      U = agk_w*expkz*coskxwt
      W=aw*expkz*sinkxwt
      P=rag*expkz*coskxwt
      ETA=RF%H/2_rp*coskxwt
      dUdX = -agkk_w*expkz*sinkxwt
      dUdZ = agkk_w*expkz*coskxwt
      dWdX = agkk_w*expkz*coskxwt
      dWdZ = agkk_w*expkz*sinkxwt
    END IF

    dETAdt = aw*sinkxwt
    dETAdX = -ak*sinkxwt


    output%eta = ETA
    output%pressure = P
    output%Vx = U*costheta
    output%Vy = U*sintheta
    output%Vz = W

    output%detadt = dETAdt
    output%detadx = dETAdX*costheta
    output%detady = dETAdX*sintheta
    output%dVxdx = dUdX*costheta*costheta
    output%dVxdy = dUdX*costheta*sintheta
    output%dVxdz = dUdZ*costheta
    output%dVydx = dUdX*costheta*sintheta
    output%dVydy = dUdX*sintheta*sintheta
    output%dVydz = dUdX*sintheta
    output%dVzdx = dWdX*costheta
    output%dVzdy = dWdX*sintheta
    output%dVzdz = dWdZ

    if (WriteAscii) call WriteOutput(output, .TRUE., option)

  end subroutine airy
end module modCNS
