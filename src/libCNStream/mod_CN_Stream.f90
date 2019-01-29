  Module mod_CN_Stream
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

    Use modCNInitialize
    use modType
    USE iso_c_binding
    USE modReconstrucVol

    Use mfpGeneral

  Implicit None

    Type(RF_type)      :: RFwave
    type(Option_type)  :: optionwave

    Real(RP) :: expLimit
    Logical :: isHydroStaticPresussureInclude

  contains

    Subroutine initializeRF(filePath) &
    bind(c, name='__mod_cn_stream_MOD_initializerf')
      Implicit None
      character(kind=c_char, len=1), dimension(StringLength),intent(in) :: filePath

      !type(typDictionaryPtr) :: inputDict

      ! Example of dictionary use
      ! Call inputDict%initalize(filePath)
      !
      ! isHydroStaticPresussureInclude = inputDict%getLogicalOrDefault("includeHydroStatic",.false.)
      ! expLimit = inputDict%getRealOrDefault("exponentialLimit",3.0_RP)

      Call calcRF(filePath, RFwave, optionwave)

    End Subroutine


subroutine getCNStreamFlow(thetaincident, x, y, z, time, eta, u, v, w, pd,  &
   detadx,detady,detadt, dudx,dvdx,dwdx,dudy,dvdy,dwdy,dudz,dvdz,dwdz, &
   max_exp) &
    bind(c, name='__mod_cn_stream_MOD_getcnstreamflow')
    implicit none
    double precision, intent(in) :: thetaincident, x, y, z, time
    double precision, intent(out) :: eta, detadx,detady,detadt
    double precision, intent(out) :: u,v,w, dudx,dvdx,dwdx,dudy,dvdy,dwdy,dudz,dvdz,dwdz
    double precision, intent(out) :: pd
    double precision, optional, intent(in) :: max_exp

    type(Output_type) :: output

    Call Evaluate_HPUVW(RFwave, optionwave, x, y, z, time, thetaincident, .false., output, max_exp)

    eta = output%eta
    detadt = output%detadt
    detadx = output%detadx
    detady = output%detady

    u = output%Vx
    v = output%Vy
    w = output%Vz
    pd =output%pressure

    dudx = output%dVxdx
    dvdx = output%dVydx
    dwdz = output%dVzdx
    dudy = output%dVxdy
    dvdy = output%dVydy
    dwdy = output%dVzdy
    dudz = output%dVxdz
    dvdz = output%dVydz
    dwdz = output%dVzdz

end subroutine

    !subroutineEndTime(v2vIndex, endTime) &
    !    bind(c, name='__modgrid2grid_MOD_gethosendtime')
    !    implicit none
    !    integer, intent(in)           :: v2vIndex
    !    double precision, intent(out) :: endTime
    !    if (vol2vol_(v2vIndex)%isInitialized_) then
    !        endTime = dble(vol2vol_(v2vIndex)%endTime_)
    !    else
    !        write(*,*) "    [Warning] Grid2Grid is not initialized, Grid2Grid::getEndTime(endTime)"
    !    end if
    !end subroutine
    !
    !subroutine getHOSWaterDepth(v2vIndex, waterDepth) &
    !    bind(c, name='__modgrid2grid_MOD_gethoswaterdepth')
    !    implicit none
    !    integer, intent(in)           :: v2vIndex
    !    double precision, intent(out) :: waterDepth
    !    if (vol2vol_(v2vIndex)%isInitialized_) then
    !        waterDepth = dble(vol2vol_(v2vIndex)%waterDepth_)
    !    else
    !        write(*,*) "    [Warning] Grid2Grid is not initialized, Grid2Grid::getWaterDepth(waterDepth)"
    !    end if
    !end subroutine
    !
    !subroutine isGrid2GridInitialized(v2vIndex, isG2Initialized) &
    !    bind(c, name='__modgrid2grid_MOD_isgrid2gridinitialized')
    !    implicit none
    !    integer, intent(in)  :: v2vIndex
    !    logical, intent(out) :: isG2Initialized
    !    if (inpVol2Vol(v2vIndex)%isActive) then
    !        isG2Initialized = vol2vol_(v2vIndex)%isInitialized_
    !    else
    !        write(*,*) "    [Warning] isGrid2GridInitialized : wrong index is given., index : ", v2vIndex
    !        isG2Initialized = .FALSE.
    !    end if
    !end subroutine



  End Module
