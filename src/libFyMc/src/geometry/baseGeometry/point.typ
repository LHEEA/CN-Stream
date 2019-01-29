!!*------------------------------------------------------------------*!!
!!  Project : mkSurfaceMesh
!!*------------------------------------------------------------------*!!
!!
!!  Fortran Type
!!      point
!!
!!  Description
!!      Point Type
!!
!!  Authors
!!      YoungMyung Choi, Ecole Centrale de Nantes
!!          - youngmyung.choi@ec-nantes.fr
!!
!!*------------------------------------------------------------------*!!

!! ------------------------------------------------------------------ !!
!!                      Type Point
!! ------------------------------------------------------------------ !!

    type, public :: typPoint

!! Type Data -------------------------------------------------------- !!

    private

        !! Point Index
        integer  :: idx_        

        !! Pointer Vector
        real(RP), dimension(3) :: vec_

!! Type Procedures -------------------------------------------------- !!

    contains

        !! Set Point
        procedure, pass, public :: set => setPoint

        !! Move Point Position
        procedure, pass, public :: move => movePoint

        !! Return x - Position
        procedure, pass, public :: x => pointX

        !! Return y - Position
        procedure, pass, public :: y => pointY

        !! Return z - Position
        procedure, pass, public :: z => pointZ

        !! Return Position Vector
        procedure, pass, public :: vec => pointVec

    End Type

!! ------------------------------------------------------------------ !!
!!                      Type Point
!! ------------------------------------------------------------------ !!
