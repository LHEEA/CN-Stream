!!*------------------------------------------------------------------*!!
!!  Project : mkSurfaceMesh
!!*------------------------------------------------------------------*!!
!!
!!  Fortran Type
!!      typParaPipeMesh
!!
!!  Description
!!      typParaPipeMesh Type
!!
!!  Authors
!!      YoungMyung Choi, Ecole Centrale de Nantes
!!          - youngmyung.choi@ec-nantes.fr
!!
!!*------------------------------------------------------------------*!!

!! ------------------------------------------------------------------ !!
!!                      Type typParaPipeMesh
!! ------------------------------------------------------------------ !!

    type,extends(typSurfMesh), public :: typParaPipeMesh

!! Type Data -------------------------------------------------------- !!

    private

        !! - Length, Width, Depth
        Real(RP) :: Lx, Ly, Lz

        !! - Nx, Ny, Nz
        Integer  :: Nx, Ny, Nz

        !! - Origin
        Real(RP) :: x0, y0, z0

        !! - Ratio to Increment
        Real(RP) :: rX, rY, rZ

        !! - Face End Node Index
        Integer :: nNodeP1, nNodeP2, nNodeP3, nNodeP4, nNodeP5, nNodeP6

!! Type Procedures -------------------------------------------------- !!

    contains

        !! - Initialze
        procedure, pass, private :: initializeParaPipeMesh

        !! - initialize with dictionary
        procedure, pass, private :: initializeParaPipeMeshDict

        !! - Initialze
        generic :: initialize => initializeParaPipeMesh, &
                                 initializeParaPipeMeshDict

    end type

!! ------------------------------------------------------------------ !!
!!                      Type Point
!! ------------------------------------------------------------------ !!
