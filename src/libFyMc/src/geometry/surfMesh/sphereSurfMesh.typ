!!*------------------------------------------------------------------*!!
!!  Project : mkSurfaceMesh
!!*------------------------------------------------------------------*!!
!!
!!  Fortran Type
!!      typSphereSurfMesh
!!
!!  Description
!!      typSphereSurfMesh Type
!!
!!  Authors
!!      YoungMyung Choi, Ecole Centrale de Nantes
!!          - youngmyung.choi@ec-nantes.fr
!!
!!*------------------------------------------------------------------*!!

!! ------------------------------------------------------------------ !!
!!                      Type typSphereSurfMesh
!! ------------------------------------------------------------------ !!

    Type,extends(typSurfMesh), public :: typSphereSurfMesh

!! Type Data -------------------------------------------------------- !!

    Private

        !! - Radius
        Real(RP) :: radius_

        !! - Nr, Nz
        Integer  :: nR_, nZ_

        !! - Origin
        Real(RP) :: x0_, y0_, z0_

!! Type Procedures -------------------------------------------------- !!

    contains

        !! Initialize Sphere Mesh
        procedure, pass, private :: initializeSphereMesh

        !! Initialize Sphere Mesh with Dictionary
        procedure, pass, private :: initializeSphereMeshDict

        !! Initialize Sphere Mesh with Dictionary
        generic :: initialize => initializeSphereMesh,      &
                                 initializeSphereMeshDict

    End Type

!! ------------------------------------------------------------------ !!
!!                      Type typSphereSurfMesh
!! ------------------------------------------------------------------ !!
