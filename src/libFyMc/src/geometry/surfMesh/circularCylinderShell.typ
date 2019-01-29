!!*------------------------------------------------------------------*!!
!!  Project : mkSurfaceMesh
!!*------------------------------------------------------------------*!!
!!
!!  Fortran Type
!!      typCircularCylinderShellSurfMesh
!!
!!  Description
!!      typHemiSphereSurfMesh Type
!!
!!  Authors
!!      YoungMyung Choi, Ecole Centrale de Nantes
!!          - youngmyung.choi@ec-nantes.fr
!!
!!*------------------------------------------------------------------*!!

!! ------------------------------------------------------------------ !!
!!                      Type typCircularCylinderShellSurfMesh
!! ------------------------------------------------------------------ !!

    Type,extends(typSurfMesh), public :: typCircularCylinderShellSurfMesh

!! Type Data -------------------------------------------------------- !!

    Private

        !! - Radius
        Real(RP) :: radius_

        !! - Height
        Real(RP) :: height_

        !! - Nr, Nz
        Integer  :: nR_, nZ_

        !! - meshRatio
        Real(rp) :: rZ_

        !! - Origin
        Real(RP) :: x0_, y0_, z0_

!! Type Procedures -------------------------------------------------- !!

    contains

        !! Initialize Sphere Mesh
        procedure, pass, private :: initializeCirularCylinderShellMesh

        ! !! Initialize Sphere Mesh with Dictionary
        procedure, pass, private :: initializeCirularCylinderShellMeshDict

        ! !! Constructor
        generic :: initialize => initializeCirularCylinderShellMesh,      &
                                 initializeCirularCylinderShellMeshDict

    End Type

!! ------------------------------------------------------------------ !!
!!                      Type typCircularCylinderShellSurfMesh
!! ------------------------------------------------------------------ !!
