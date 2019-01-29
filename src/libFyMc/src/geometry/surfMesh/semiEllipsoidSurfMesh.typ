!!*------------------------------------------------------------------*!!
!!  Project : mkSurfaceMesh
!!*------------------------------------------------------------------*!!
!!
!!  Fortran Type
!!      typSemiEllipsoidSurfMesh
!!
!!  Description
!!      typSemiEllipsoidSurfMesh Type
!!
!!  Authors
!!      YoungMyung Choi, Ecole Centrale de Nantes
!!          - youngmyung.choi@ec-nantes.fr
!!
!!*------------------------------------------------------------------*!!

!! ------------------------------------------------------------------ !!
!!                      Type typSemiEllipsoidSurfMesh
!! ------------------------------------------------------------------ !!

    Type,extends(typSurfMesh), public :: typSemiEllipsoidSurfMesh

!! Type Data -------------------------------------------------------- !!

    Private

        !! - Length of Principal Axes
        Real(RP) :: rX_, rY_, rZ_

        !! - Nx, Ny, Nz
        Integer  :: nX_, nY_, nZ_

        !! - Origin
        Real(RP) :: x0_, y0_, z0_

!! Type Procedures -------------------------------------------------- !!

    contains

        !! Initialize Semi-Ellipsoid Surface Mesh
        procedure, pass, private :: initializeSemiEllipsoidSurfMesh

        !! Initialize Semi-Ellipsoid Surface Mesh with dictionary
        procedure, pass, private :: initializeSemiEllipsoidSurfMeshDict

        !! - Initializer
        generic :: initialize => initializeSemiEllipsoidSurfMesh,   &
                                 initializeSemiEllipsoidSurfMeshDict

    End Type

!! ------------------------------------------------------------------ !!
!!                      Type typSphereSurfMesh
!! ------------------------------------------------------------------ !!
