!!*------------------------------------------------------------------*!!
!!  Project : mkSurfaceMesh
!!*------------------------------------------------------------------*!!
!!
!!  Fortran Type
!!      surfMesh
!!
!!  Description
!!      Base Type for surface mesh
!!
!!  Authors
!!      YoungMyung Choi, Ecole Centrale de Nantes
!!          - youngmyung.choi@ec-nantes.fr
!!
!!*------------------------------------------------------------------*!!

!! ------------------------------------------------------------------ !!
!!                      Type SurfMesh
!! ------------------------------------------------------------------ !!

    type, public :: typSurfMesh

!! Type Data -------------------------------------------------------- !!

        !! - Is top is opened ?
        Logical :: isOpenTop

        !! - Number of Node
        Integer :: nNode, nLineNode

        !! - Number of Surf Panel
        Integer :: nPanel

        !! - Number of Line Mesh
        Integer :: nLine

        !! - Number of Segment of Line
        Integer :: nLineSub

        !! - Node Position
        type(typPoint), allocatable :: node(:)

        !! - Panel Connectivity
        type(typPanel), allocatable :: panel(:)

        !! - Line Node
        type(typPoint), allocatable :: lineNode(:)

        !! - Line Mesh if top is opened
        type(typPanel), allocatable :: linePanel(:)

!! Type Procedures -------------------------------------------------- !!

    contains

        procedure, pass, public :: writeVTK

        procedure, pass, public :: writeVTKSurfMesh

        procedure, pass, public :: writeVTKSurfGaussPoint

        procedure, pass, public :: writeVTKLineMesh

        procedure, pass, public :: writeVTKLineMeshGaussPoint

        procedure, pass, public :: assignCopySurfMesh

        generic :: assignment(=) => assignCopySurfMesh

        procedure, pass, public :: destroy => destroySurfMesh

        final :: finalSurfMesh

    end type

!! Type Procedures -------------------------------------------------- !!

    public :: initializeSurfMesh

!! ------------------------------------------------------------------ !!
!!  Derived Class
!! ------------------------------------------------------------------ !!

    !!- paraPipeSurfMesh
    Include "surfMesh/paraPipeSurfMesh.typ"

    !!- Sphere Mesh
    Include "surfMesh/sphereSurfMesh.typ"

    !!- Hemisphere Mesh
    Include "surfMesh/hemiSphereSurfMesh.typ"

    !!- Ellipsoid Mesh
    Include "surfMesh/ellipsoidSurfMesh.typ"

    !!- Semi-Ellipsoid Mesh
    Include "surfMesh/semiEllipsoidSurfMesh.typ"

    !!- Circular Cylinder Shell Mesh
    Include "surfMesh/circularCylinderShell.typ"

!! ------------------------------------------------------------------ !!
!!                      Type SurfMesh
!! ------------------------------------------------------------------ !!
