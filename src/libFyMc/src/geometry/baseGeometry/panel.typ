!!*------------------------------------------------------------------*!!
!!  Project : mkSurfaceMesh
!!*------------------------------------------------------------------*!!
!!
!!  Fortran Type
!!      panel
!!
!!  Description
!!      Panel Type
!!
!!  Authors
!!      YoungMyung Choi, Ecole Centrale de Nantes
!!          - youngmyung.choi@ec-nantes.fr
!!
!!*------------------------------------------------------------------*!!

!! ------------------------------------------------------------------ !!
!!                      Type Panel
!! ------------------------------------------------------------------ !!

    type, public :: typPanel

!! Type Data -------------------------------------------------------- !!

    private

        !! Number of Node of Panel
        integer  :: nNode_

        !! Node Label Array
        integer, allocatable :: nodeLabel_(:)

        !! Node Label Array
        type(typPoint), public, allocatable :: node_(:)

        !! vtk write Surface Type
        integer  :: vtkCellType_

        !! Area of Panel (Sum of sub-triangle area)
        real(RP), public :: area

        !! Center of Panel (Area averaged normal)
        type(typPoint), public :: center

        !! Normal Vector
        type(typPoint), public :: normal

        !! Number of Gauss Point
        integer, public        :: nGauss

        !! Gauss Point
        type(typPoint), public, allocatable :: gPoint(:)

        !! Gauss Point
        real(RP), public, allocatable       :: gWeight(:)

!! Type Procedures -------------------------------------------------- !!

    contains

        !! Set Panel
        procedure, pass, public :: set => setPanel

        !! Compute Area and Normal with given nodes without changing without nodes
        procedure, pass, public :: correct => correctPanel

        !! Compute Area and Normal with given nodes without changing without nodes
        procedure, pass, public :: correctGaussPoint => computeConstantPanelGaussPoint

        !! Return Number of Panel Node
        procedure, pass, public :: nNode => nNodePanel

        !! Return Node label
        procedure, pass, public :: nodeLabel => panelNodeLabel

        !! Return VTK Surface Mesh Type
        procedure, pass, public :: vtkCellType => panelVTKCellType

        !! Copy Subroutine
        procedure, pass, public :: assignCopyPanel

        !! Copy Operator
        generic :: assignment(=) => assignCopyPanel

        !! Destroy Panel Type
        procedure, pass, public :: destroy => destroyPanel

        !! Destructor
        final :: finalPanel

    end type

!! ------------------------------------------------------------------ !!
!!                      Type Panel
!! ------------------------------------------------------------------ !!
