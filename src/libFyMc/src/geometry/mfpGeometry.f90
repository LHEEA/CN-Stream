!!*------------------------------------------------------------------*!!
!!  Project : mfpDataStructure
!!*------------------------------------------------------------------*!!
!!
!!  Fortran Module
!!      General
!!
!!  Description
!!      FyMC Pack General Module
!!
!!  Authors
!!      YoungMyung Choi, Ecole Centrale de Nantes
!!          - youngmyung.choi@ec-nantes.fr
!!
!!*------------------------------------------------------------------*!!

!! ------------------------------------------------------------------ !!
!!  Module : FyMcPack Module General
!! ------------------------------------------------------------------ !!
Module  mfpGeometry

!!  Dependency ------------------------------------------------------ !!

    use mfpGlobal
    use mfpGeneral

!!  Variable Declaration -------------------------------------------- !!

Implicit None

private

!!  Module Variable ------------------------------------------------- !!

    Include "../mathConstant/gaussQuadrature.inc"

!!  Header Files ---------------------------------------------------- !!

    Include "baseGeometry/point.typ"

    Include "baseGeometry/panel.typ"

    Include "surfMesh/surfMesh.typ"

    !! Public

    Public :: testSurfMesh

Contains

!!  Procedure Files ------------------------------------------------- !!

    Include "auxiliary/getGaussCoordinate.inc"

    Include "baseGeometry/point.inc"

    Include "baseGeometry/panel.inc"

    Include "surfMesh/surfMesh.inc"

!!  Subroutine Test Script ------------------------------------------ !!

    Include "surfMesh/testSurfMesh.inc"

End Module
