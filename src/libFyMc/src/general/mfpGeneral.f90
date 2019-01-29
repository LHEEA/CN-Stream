!!*------------------------------------------------------------------*!!
!!  FyMc Pack
!!*------------------------------------------------------------------*!!
!!  BSD 3-Clause License
!!
!!  Copyright (c) 2018, YoungMyung Choi
!!  All rights reserved.
!!
!!  Redistribution and use in source and binary forms, with or without
!!  modification, are permitted provided that the following conditions are met:
!!
!!  * Redistributions of source code must retain the above copyright notice, this
!!    list of conditions and the following disclaimer.
!!
!!  * Redistributions in binary form must reproduce the above copyright notice,
!!    this list of conditions and the following disclaimer in the documentation
!!    and/or other materials provided with the distribution.
!!
!!  * Neither the name of the copyright holder nor the names of its
!!    contributors may be used to endorse or promote products derived from
!!    this software without specific prior written permission.
!!
!!  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
!!  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
!!  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
!!  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
!!  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
!!  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
!!  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
!!  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
!!  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
!!  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!!
!!*------------------------------------------------------------------*!!
!!
!!  Fortran Module
!!      General
!!
!!  Authors
!!      YoungMyung Choi, Ecole Centrale de Nantes
!!          - youngmyung.choi@ec-nantes.fr
!!
!!*------------------------------------------------------------------*!!

!! ------------------------------------------------------------------ !!
!!  Module : FyMcPack Module General
!! ------------------------------------------------------------------ !!
Module  mfpGeneral

!!  Dependency ------------------------------------------------------ !!

    use mfpGlobal

!!  Variable Declaration -------------------------------------------- !!

Implicit None

private

!!  Module Variable ------------------------------------------------- !!

    integer, parameter :: strMaxLength = CHAR_LEN

    integer, parameter :: sLTableDefaultSize = 50

    logical, parameter :: sLTableSearchErrrorStop = .TRUE.

    integer, parameter :: fileLineLength = 2000

    integer, parameter :: DEFAULT_TAB_SIZE = 4

!!  Header Files ---------------------------------------------------- !!

    Include "string/stringI.typ"

    Include "string/string.typ"

    Include "fileIO/fileIO.typ"

    Include "dataArray/dataArray.typ"

    Include "sLookUpTable/sLookUpTable.typ"

    Include "dictionary/dictionary.typ"

    Include "dictionary/dictionaryPtr.typ"

    Include "dictionary/dictDataBase.typ"

    public :: testDataArray, testString, testSLTable, testDictionary

Contains

!!  Procedure Files ------------------------------------------------- !!

    Include "string/stringI.inc"

    Include "string/string.inc"

    Include "fileIO/fileIO.inc"

    Include "dataArray/dataArray.inc"

    Include "sLookUpTable/sLookUpTable.inc"

    Include "auxiliary/auxiliary.inc"

    Include "dictionary/dictionary.inc"

    Include "dictionary/dictionaryPtr.inc"

    Include "dictionary/dictDataBase.inc"

!!  Subroutine Test Script ------------------------------------------ !!

    Include "string/testString.inc"

    Include "dataArray/testDataArray.inc"

    Include "sLookUpTable/testSLTable.inc"

    Include "dictionary/testDictionary.inc"

End Module
