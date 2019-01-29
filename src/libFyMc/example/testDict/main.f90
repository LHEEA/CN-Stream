!! --------------------------------------------------------------------------
!!
!!                            FyMcPack Example
!!
!!  Example : testDict
!!
!!      Initiaize dictionary with input file and get variables
!!
!!  Authors
!!      YoungMyung Choi, Ecole Centrale de Nantes
!!
!! --------------------------------------------------------------------------
    Program main

        use mfpGlobal
        use mfpGeneral

    Implicit None
        type(typDictionaryPtr)  :: dictPtr, subDict
        character(len=CHAR_LEN) :: fileDir
        character(len=CHAR_LEN) :: fileName
        character(len=CHAR_LEN) :: fileExt

        real(rp)                :: realval
        real(rp), allocatable   :: realArr(:)

        !!... Set Dictionary File Path
        fileDir  = "../../"
        fileName = "testDict"
        fileExt  = ""

        !!... Intialize Dictionary
        Call dictPtr%initialize(fileDir, fileName, fileExt)

        !!... Show Dictionary Contents
        Call dictPtr%print()

        !!... Get Variables
        !!
        !!      Be careful. Fortran does not recognize output type.
        !!      It could be dangerous.
        !!
        write(*,*) "varR : ", dictPtr%getReal("varR")

        realArr = dictPtr%getRealArray("arrR")

        write(*,*) "realArray : ",realArr

        subDict = dictPtr%subDict("dict1")

        Call subDict%print()

        Call checkSubDict(subDict%subDict("dict3"))

    End Program

    Subroutine checkSubDict(dict)
        use mfpGlobal
        use mfpGeneral
    Implicit None
        type(typDictionaryPtr)  :: dict
        character(len=CHAR_LEN) :: fruit

        fruit = dict%getChar("fruit")
        write(*,*) "fruit = ", fruit

    End Subroutine
