    Subroutine testSLTable()
        Implicit None

        Call testPushAndGet()

    end Subroutine

    Subroutine testPushAndGet()

        Type(typSLookUpTable) :: sLTable
        integer :: nData

        !! Input Variables -------------------------------------------------
        Real(RP), allocatable :: valR(:)
        character(len=CHAR_LEN), allocatable :: valC(:)
        logical, allocatable :: valL(:)

        !! Output Variable -------------------------------------------------
        real(rp)                :: r1, r2, r3
        real(rp),allocatable    :: resultR1(:), resultR2(:), resultR3(:)

        integer                 :: i1, i2, i3
        integer,allocatable     :: resultI1(:), resultI2(:), resultI3(:)

        Type(typString)             :: tmpString
        Type(typString),allocatable :: tmpStringArr(:)

        character(len=CHAR_LEN) :: tmpChar
        character(len=CHAR_LEN),allocatable :: tmpCharArr(:)

        logical                 :: l1, l2
        logical,allocatable     :: resultL(:)

        !! -----------------------------------------------------------------

        !! Set Input Data

        nData = 3
        allocate(valR(nData))
        allocate(valC(nData))
        allocate(valL(nData))

        valR(1) = 1
        valR(2) = 2
        valR(3) = 3

        valC(1) = "false"
        valC(2) = "false"
        valC(3) = "false"

        valL(1) = .TRUE.
        valL(2) = .FALSE.
        valL(3) = .FALSE.

        Call sLTable%initialize()

        !! Push Numbers -----------------------------------------------------

        Call sLTable%push("one", 1)

        Call sLTable%push("ott", "1 2 3")

        Call sLTable%push("real1", 2.0_RP)

        Call sLTable%push("real2", 2.0)

        Call sLTable%push("realArray1", (/ 1.2_RP, 3.5_RP, 1.1_RP, 1.5_RP /) )

        Call sLTable%push("realArray2", (/ 1.2, 3.5, 1.1, 1.5 /) )

        Call sLTable%push("realArray3", valR)

        !! ----------------------------------------------------- Push Numbers

        Call sLTable%push("string", "a is equal to b")

        Call sLTable%push("stringArray", valC)

        Call sLTable%push("logical", "t t f f")

        Call sLTable%push("logicalArray", valL)

        Call sLTable%print()


        ! Call sLTable%delete("logical")
        ! Call sLTable%delete("stringArray")


        !!!... Check getReal
        Call sLTable%getReal("one", r1)
        write(*,*) "r1 = ", r1
        !write(*,*) sLTable%getReal("ott", 2), sLTable%getInt("ott", 3)
        !write(*,*) sLTable%getReal("real1"), sLTable%getInt("real2")

        Call sLTable%getRealArray("realArray1", resultR1)
        Call sLTable%getRealArray("realArray2", resultR2)
        Call sLTable%getRealArray("realArray3", resultR3)

        write(*,*) resultR1
        write(*,*) resultR2
        write(*,*) resultR3

        !!!... Check getInt

        Call sLTable%getInt("one", i1)
        write(*,*) i1
        ! write(*,*) sLTable%getInt("ott", 2), sLTable%getInt("ott", 3)
        ! write(*,*) sLTable%getInt("real1"), sLTable%getInt("real2")

        Call sLTable%getIntArray("realArray1", resultI1)
        Call sLTable%getIntArray("realArray2", resultI2)
        Call sLTable%getIntArray("realArray3", resultI3)

        write(*,*) resultI1
        write(*,*) resultI2
        write(*,*) resultI3

        !!!... Check getString

        ! Call sLTable%getString("string", tmpString)
        ! Call sLTable%getStringArray("stringArray", tmpStringArr)
        !
        ! write(*,*) tmpString%get()
        ! write(*,*) trim(tmpStringArr(1)%get()), trim(tmpStringArr(2)%get()), trim(tmpStringArr(3)%get())

        !!!... Check getChar
        Call sLTable%getChar("string", tmpChar)
        Call sLTable%getCharArray("stringArray", tmpCharArr)

        ! write(*,*) trim(tmpChar)
        write(*,*) trim(tmpCharArr(1)), trim(tmpCharArr(2)), trim(tmpCharArr(3))

        !!!... Check getLogical
        Call sLTable%getLogical("logical", l1)
        Call sLTable%getLogicalArray("logicalArray", resultL)
        write(*,*) l1
        write(*,*) resultL

        Call sLTable%getLogicalArray("stringArray", resultL)

        write(*,*) resultL

    End Subroutine
