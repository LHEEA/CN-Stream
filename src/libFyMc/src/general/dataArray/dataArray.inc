Subroutine setDataArrayIntArray(this, array)
    Implicit None
    Class(typDataArray), intent(inout)  :: this
    integer, dimension(:), intent(in) :: array
    integer :: nData

    Call this%destroy()

    nData = size(array)

    if (nData.le.0) then
        write(*,*) "    [WARNING] setDataArrayReal(array), the number of array should be larger than 0."
        return
    endif

    allocate(this%arrR(nData))
    allocate(this%arrI(nData))

    this%nData = nData

    Call this%setAllfalseDataArray()
    this%isNum = .TRUE.
    this%isInt = .TRUE.

    this%arrR = real(array,rp)
    this%arrI = array

End Subroutine

Subroutine setDataArrayInt(this, value)
    Implicit None
    Class(typDataArray), intent(inout)  :: this
    integer, intent(in) :: value

    Call this%destroy()

    this%nData = 1
    allocate(this%arrR(1))
    allocate(this%arrI(1))

    Call this%setAllfalseDataArray()
    this%isNum = .TRUE.
    this%isInt = .TRUE.

    this%arrR(1) = real(value,rp)
    this%arrI(1) = value

End Subroutine

Subroutine setDataArrayRealArray(this, array)
    Implicit None
    Class(typDataArray), intent(inout)  :: this
    real(rp), dimension(:), intent(in) :: array
    integer :: nData

    Call this%destroy()

    nData = size(array)

    if (nData.le.0) then
        write(*,*) "    [WARNING] setDataArrayReal(array), the number of array should be larger than 0."
        return
    endif

    allocate(this%arrR(nData))
    allocate(this%arrI(nData))

    this%nData = nData

    Call this%setAllfalseDataArray()
    this%isNum = .TRUE.

    this%arrR = array
    this%arrI = int(array)

End Subroutine

Subroutine setDataArrayReal(this, value)
    Implicit None
    Class(typDataArray), intent(inout)  :: this
    real(rp), intent(in) :: value

    Call this%destroy()

    this%nData = 1
    allocate(this%arrR(1))
    allocate(this%arrI(1))

    Call this%setAllfalseDataArray()
    this%isNum = .TRUE.

    this%arrR(1) = value
    this%arrI(1) = int(value)

End Subroutine

Subroutine setDataArrayString(this, nData, value)
    Implicit None
    Class(typDataArray), intent(inout)  :: this
    integer, intent(in)                 :: nData
    type(typString), intent(in)         :: value
    integer:: idata
    Call this%destroy()
    this%nData = 1
    allocate(this%arrS(1))
    Call this%setAllfalseDataArray()
    this%isStr = .TRUE.
    this%arrS(1) = value
End Subroutine

Subroutine setDataArrayStringArray(this, nData, array)
    Implicit None
    Class(typDataArray), intent(inout)               :: this
    integer, intent(in)                              :: nData
    type(typString), dimension(:), intent(in)        :: array
    integer:: idata

    Call this%destroy()

    if (nData.le.0) then
        write(*,*) "    [WARNING] setDataArrayString(nData, array), nData should be larger than 0."
        return
    endif

    allocate(this%arrS(nData))
    this%nData = nData

    Call this%setAllfalseDataArray()
    this%isStr = .TRUE.

    do idata = 1, nData
        this%arrS(idata) = array(idata)
    enddo

End Subroutine

Subroutine setDataArrayChar(this, value, isTrim)
    Implicit None
    Class(typDataArray), intent(inout)               :: this
    Character(len=*), intent(in) :: value
    Logical, optional :: isTrim
    Call this%destroy()
    this%nData = 1
    allocate(this%arrS(1))
    Call this%arrS(1)%set(value, isTrim)
    Call this%setAllfalseDataArray()
    this%isStr = .TRUE.
End Subroutine

Subroutine setDataArrayCharArray(this, array, isTrim)
    Implicit None
    Class(typDataArray), intent(inout)               :: this
    Character(len=CHAR_LEN), dimension(:), intent(in) :: array
    Character(len=CHAR_LEN)                          :: dummyChar
    Logical, optional :: isTrim

    integer:: nData, idata

    Call this%destroy()

    dummyChar = "a"
    nData = size(array)

    if (nData.le.0) then
        write(*,*) "    [WARNING] setDataArrayChar(array), the number of array should be larger than 0."
        return
    endif
    this%nData = nData

    allocate(this%arrS(nData))

    do idata = 1, nData
        Call this%arrS(idata)%set(array(idata), isTrim)
    enddo

    Call this%setAllfalseDataArray()
    this%isStr = .TRUE.

End Subroutine

Subroutine setDataArrayLogical(this, value)
    Implicit None
    Class(typDataArray), intent(inout)  :: this
    logical, intent(in)     :: value
    Call this%destroy()
    this%nData = 1
    allocate(this%arrL(1))
    Call this%setAllfalseDataArray()
    this%isLog = .TRUE.
    this%arrL(1) = value
End Subroutine

Subroutine setDataArrayLogicalArray(this, array)
    Implicit None
    Class(typDataArray), intent(inout)  :: this
    logical, dimension(:), intent(in)     :: array
    integer:: nData
    Call this%destroy()
    nData = size(array)
    if (nData.le.0) then
        write(*,*) "    [WARNING] setDataArrayLogical(nData, array), the number of array should be larger than 0."
        return
    endif
    allocate(this%arrL(nData))
    this%nData = nData
    Call this%setAllfalseDataArray()
    this%isLog = .TRUE.
    this%arrL = array
End Subroutine

Subroutine destroyDataArray(this)
    Implicit None
    Class(typDataArray), intent(inout)  :: this
    integer :: idata

    if (allocated(this%arrR)) deallocate(this%arrR)
    if (allocated(this%arrI)) deallocate(this%arrI)
    if (allocated(this%arrL)) deallocate(this%arrL)

    if (this%isStr) then
    do idata = 1, this%nData
        Call this%arrS(idata)%destroy()
    enddo
    end if
    if (allocated(this%arrS)) deallocate(this%arrS)
    this%nData = 0
    Call setAllfalseDataArray(this)

End Subroutine

Subroutine setAllfalseDataArray(this)
    Implicit None
    Class(typDataArray), intent(inout)  :: this

    this%isNum = .FALSE.
    this%isStr = .FALSE.
    this%isLog = .FALSE.
    this%isInt = .FALSE.

End Subroutine

Subroutine copyDataArray(origin, target)
    Implicit None
    Class(typDataArray), intent(inout) :: origin
    Type(typDataArray), intent(in)     :: target
    integer :: idata

    if (origin%nData.ne.target%nData) then
        Call origin%destroy()
    end if

    if (target%isNum) then
        if (target%isInt) then
            call origin%set(target%arrI)
        else
            call origin%set(target%arrR)
        end if
    end if

    if (target%isLog) call origin%set(target%arrL)
    if (target%isStr) call origin%set(target%nData, target%arrS)

end subroutine

! Subroutine terminateDataArray(this)
!     Type(typDataArray),intent(inout) :: this
!     Call this%destroy
! End Subroutine
