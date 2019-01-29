subroutine initializeSLtable(this, name, strongSearch, tableSize)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    Character(len=*),optional :: name
    Integer, optional :: tableSize
    logical,optional :: strongSearch

    this%nKey   = 0
    this%nTable = sLTableDefaultSize

    if (present(tableSize)) then
        if (tableSize.ge.1) then
            this%nTable = tableSize
        end if
    end if

    allocate(this%keys(this%nTable))
    allocate(this%dataArray(this%nTable))
    allocate(this%isKeyActive(this%nTable))

    this%isKeyActive = .FALSE.

    this%name = " "
    if (present(name)) then
        this%name = name
    end if

    this%strongSearch = .FALSE.
    if (present(strongSearch)) then
        this%strongSearch = strongSearch
    end if

end

subroutine pushSLTableReal(this, key, value)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key
    real, intent(in)                      :: value
    integer :: keyIndex
    logical :: isExist

    Call pushSLTableRealRP(this, key, real(value, rp))

end subroutine

subroutine pushSLTableRealArray(this, key, value)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key
    real, dimension(:), intent(in)    :: value
    real(rp), allocatable                 :: tmpValue(:)
    integer :: keyIndex
    logical :: isExist
    integer :: nData
    nData = size(value)
    allocate(tmpValue(nData))
    tmpValue = real(value, rp)
    Call pushSLTableRealRPArray(this, key, tmpValue)
    deallocate(tmpValue)

end subroutine

subroutine pushSLTableRealRP(this, key, value)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key
    real(rp), intent(in)                  :: value
    integer :: keyIndex
    logical :: isExist

    Call this%searchKeyIndexAndAdd(key, keyIndex, isExist)

    if (isExist) then
        Call this%dataArray(keyIndex)%set(real(value, rp))
    else
        if (keyIndex.gt.this%nTable) then
            Call this%reallocateSLTable(keyIndex)
        end if

        Call this%keys(keyIndex)%set(key, .TRUE.)
        Call this%dataArray(keyIndex)%set(value)
        this%nkey = this%nkey + 1
        this%isKeyActive(keyIndex) = .TRUE.
    end if

end subroutine

subroutine pushSLTableRealRPArray(this, key, value)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key
    real(rp), dimension(:), intent(in)    :: value
    integer :: keyIndex
    logical :: isExist

    Call this%searchKeyIndexAndAdd(key, keyIndex, isExist)

    if (isExist) then
        Call this%dataArray(keyIndex)%set(value)
    else
        if (keyIndex.gt.this%nTable) then
            Call this%reallocateSLTable(keyIndex)
        end if

        Call this%keys(keyIndex)%set(key, .TRUE.)
        Call this%dataArray(keyIndex)%set(value)
        this%nkey = this%nkey + 1
        this%isKeyActive(keyIndex) = .TRUE.
    end if

end subroutine


subroutine pushSLTableInt(this, key, value)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key
    integer, intent(in)                   :: value
    integer :: keyIndex
    logical :: isExist
    type(typString) :: tmpString

    Call this%searchKeyIndexAndAdd(key, keyIndex, isExist)

    if (isExist) then
        Call this%dataArray(keyIndex)%set(real(value, rp))
    else
        if (keyIndex.gt.this%nTable) then
            Call this%reallocateSLTable(keyIndex)
        end if

        Call this%keys(keyIndex)%set(key, .TRUE.)
        Call this%dataArray(keyIndex)%set(value)
        this%nkey = this%nkey + 1
        this%isKeyActive(keyIndex) = .TRUE.
    end if

end subroutine

subroutine pushSLTableIntArray(this, key, value)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key
    integer, dimension(:), intent(in)     :: value
    integer :: keyIndex
    logical :: isExist

    Call this%searchKeyIndexAndAdd(key, keyIndex, isExist)

    if (isExist) then
        Call this%dataArray(keyIndex)%set(value)
    else
        if (keyIndex.gt.this%nTable) then
            Call this%reallocateSLTable(keyIndex)
        end if

        Call this%keys(keyIndex)%set(key, .TRUE.)
        Call this%dataArray(keyIndex)%set(value)
        this%nkey = this%nkey + 1
        this%isKeyActive(keyIndex) = .TRUE.
    end if

end subroutine

subroutine pushSLTableChar(this, key, value)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key
    character(len=*), intent(in)          :: value
    integer :: keyIndex
    logical :: isExist

    Call this%searchKeyIndexAndAdd(key, keyIndex, isExist)

    if (isExist) then
        Call this%dataArray(keyIndex)%set(value)
    else
        if (keyIndex.gt.this%nTable) then
            Call this%reallocateSLTable(keyIndex)
        end if

        Call this%keys(keyIndex)%set(key, .TRUE.)
        Call this%dataArray(keyIndex)%set(value)
        this%nkey = this%nkey + 1
        this%isKeyActive(keyIndex) = .TRUE.
    end if

end subroutine

subroutine pushSLTableCharArray(this, key, value)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key
    character(len=CHAR_LEN), dimension(:), intent(in) :: value
    integer :: keyIndex
    logical :: isExist

    Call this%searchKeyIndexAndAdd(key, keyIndex, isExist)

    if (isExist) then
        Call this%dataArray(keyIndex)%set(value)
    else
        if (keyIndex.gt.this%nTable) then
            Call this%reallocateSLTable(keyIndex)
        end if

        Call this%keys(keyIndex)%set(key, .TRUE.)
        Call this%dataArray(keyIndex)%set(value)
        this%nkey = this%nkey + 1
        this%isKeyActive(keyIndex) = .TRUE.
    end if

end subroutine


subroutine pushSLTableLogical(this, key, value)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key
    logical, intent(in)          :: value
    integer :: keyIndex
    logical :: isExist

    Call this%searchKeyIndexAndAdd(key, keyIndex, isExist)

    if (isExist) then
        Call this%dataArray(keyIndex)%set(value)
    else
        if (keyIndex.gt.this%nTable) then
            Call this%reallocateSLTable(keyIndex)
        end if

        Call this%keys(keyIndex)%set(key, .TRUE.)
        Call this%dataArray(keyIndex)%set(value)
        this%nkey = this%nkey + 1
        this%isKeyActive(keyIndex) = .TRUE.
    end if

end subroutine

subroutine pushSLTableLogicalArray(this, key, value)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key
    logical, dimension(:), intent(in)     :: value
    integer :: keyIndex
    logical :: isExist

    Call this%searchKeyIndexAndAdd(key, keyIndex, isExist)

    if (isExist) then
        Call this%dataArray(keyIndex)%set(value)
    else
        if (keyIndex.gt.this%nTable) then
            Call this%reallocateSLTable(keyIndex)
        end if

        Call this%keys(keyIndex)%set(key, .TRUE.)
        Call this%dataArray(keyIndex)%set(value)
        this%nkey = this%nkey + 1
        this%isKeyActive(keyIndex) = .TRUE.
    end if

end subroutine

function getSLTableDataArray(this, keyIndex, isStop) result(dataArray)
    Implicit None
    class(typSLookUpTable), intent(in)    :: this
    integer, intent(in)                   :: keyIndex
    type(typDataArray)                    :: dataArray
    logical,optional                      :: isStop
    logical                               :: isExist
    logical :: isErr

    isErr = .TRUE.
    if (present(isStop)) isErr = isStop

    if (keyIndex.le.this%nKey) then
        if (this%isKeyActive(keyIndex)) then
            dataArray = this%dataArray(keyIndex)
        else
            write(*,*) "No key exist for given key Index. keyIndex : ", keyIndex
            if (isErr) stop
        end if
    else
        write(*,*) "Given keyIndex is over than sLookTable Size. keyIndex : ", keyIndex
        if (isErr) stop
    end if

end function


function getSLTableKey(this, keyIndex, isStop) result(key)
    Implicit None
    class(typSLookUpTable), intent(in)    :: this
    integer, intent(in)                   :: keyIndex
    character(len=CHAR_LEN)               :: key
    logical,optional                      :: isStop
    logical                               :: isExist
    logical :: isErr

    isErr = .TRUE.
    if (present(isStop)) isErr = isStop

    key = ""
    if (keyIndex.le.this%nKey) then
        if (this%isKeyActive(keyIndex)) then
            key = this%keys(keyIndex)%get()
        else
            write(*,*) "No key exist for given key Index. keyIndex : ", keyIndex
            if (isErr) stop
        end if
    else
        write(*,*) "Given keyIndex is over than sLookTable Size. keyIndex : ", keyIndex
        if (isErr) stop
    end if

end function

subroutine getSLTableReal(this, key, value, ivar, isStop, isCorrect)
    Implicit None
    class(typSLookUpTable), intent(in)    :: this
    character(len=*), intent(in)          :: key
    real(RP)                              :: value

    integer,optional                      :: ivar
    logical, intent(in), optional         :: isStop
    logical, intent(out), optional        :: isCorrect

    logical :: errStop, errCorrect

    integer :: keyIndex
    logical :: isExist
    integer :: nData, idata
    type(typString),allocatable     :: parsedString(:)
    integer :: nargs

    errCorrect = .FALSE.
    errStop = sLTableSearchErrrorStop
    if (present(isStop)) errStop = isStop

    Call this%searchKeyIndex(key, keyIndex, isExist)

    value = 0.0_RP
    if (isExist) then

        nData = this%dataArray(keyIndex)%nData

        if (nData.ge.1) then
            if (this%dataArray(keyIndex)%isNum) then

                if (present(ivar)) then
                    if ((ivar.le.nData).and.(ivar.ge.1)) then
                        value = this%dataArray(keyIndex)%arrR(ivar)
                        errCorrect = .TRUE.
                    else
                        write(*,*) "    [ERROR] getSLTableString(key, ivar), key : ", trim(key)
                        write(*,*) "            ivar is over than the size of array."
                        write(*,*) "              ivar : ", ivar, " nArray : ", nData
                        if (errStop) stop
                    end if
                else
                    value = this%dataArray(keyIndex)%arrR(1)
                    errCorrect = .TRUE.
                end if

            else if (this%dataArray(keyIndex)%isStr) then

                if (present(ivar)) then

                    if (nData.eq.1) then

                        Call parse(this%dataArray(keyIndex)%arrS(1)," ", parsedString, nargs)

                        if (ivar.le.nargs) then
                            value = parsedString(ivar)%getReal()
                            errCorrect = .TRUE.
                        else
                            write(*,*) "    [ERROR] getSLTableString(key, ivar), key : ", trim(key)
                            write(*,*) "            ivar is over than the size of array."
                            write(*,*) "              ivar : ", ivar, " nArray : ", nargs
                            if (errStop) stop
                        end if

                        do idata = 1,nData
                            Call parsedString(idata)%destroy()
                        enddo

                        deallocate(parsedString)

                    else

                        if ((ivar.le.nData).and.(ivar.ge.1)) then
                            value = this%dataArray(keyIndex)%arrS(ivar)%getReal()
                            errCorrect = .TRUE.
                        else
                            write(*,*) "    [ERROR] getSLTableString(key, ivar), key : ", trim(key)
                            write(*,*) "            ivar is over than the size of array."
                            write(*,*) "              ivar : ", ivar, " nArray : ", nData
                            if (errStop) stop
                        end if

                    end if

                else
                    value = this%dataArray(keyIndex)%arrS(1)%getReal()
                    errCorrect = .TRUE.
                end if

            else
                write(*,*) "    [ERROR] getSLTableReal, key : ", trim(key)
                write(*,*) "            data is not number nor number type string."
                if (errStop) stop
            end if
        else
            write(*,*) "    [ERROR] getSLTableReal, key : ", trim(key)
            write(*,*) "            the size of data is zero."
            if (errStop) stop
        end if
    else
        call this%printSearchError(key, errStop)
    end if

    if (present(isCorrect)) isCorrect = errCorrect

end subroutine

subroutine getSLTableRealArray(this, key, value, isStop, isCorrect)
    Implicit None
    class(typSLookUpTable), intent(in)    :: this
    character(len=*), intent(in)          :: key
    real(rp), allocatable, intent(out)    :: value(:)

    logical, intent(in), optional         :: isStop
    logical, intent(out), optional        :: isCorrect

    logical :: errStop, errCorrect

    integer :: keyIndex
    logical :: isExist
    integer :: nData, idata

    type(typString),allocatable     :: parsedString(:)
    integer :: nargs

    errCorrect = .FALSE.
    errStop = sLTableSearchErrrorStop
    if (present(isStop)) errStop = isStop

    !!!... Allocate Array
    if (allocated(value)) deallocate(value)

    Call this%searchKeyIndex(key, keyIndex, isExist)

    if (isExist) then
        nData = this%dataArray(keyIndex)%nData

        if (nData.ge.1) then
            if (this%dataArray(keyIndex)%isNum) then
                allocate(value(nData))
                value = this%dataArray(keyIndex)%arrR
                errCorrect = .TRUE.

            else if (this%dataArray(keyIndex)%isStr) then
                if (nData.eq.1) then

                    Call parse(this%dataArray(keyIndex)%arrS(1)," ", parsedString, nargs)
                    nData = nargs

                    allocate(value(nData))

                    do idata = 1,nData
                        value(idata) = parsedString(idata)%getReal()
                    enddo

                    do idata = 1,nData
                        Call parsedString(idata)%destroy()
                    enddo

                    if (allocated(parsedString)) deallocate(parsedString)

                    errCorrect = .TRUE.
                else

                    !!!... Allocate Array
                    allocate(value(nData))

                    do idata = 1,nData
                        value(idata) = this%dataArray(keyIndex)%arrS(idata)%getReal()
                    enddo
                    errCorrect = .TRUE.
                end if

            else
                write(*,*) "    [ERROR] getSLTableRealArray, key : ", trim(key)
                write(*,*) "            data is not number nor number type string."
                if (errStop) stop
            end if
        else
            write(*,*) "    [ERROR] getSLTableRealArray, key : ", trim(key)
            write(*,*) "            the size of data is zero."
            if (errStop) stop
        end if
    else
        call this%printSearchError(key, errStop)
    end if

    if (present(isCorrect)) isCorrect = errCorrect

end subroutine

subroutine getSLTableInt(this, key, value, ivar, isStop, isCorrect)
    Implicit None
    class(typSLookUpTable), intent(in)    :: this
    character(len=*), intent(in)          :: key
    integer                               :: value
    integer,optional                      :: ivar
    logical, intent(in), optional         :: isStop
    logical, intent(out), optional        :: isCorrect
    real(rp) :: rValue
    logical :: errStop, errCorrect

    errCorrect = .FALSE.
    errStop = sLTableSearchErrrorStop
    if (present(isStop)) errStop = isStop

    Call this%getReal(key, rValue, ivar, .FALSE., errCorrect)

    if (errStop.AND.(errCorrect.EQV..FALSE.)) then
        write(*,*)  "   [ERROR] getSLTableInt(key, value). no number type."
        write(*,*)  "     key : ", trim(key)
        stop
    end if

    value = int(rValue)

    if (present(isCorrect)) isCorrect = errCorrect

end subroutine

subroutine getSLTableIntArray(this, key, value, isStop, isCorrect)
    Implicit None
    class(typSLookUpTable), intent(in)    :: this
    character(len=*), intent(in)          :: key
    integer, allocatable, intent(out)     :: value(:)
    real(rp), allocatable                 :: rValue(:)

    logical, intent(in), optional         :: isStop
    logical, intent(out), optional        :: isCorrect

    logical :: errStop, errCorrect
    integer :: nData

    integer :: keyIndex
    logical :: isExist

    errCorrect = .FALSE.
    errStop = sLTableSearchErrrorStop
    if (present(isStop)) errStop = isStop

    if (allocated(value)) deallocate(value)
    Call this%searchKeyIndex(key, keyIndex, isExist)

    if (isExist) then

        Call this%getRealArray(key, rValue, .FALSE., errCorrect)

        if (errStop.AND.(errCorrect.EQV..FALSE.)) then
            write(*,*)  "   [ERROR] getSLTableIntArray(key, value). no number type."
            write(*,*)  "     key : ", trim(key)
            stop
        end if

        errCorrect = .FALSE.

        if (sizeof(rValue).gt.0) then
            nData = sizeof(rValue) / sizeof(1.0_RP)
            if (nData.ge.1) then
                allocate(value(nData))
                value = int(rValue)
                errCorrect = .TRUE.
            else
                write(*,*) "    [ERROR] getSLTableReal, key : ", trim(key)
                write(*,*) "            the size of data is zero."
                stop
            end if
        else
            write(*,*) "    [ERROR] getSLTableReal, key : ", trim(key)
            write(*,*) "            the size of data is zero."
            stop
        end if

        if (allocated(rValue)) deallocate(rValue)

    else
        Call this%printSearchError(key, errStop)
    end if

    if (present(isCorrect)) isCorrect = errCorrect

end subroutine

subroutine getSLTableString(this, key, value, ivar, isStop, isCorrect)
    Implicit None
    class(typSLookUpTable), intent(in)    :: this
    character(len=*), intent(in)          :: key
    type(typString),intent(out)           :: value

    integer, optional                     :: ivar
    logical, intent(in), optional         :: isStop
    logical, intent(out), optional        :: isCorrect

    logical :: errStop, errCorrect

    integer :: keyIndex
    logical :: isExist
    integer :: nData

    errCorrect = .FALSE.
    errStop = sLTableSearchErrrorStop
    if (present(isStop)) errStop = isStop

    Call this%searchKeyIndex(key, keyIndex, isExist)

    if (isExist) then
        nData = this%dataArray(keyIndex)%nData
        if (nData.ge.1) then
            if (this%dataArray(keyIndex)%isStr) then
                if (present(ivar)) then
                    if ((ivar.le.nData).and.(ivar.ge.1)) then
                        value = this%dataArray(keyIndex)%arrS(ivar)
                        errCorrect = .TRUE.
                    else
                        write(*,*) "    [ERROR] getSLTableString(key, ivar), key : ", trim(key)
                        write(*,*) "            ivar is over than the size of array."
                        write(*,*) "              ivar : ", ivar, " nArray : ", nData
                        if (errStop) stop
                    end if
                else
                    value = this%dataArray(keyIndex)%arrS(1)
                    errCorrect = .TRUE.
                end if
            else
                write(*,*) "    [ERROR] getSLTableString, key : ", trim(key)
                write(*,*) "            data is not string type."
                if (errStop) stop
            end if
        else
            write(*,*) "    [ERROR] getSLTableString, key : ", trim(key)
            write(*,*) "            the size of data is zero."
            if (errStop) stop
        end if
    else
        call this%printSearchError(key, errStop)
    end if

    if (present(isCorrect)) isCorrect = errCorrect

end subroutine

subroutine getSLTableStringArray(this, key, value, isStop, isCorrect, delims)
    Implicit None
    class(typSLookUpTable), intent(in)    :: this
    character(len=*), intent(in)          :: key

    logical, intent(in), optional         :: isStop
    logical, intent(out), optional        :: isCorrect
    logical :: errStop, errCorrect

    character(len=*),optional             :: delims
    type(typString), allocatable          :: value(:)
    integer :: keyIndex
    logical :: isExist
    integer :: nData, idata
    type(typString), allocatable          :: parsedString(:)
    integer :: nargs

    !!!... Allocate Array
    if (allocated(value)) deallocate(value)

    errCorrect = .FALSE.
    errStop = sLTableSearchErrrorStop
    if (present(isStop)) errStop = isStop

    Call this%searchKeyIndex(key, keyIndex, isExist)

    if (isExist) then
        nData = this%dataArray(keyIndex)%nData
        if (nData.ge.1) then
            if (this%dataArray(keyIndex)%isStr) then

                if (nData.eq.1) then
                    if (present(delims)) then
                        Call parse(this%dataArray(keyIndex)%arrS(1), delims, value, nargs)
                    else
                        allocate(value(1))
                        value = this%dataArray(keyIndex)%arrS(1)
                    end if
                else
                    allocate(value(nData))
                    do idata = 1,nData
                        value(idata) = this%dataArray(keyIndex)%arrS(idata)
                    enddo
                end if
                errCorrect = .TRUE.
            else
                write(*,*) "    [ERROR] getSLTableStringArray, key : ", trim(key)
                write(*,*) "            data is not string type."
                if (errStop) stop
            end if
        else
            write(*,*) "    [ERROR] getSLTableStringArray, key : ", trim(key)
            write(*,*) "            the size of data is zero."
            if (errStop) stop
        end if
    else
        call this%printSearchError(key, errStop)
    end if

    if (present(isCorrect)) isCorrect = errCorrect

end subroutine

subroutine getSLTableChar(this, key, value, ivar, isStop, isCorrect)
    Implicit None
    class(typSLookUpTable), intent(in)    :: this
    character(len=*), intent(in)          :: key
    character(len=CHAR_LEN)               :: value
    integer, optional                     :: ivar

    logical, intent(in), optional         :: isStop
    logical, intent(out), optional        :: isCorrect

    logical                               :: errStop, errCorrect
    type(typString)                       :: tmpString

    errCorrect = .FALSE.
    errStop = sLTableSearchErrrorStop
    if (present(isStop)) errStop = isStop

    Call this%getString(key, tmpString, ivar, .FALSE., errCorrect)

    if (errStop.and.(errCorrect.EQV..FALSE.)) then
        write(*,*)  "   [ERROR] getSLTableChar(key, value). no string type."
        write(*,*)  "     key : ", trim(key)
        stop
    end if

    value = tmpString%get()
    Call tmpString%destroy()

    if (present(isCorrect)) isCorrect = errCorrect

end subroutine

subroutine getSLTableCharArray(this, key, value, isStop, isCorrect, delims)
    Implicit None
    class(typSLookUpTable), intent(in)    :: this
    character(len=*), intent(in)          :: key
    character(len=CHAR_LEN), allocatable  :: value(:)

    logical, intent(in), optional         :: isStop
    logical, intent(out), optional        :: isCorrect
    character(len=*),optional             :: delims

    logical                               :: errStop, errCorrect
    type(typString),allocatable           :: tmpString(:)
    integer :: i, nArray

    !!!... Allocate Array
    if (allocated(value)) deallocate(value)

    errCorrect = .FALSE.
    errStop = sLTableSearchErrrorStop
    if (present(isStop)) errStop = isStop

    Call this%getStringArray(key, tmpString, .FALSE., errCorrect, delims)

    if (errStop.and.(errCorrect.EQV..FALSE.)) then
        write(*,*)  "   [ERROR] getSLTableCharArray(key, value). no string type."
        write(*,*)  "     key : ", trim(key)
        if (present(delims)) write(*,*) "     delims : ", trim(delims)
        stop
    end if

    errCorrect = .FALSE.
    if (sizeof(tmpString).gt.1) then
        nArray = sizeof(tmpString) / sizeof(tmpString(1))
        if (nArray.ge.1) then
            allocate(value(nArray))
            do i = 1, nArray
                value(i) = tmpString(i)%get()
            enddo

            do i = 1, nArray
                Call tmpString(i)%destroy()
            enddo
            deallocate(tmpString)
            errCorrect = .TRUE.
        else
            write(*,*)  "   [WARNING] getSLTableCharArray(key, value) array size is zero."
            write(*,*)  "     key : ", trim(key)
            if (present(delims)) write(*,*) "     delims : ", trim(delims)
            if (errStop) stop
        end if
    else
        write(*,*)  "   [WARNING] getSLTableCharArray(key, value) array size is zero."
        write(*,*)  "     key : ", trim(key)
        if (present(delims)) write(*,*) "     delims : ", trim(delims)
        if (errStop) stop
    end if

    if (present(isCorrect)) isCorrect = errCorrect

end subroutine

subroutine getSLTableLogical(this, key, value, ivar, isStop, isCorrect)
    Implicit None
    class(typSLookUpTable), intent(in)    :: this
    character(len=*), intent(in)          :: key
    logical,intent(out)                   :: value
    integer, optional                     :: ivar

    logical, intent(in), optional         :: isStop
    logical, intent(out), optional        :: isCorrect
    logical                               :: errStop, errCorrect

    integer :: keyIndex
    logical :: isExist
    integer :: nData, idata

    type(typString), allocatable          :: parsedString(:)
    integer :: nargs

    type(typString)                       :: tmpString

    Call this%searchKeyIndex(key, keyIndex, isExist)

    errCorrect = .FALSE.
    errStop = sLTableSearchErrrorStop
    if (present(isStop)) errStop = isStop

    if (isExist) then
        nData = this%dataArray(keyIndex)%nData
        if (nData.ge.1) then
            if (this%dataArray(keyIndex)%isLog) then
                if (present(ivar)) then
                    if ((ivar.le.nData).and.(ivar.ge.1)) then
                        value = this%dataArray(keyIndex)%arrL(ivar)
                        errCorrect = .TRUE.
                    else
                        write(*,*) "    [ERROR] getSLTableLogical(key, ivar), key : ", trim(key)
                        write(*,*) "            ivar is over than the size of array."
                        write(*,*) "              ivar : ", ivar, " nArray : ", nData
                        if (errStop) stop
                    end if
                else
                    value = this%dataArray(keyIndex)%arrL(1)
                    errCorrect = .TRUE.
                end if
            else if (this%dataArray(keyIndex)%isStr) then

                if (nData.eq.1) then

                    Call parse(this%dataArray(keyIndex)%arrS(1)," ", parsedString, nargs)

                    if (nargs.ge.1) then

                        if (present(ivar)) then
                            if ( isStringEqual(parsedString(ivar), "true", .FALSE.) ) then
                                value = .TRUE.
                            else if ( isStringEqual(parsedString(ivar), "t", .FALSE.) ) then
                                value = .TRUE.
                            else if ( isStringEqual(parsedString(ivar), "false", .FALSE.) ) then
                                value = .FALSE.
                            else if ( isStringEqual(parsedString(ivar), "f", .FALSE.) ) then
                                value = .FALSE.
                            else
                                write(*,*) "    [ERROR] getSLTableLogical, key, idata : ", trim(key), idata
                                write(*,*) "            converting error (string to logical) : ", parsedString(idata)%get()
                                if (errStop) stop
                            end if
                        else
                            if ( isStringEqual(parsedString(1), "true", .FALSE.) ) then
                                value = .TRUE.
                            else if ( isStringEqual(parsedString(1), "t", .FALSE.) ) then
                                value = .TRUE.
                            else if ( isStringEqual(parsedString(1), "false", .FALSE.) ) then
                                value = .FALSE.
                            else if ( isStringEqual(parsedString(1), "f", .FALSE.) ) then
                                value = .FALSE.
                            else
                                write(*,*) "    [ERROR] getSLTableLogical, key, idata : ", trim(key), idata
                                write(*,*) "            converting error (string to logical) : ", parsedString(idata)%get()
                                if (errStop) stop
                            end if
                        end if

                    else
                        write(*,*) "    [ERROR] getSLTableLogical, key : ", trim(key)
                        write(*,*) "            string is not parsed with space."
                        if (errStop) stop
                    end if

                    do idata = 1, nData
                        Call parsedString(idata)%destroy()
                    enddo
                    if (allocated(parsedString)) deallocate(parsedString)

                else

                    if (present(ivar)) then

                        if ((ivar.le.nData).and.(ivar.ge.1)) then

                            tmpString = this%dataArray(keyIndex)%arrS(ivar)

                            if ( isStringEqual(tmpString, "true", .FALSE.) ) then
                                value = .TRUE.
                                errCorrect = .TRUE.
                            else if ( isStringEqual(tmpString, "t", .FALSE.) ) then
                                value = .TRUE.
                                errCorrect = .TRUE.
                            else if ( isStringEqual(tmpString, "false", .FALSE.) ) then
                                value = .FALSE.
                                errCorrect = .TRUE.
                            else if ( isStringEqual(tmpString, "f", .FALSE.) ) then
                                value = .FALSE.
                                errCorrect = .TRUE.
                            else
                                write(*,*) "    [ERROR] getSLTableLogical, key : ", trim(key)
                                write(*,*) "            data is not logical type."
                                if (errStop) stop
                            end if

                            Call tmpString%destroy()

                        else
                            write(*,*) "    [ERROR] getSLTableLogical(key, ivar), key : ", trim(key)
                            write(*,*) "            ivar is over than the size of array."
                            write(*,*) "              ivar : ", ivar, " nArray : ", nData
                            if (errStop) stop
                        end if
                    else

                        tmpString = this%dataArray(keyIndex)%arrS(1)

                        if ( isStringEqual(tmpString, "true", .FALSE.) ) then
                            value = .TRUE.
                            errCorrect = .TRUE.
                        else if ( isStringEqual(tmpString, "t", .FALSE.) ) then
                            value = .TRUE.
                            errCorrect = .TRUE.
                        else if ( isStringEqual(tmpString, "false", .FALSE.) ) then
                            value = .FALSE.
                            errCorrect = .TRUE.
                        else if ( isStringEqual(tmpString, "f", .FALSE.) ) then
                            value = .FALSE.
                            errCorrect = .TRUE.
                        else
                            write(*,*) "    [ERROR] getSLTableLogical, key : ", trim(key)
                            write(*,*) "            data is not logical type.", tmpString%get()
                            if (errStop) stop
                        end if

                        Call tmpString%destroy()

                    end if

                end if

            else
                write(*,*) "    [ERROR] getSLTableLogical, key : ", trim(key)
                write(*,*) "            data is not logical type."
                if (errStop) stop
            end if
        else
            write(*,*) "    [ERROR] getSLTableLogical, key : ", trim(key)
            write(*,*) "            the size of data is zero."
            if (errStop) stop
        end if
    else
        call this%printSearchError(key, errStop)
    end if

    if (present(isCorrect)) isCorrect = errCorrect

end subroutine

subroutine getSLTableLogicalArray(this, key, value, isStop, isCorrect)
    Implicit None
    class(typSLookUpTable), intent(in)    :: this
    character(len=*), intent(in)          :: key
    logical, allocatable                  :: value(:)

    logical, intent(in), optional         :: isStop
    logical, intent(out), optional        :: isCorrect
    logical                               :: errStop, errCorrect

    integer :: keyIndex
    logical :: isExist
    integer :: nData, idata

    type(typString), allocatable          :: parsedString(:)
    integer :: nargs

    logical :: subLoopError

    Call this%searchKeyIndex(key, keyIndex, isExist)

    errCorrect = .FALSE.
    errStop = sLTableSearchErrrorStop
    if (present(isStop)) errStop = isStop

    if (allocated(value)) deallocate(value)
    if (isExist) then
        nData = this%dataArray(keyIndex)%nData

        if (nData.ge.1) then
            if (this%dataArray(keyIndex)%isLog) then
                !!!... Allocate Array
                allocate(value(nData))
                value = this%dataArray(keyIndex)%arrL

                errCorrect = .TRUE.

            else if (this%dataArray(keyIndex)%isStr) then

                if (nData.eq.1) then
                    Call parse(this%dataArray(keyIndex)%arrS(1)," ", parsedString, nargs)
                    if (nargs.ge.1) then
                        nData = nargs
                        allocate(value(nData))

                        subLoopError = .TRUE.
                        do idata = 1, nData
                            if ( isStringEqual(parsedString(idata), "true", .FALSE.) ) then
                                value(idata) = .TRUE.
                            else if ( isStringEqual(parsedString(idata), "t", .FALSE.) ) then
                                value(idata) = .TRUE.
                            else if ( isStringEqual(parsedString(idata), "false", .FALSE.) ) then
                                value(idata) = .FALSE.
                            else if ( isStringEqual(parsedString(idata), "f", .FALSE.) ) then
                                value(idata) = .FALSE.
                            else
                                subLoopError = .FALSE.
                                write(*,*) "    [ERROR] getSLTableLogical, key, idata : ", trim(key), idata
                                write(*,*) "            converting error (string to logical) : ", parsedString(idata)%get()
                                if (errStop) stop
                            end if
                        enddo

                        do idata = 1, nData
                            Call parsedString(idata)%destroy()
                        enddo
                        deallocate(parsedString)

                        if (subLoopError) errCorrect = .TRUE.

                    else
                        write(*,*) "    [ERROR] getSLTableLogicalArray, key : ", trim(key)
                        write(*,*) "            string is not parsed with space."
                        if (errStop) stop
                    end if
                else
                    allocate(value(nData))
                    subLoopError = .TRUE.

                    do idata = 1, nData

                        if ( isStringEqual(this%dataArray(keyIndex)%arrS(idata), "true", .FALSE.) ) then
                            value(idata) = .TRUE.
                        else if ( isStringEqual(this%dataArray(keyIndex)%arrS(idata), "t", .FALSE.) ) then
                            value(idata) = .TRUE.
                        else if ( isStringEqual(this%dataArray(keyIndex)%arrS(idata), "false", .FALSE.) ) then
                            value(idata) = .FALSE.
                        else if ( isStringEqual(this%dataArray(keyIndex)%arrS(idata), "f", .FALSE.) ) then
                            value(idata) = .FALSE.
                        else
                            subLoopError = .FALSE.
                            write(*,*) "    [ERROR] getSLTableLogical, key, idata : ", trim(key), idata
                            write(*,*) "            converting error (string to logical) : ", parsedString(idata)%get()
                            if (errStop) stop
                        end if
                    enddo

                    if (subLoopError) errCorrect = .TRUE.

                end if

            else
                write(*,*) "    [ERROR] getSLTableLogicalArray, key : ", trim(key)
                write(*,*) "            data is not logical type."
                if (errStop) stop
            end if
        else
            write(*,*) "    [ERROR] getSLTableLogicalArray, key : ", trim(key)
            write(*,*) "            the size of data is zero."
            if (errStop) stop
        end if
    else
        call this%printSearchError(key, errStop)
    end if

    if (present(isCorrect)) isCorrect = errCorrect

end subroutine

subroutine reallocateSLTable(this, tableSize)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    integer, intent(in)                   :: tableSize

    !! Local Variables
    type(typString), allocatable          :: tmpKeys(:)
    type(typDataArray), allocatable       :: tmpDataArray(:)
    logical, allocatable                  :: tmpisKeyActive(:)
    integer :: ikey
    integer :: tmpNKey, tmpNTable


    if (tableSize.eq.0) then
        !!... destroy dynamic array
        Call this%destroy()

    else if (tableSize.ne.this%nTable) then

        allocate(tmpKeys(tableSize))
        allocate(tmpDataArray(tableSize))
        allocate(tmpisKeyActive(tableSize))

        tmpisKeyActive(:) = .FALSE.

        tmpNKey = this%nKey

        if (this%nKey.ge.tableSize) then
            tmpNKey = tableSize
        end if

        if (tableSize.lt.this%nTable) then

            do ikey = 1, tableSize
                tmpKeys(ikey)        = this%keys(ikey)
                tmpDataArray(ikey)   = this%dataArray(ikey)
                tmpisKeyActive(ikey) = this%isKeyActive(ikey)
            end do
        else

            do ikey = 1, this%nTable
                tmpKeys(ikey)        = this%keys(ikey)
                tmpDataArray(ikey)   = this%dataArray(ikey)
                tmpisKeyActive(ikey) = this%isKeyActive(ikey)
            end do

        end if

        Call this%destroy()

        this%nKey   = tmpNKey
        this%nTable = tableSize

        allocate(this%keys(this%nTable))
        allocate(this%dataArray(this%nTable))
        allocate(this%isKeyActive(this%nTable))

        this%isKeyActive = .FALSE.

        do ikey = 1, this%nTable
            this%keys(ikey)        = tmpKeys(ikey)
            this%dataArray(ikey)   = tmpDataArray(ikey)
            this%isKeyActive(ikey) = tmpisKeyActive(ikey)
        end do

    end if

end subroutine

subroutine printSLTable(this, tabSize)
    Implicit None
    class(typSLookUpTable), intent(in) :: this
    integer :: ikey, idata, nData
    Integer,optional :: tabSize
    integer :: iblk

    ! write(*,*) "LookUpTable : ", trim(this%name)

    if (present(tabSize)) then
        iblk = tabSize
    else
        iblk = DEFAULT_TAB_SIZE
    end if

    if (this%nKey.ge.1) then
        do ikey = 1, this%nKey

            if (this%isKeyActive(ikey)) then

            nData = this%dataArray(ikey)%nData

            !!... Write Number
            if (this%dataArray(ikey)%isNum) then
                if(this%dataArray(ikey)%isInt) then
                    if (nData.eq.1) then
                        write(*,1021) blankSpace(iblk),trim(this%keys(ikey)%get()), &
                                      this%dataArray(ikey)%arrI(1)
                    else
                        write(*,1020) blankSpace(iblk),trim(this%keys(ikey)%get()), nData
                        write(*,"(a,a)") blankSpace(iblk),"("
                        do idata = 1,nData
                            write(*,1022) blankSpace(iblk),this%dataArray(ikey)%arrI(idata)
                        enddo
                        write(*,"(a,a)") blankSpace(iblk),")"
                    end if
                else
                    if (nData.eq.1) then
                        write(*,1011) blankSpace(iblk),trim(this%keys(ikey)%get()), &
                                      this%dataArray(ikey)%arrR(1)
                    else
                        write(*,1010) blankSpace(iblk),trim(this%keys(ikey)%get()), nData
                        write(*,"(a,a)") blankSpace(iblk),"("
                        do idata = 1,nData
                            write(*,1012) blankSpace(iblk),this%dataArray(ikey)%arrR(idata)
                        enddo
                        write(*,"(a,a)") blankSpace(iblk),")"
                    end if
                endif
            end if

            !!... Write Character
            if (this%dataArray(ikey)%isStr) then
                if (nData.eq.1) then
                    write(*,1111) blankSpace(iblk),trim(this%keys(ikey)%get()), &
                                  trim(this%dataArray(ikey)%arrS(1)%get())
                else
                    write(*,1110) blankSpace(iblk),trim(this%keys(ikey)%get()), nData
                    write(*,"(a,a)") blankSpace(iblk),"("
                    do idata = 1,nData
                        write(*,1112) blankSpace(iblk),trim(this%dataArray(ikey)%arrS(idata)%get())
                    enddo
                    write(*,"(a,a)") blankSpace(iblk),")"
                end if
            end if

            !!... Write Logical
            if (this%dataArray(ikey)%isLog) then
                if (nData.eq.1) then
                    write(*,1211) blankSpace(iblk),trim(this%keys(ikey)%get()), &
                                  this%dataArray(ikey)%arrL(1)
                else
                    write(*,1210) trim(this%keys(ikey)%get()), nData
                    write(*,"(a,a)") blankSpace(iblk),"("
                    do idata = 1,nData
                        write(*,1212) this%dataArray(ikey)%arrL(idata)
                    enddo
                    write(*,"(a,a)") blankSpace(iblk),")"
                end if
            end if

            end if

        end do
    else
        write(*,"(a,a)") blankSpace(iblk),"    empty."
    end if


    1010 format(a,a,3x,"numVector",3x,i6)
    1011 format(a,a,3x,"num",f15.6)
    1012 format(a,f15.6)

    1020 format(a,a,3x,"intVector",3x,i6)
    1021 format(a,a,3x,"int",i15)
    1022 format(a,i15)

    1110 format(a,a,3x,"charVector",3x,a)
    1111 format(a,a,3x,"char",3x,"[ ",a," ]")
    1112 format(a,5x,"[ ",a," ]")

    1210 format(a,a,3x,"logVector",3x,l2)
    1211 format(a,a,3x,"log",3x,l2)
    1212 format(a,5x,l2)

end subroutine

subroutine destroySLTable(this)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    integer :: ikey

    do ikey = 1, this%nTable
        Call this%keys(ikey)%destroy()
        Call this%dataArray(ikey)%destroy()
    end do

    if (allocated(this%keys))        deallocate(this%keys)
    if (allocated(this%dataArray))   deallocate(this%dataArray)
    if (allocated(this%isKeyActive)) deallocate(this%isKeyActive)

    this%nKey   = 0
    this%nTable = 0

end subroutine

subroutine deleteSLTableData(this, key)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key

    integer :: keyIndex
    logical :: isExist

    Call this%searchKeyIndex(key, keyIndex, isExist)

    if (isExist) then
        this%isKeyActive(keyIndex) = .FALSE.
        Call this%keys(keyIndex)%destroy()
        Call this%dataArray(keyIndex)%destroy()
    endif

end subroutine

function keyExistSLTableData(this, key) result(isExist)
    Implicit None
    class(typSLookUpTable), intent(in)    :: this
    character(len=*), intent(in)          :: key
    logical                               :: isExist
    integer                               :: keyIndex
    Call searchKeyIndex(this, key, keyIndex, isExist)
end function

function isNumberSLTableData(this, key) result(isNumber)
    Implicit None
    class(typSLookUpTable), intent(in)    :: this
    character(len=*), intent(in)          :: key
    logical                               :: isNumber
    integer                               :: keyIndex
    logical                               :: isExist
    isNumber = .FALSE.
    Call searchKeyIndex(this, key, keyIndex, isExist)

    if (isExist) then
        isNumber = this%dataArray(keyIndex)%isNum
    else
        call this%printSearchError(key, .TRUE.)
    end if
end function

function isIntSLTableData(this, key) result(isInt)
    Implicit None
    class(typSLookUpTable), intent(in)    :: this
    character(len=*), intent(in)          :: key
    logical                               :: isInt
    integer                               :: keyIndex
    logical                               :: isExist

    isInt = .FALSE.
    Call searchKeyIndex(this, key, keyIndex, isExist)

    if (isExist) then
        isInt = this%dataArray(keyIndex)%isInt
    else
        call this%printSearchError(key, .TRUE.)
    end if

end function

function isCharSLTableData(this, key) result(isChar)
    Implicit None
    class(typSLookUpTable), intent(in)    :: this
    character(len=*), intent(in)          :: key
    logical                               :: isChar
    integer                               :: keyIndex
    logical                               :: isExist
    isChar = .FALSE.
    Call searchKeyIndex(this, key, keyIndex, isExist)
    if (isExist) then
        isChar = this%dataArray(keyIndex)%isStr
    else
        call this%printSearchError(key, .TRUE.)
    end if
end function

function isLogicalSLTableData(this, key) result(isLogical)
    Implicit None
    class(typSLookUpTable), intent(in)    :: this
    character(len=*), intent(in)          :: key
    logical                               :: isLogical
    integer                               :: keyIndex
    logical                               :: isExist
    isLogical = .FALSE.
    Call searchKeyIndex(this, key, keyIndex, isExist)
    if (isExist) then
        isLogical = this%dataArray(keyIndex)%isLog
    else
        call this%printSearchError(key, .TRUE.)
    end if
end function

subroutine searchKeyIndex(this, key, keyIndex, isExist)
    Implicit None
    class(typSLookUpTable), intent(in)    :: this
    character(len=*), intent(in)          :: key
    integer,intent(out)                   :: keyIndex
    logical,intent(out)                   :: isExist
    integer :: ikey

    !!... Optimization is needed !!!
    isExist = .FALSE.

    do ikey = 1, this%nKey
        if (this%isKeyActive(ikey)) then    !! Acceletate Search
            if (isStringEqual(trim(key), this%keys(ikey), this%strongSearch)) then
                isExist = .TRUE.
                keyIndex = ikey
            end if
        end if
    enddo

end subroutine


subroutine searchKeyIndexAndAdd(this, key, keyIndex, isExist)
    Implicit None
    class(typSLookUpTable), intent(in)    :: this
    character(len=*), intent(in)          :: key
    integer,intent(out)                   :: keyIndex
    logical,intent(out)                   :: isExist
    integer :: ikey

    !!... Optimization is needed !!!
    isExist = .FALSE.
    do ikey = 1, this%nKey
        if (this%isKeyActive(ikey)) then    !! Acceletate Search
            if (isStringEqual(trim(key), this%keys(ikey), this%strongSearch)) then
                isExist = .TRUE.
                keyIndex = ikey
            end if
        end if
    enddo

    if (isExist.EQV..FALSE.) then
        keyIndex = this%nKey + 1
    end if

end subroutine

subroutine printSearchError(this, key, errStop)
    Implicit None
    class(typSLookUpTable), intent(in) :: this
    Character(len=*), intent(in) :: key
    logical,optional :: errStop

    if (present(errStop)) then
        if (errStop.EQV..TRUE.) then
        write(*,*) "   [ERROR]   Unknown Key : ", key, " in LookUpTable ", trim(this%name)
        stop
        else
        write(*,*) "   [WARNING] Unknown Key : ", key, " in LookUpTable ", trim(this%name)
        end if
    else
        write(*,*) "   [WARNING] Unknown Key : ", key, " in LookUpTable ", trim(this%name)
    end if

end subroutine

subroutine copySLTable(origin, target)
Implicit None
    Class(typSLookUpTable), intent(inout) :: origin
    Type(typSLookUpTable), intent(in)     :: target
    integer :: idata

    Call origin%destroy()

    origin%nKey         = target%nKey
    origin%nTable       = target%nTable
    origin%name         = target%name
    origin%strongSearch = target%strongSearch

    if (origin%nTable.ge.1) then
        allocate(origin%keys(origin%nTable))
        allocate(origin%dataArray(origin%nTable))
        allocate(origin%isKeyActive(origin%nTable))

        do idata = 1, origin%nTable
            origin%keys(idata)        = target%keys(idata)
            origin%dataArray(idata)   = target%dataArray(idata)
            origin%isKeyActive(idata) = target%isKeyActive(idata)
        enddo

    endif

end subroutine

Subroutine addSLTable(this, target)
    Implicit None
    Class(typSLookUpTable), intent(inout) :: this
    Type(typSLookUpTable), intent(in)     :: target

    integer :: iKey
    Character(len=CHAR_LEN)             :: tmpKey

    Real(RP), allocatable               :: tmpRealArray(:)
    Integer, allocatable                :: tmpIntArray(:)
    Character(len=CHAR_LEN),allocatable :: tmpCharArray(:)
    logical,allocatable                 :: tmpLogicalArray(:)

    do iKey = 1, target%nKey
        tmpKey = target%getKey(iKey)

        if (target%isNumber(tmpKey)) then

            if (target%isInt(tmpKey)) then
                Call target%getIntArray(tmpKey, tmpIntArray)
                Call this%push(tmpKey, tmpIntArray)
            else
                Call target%getRealArray(tmpKey, tmpRealArray)
                Call this%push(tmpKey, tmpRealArray)
            end if

        else if (target%isCharacter(tmpKey)) then

            Call target%getCharArray(tmpKey, tmpCharArray)
            Call this%push(tmpKey, tmpCharArray)

        else if (target%isLogical(tmpKey)) then

            Call target%getLogicalArray(tmpKey, tmpLogicalArray)
            Call this%push(tmpKey, tmpLogicalArray)

        end if

    enddo

    if (allocated(tmpRealArray)) deallocate(tmpRealArray)
    if (allocated(tmpCharArray)) deallocate(tmpCharArray)
    if (allocated(tmpLogicalArray)) deallocate(tmpLogicalArray)


End Subroutine
