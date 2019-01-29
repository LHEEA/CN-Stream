Subroutine initDict_Name(this, dictName)
    Implicit None
    Class(typDictionaryPtr), intent(inout) ::  this
    Character(len=*), intent(in)         :: dictName
    !! ------------------------------------------------------------------------
    integer :: dictID

    nullify(this%ptrDict)
    Call initializeDictDataBase(dictName, dictID)

    this%ptrDict => dictDB(dictID)

End Subroutine

Subroutine initDict_NameLookUpTable(this, dictName, sLTable)
    Implicit None
    Class(typDictionaryPtr), intent(inout)      :: this
    Character(len=*), intent(in)                :: dictName
    Type(typSLookUpTable), intent(in)           :: sLTable
    !! ------------------------------------------------------------------------

    Call this%initialize(dictName)

    this%ptrDict%sLTable  = sLTable
    this%ptrDict%sLTable%name = trim(dictName)//"_subDict"

End Subroutine

Subroutine initDict_File(this, fileDir, fileName, ext)
    Implicit None
    Class(typDictionaryPtr), intent(inout)  :: this
    Character(len=*), intent(in)            :: fileDir
    Character(len=*), intent(in)            :: fileName
    Character(len=*),optional               :: ext
    !! ------------------------------------------------------------------------
    Type(typFileIO) :: dictFileIO
    character(len = fileLineLength),allocatable :: charArray(:)
    integer :: nLine
    integer :: dictID
    !! ------------------------------------------------------------------------
    Call this%initialize(fileName)

    Call dictFileIO%initialize(fileDir, fileName, ext, "old")

    Call readMakeCharArrayFromFile(dictFileIO%fileIndex, charArray, nLine, " ", " ")

    Call initDictDB_NameCharArray(fileName, charArray, dictID)

    this%ptrDict => dictDB(dictID)

    Call dictFileIO%destroy()

End Subroutine

Recursive subroutine printDictionary(this, tabSize)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    integer :: idict
    integer,optional :: tabSize
    integer :: iblk
    Character(CHAR_LEN) :: subDictName

    if (present(tabSize)) then
        iblk = tabSize
    else
        iblk = 0
    end if

    !!... Write Dictionary Name
    write(*,"(a,a)") blankSpace(iblk), trim(this%ptrDict%dictName)

    write(*,"(a,a)") blankSpace(iblk),"{"
    !!... Write Dictionary Member Data
    Call this%ptrDict%sLTable%print(iblk + DEFAULT_TAB_SIZE)

    !!... Write SubDictionary
    do idict = 1, this%ptrDict%subDictTable%nKey
        if ( this%ptrDict%subDictTable%isKeyActive(idict) ) then
            write(*,"(a)")
            subDictName = this%ptrDict%subDictTable%keys(idict)%get()
            Call printDictionary( this%subDict(subDictName), iblk + DEFAULT_TAB_SIZE)
            subDictName = ""
        end if
    enddo

    write(*,"(a,a)") blankSpace(iblk),"}"

    ! write(*,*) " "
    ! write(*,*) "---------------------------------------------------------------"
    ! write(*,*) " "
    ! write(*,*) " Dictionary : ", trim(this%ptrDict%dictName)
    ! write(*,*) " "
    ! write(*,*) "---------------------------------------------------------------"
    ! write(*,*) " "
    ! write(*,*) "  - Data : "
    ! write(*,*) " "
    ! Call this%ptrDict%sLTable%print()
    ! write(*,*) " "
    ! write(*,*) "  - Sub Dictionary : "
    ! write(*,*) " "
    ! Call this%ptrDict%subDictTable%print()
    ! write(*,*) " "
    ! write(*,*) "---------------------------------------------------------------"

End Subroutine

function subDictionaryPtr(this, subDictName) result(ptrSubDict)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: subDictName
    type(typDictionaryPtr)               :: ptrSubDict
    integer :: subDictID

    Call this%ptrDict%subDictTable%getInt(subDictName, subDictID)

    ptrSubDict%ptrDict => dictDB(subDictID)

end function

function nData_Dict(this) result (nVar)
    Class(typDictionaryPtr), intent(in)  :: this
    integer                              :: nVar, ikey
    nVar = 0
    !!... Write Data
    do ikey = 1, this%ptrDict%sLTable%nKey
        if (this%ptrDict%sLTable%isKeyActive(ikey)) then
            nVar = nVar + 1
        end if
    enddo
end function

function nSubDict_Dict(this) result (nVar)
    Class(typDictionaryPtr), intent(in)  :: this
    integer                              :: nVar, ikey
    nVar = 0
    !!... Write Data
    do ikey = 1, this%ptrDict%subDictTable%nKey
        if (this%ptrDict%subDictTable%isKeyActive(ikey)) then
            nVar = nVar + 1
        end if
    enddo
end function

function getDictKey(this, ivar) result(value)
    Class(typDictionaryPtr), intent(in)  :: this
    integer, intent(in)                  :: ivar
    Character(len=CHAR_LEN)              :: value
    integer                              :: jkey, ikey
    logical :: isFoundKey
    value =""
    isFoundKey = .FALSE.
    jkey = 0
    !!... Write Data
    do ikey = 1, this%ptrDict%sLTable%nKey
        if (this%ptrDict%sLTable%isKeyActive(ikey)) then
            jkey = jkey + 1
        end if
        if (jkey.eq.ivar) then
            value = this%ptrDict%sLTable%getKey(ivar)
            isFoundKey = .TRUE.
        end if
    enddo

    if ( .not.(isFoundKey) ) then
        write(*,*) "    [ERROR] getDictKey::key is not found. Index Error"
        write(*,*) "    ivar : ", ivar
        write(*,*) "    nKey : ", this%ptrDict%subDictTable%nKey
        stop
    end if

End Function

function getDictSubDictName(this, ivar) result(value)
    Class(typDictionaryPtr), intent(in)  :: this
    integer, intent(in)                  :: ivar
    Character(len=CHAR_LEN)              :: value
    integer                              :: jkey, ikey
    logical :: isFoundKey
    value =""
    isFoundKey = .FALSE.
    jkey = 0
    !!... Write Data
    do ikey = 1, this%ptrDict%subDictTable%nKey
        if (this%ptrDict%subDictTable%isKeyActive(ikey)) then
            jkey = jkey + 1
        end if
        if (jkey.eq.ivar) then
            value = this%ptrDict%subDictTable%getKey(ivar)
            isFoundKey = .TRUE.
        end if
    enddo

    if ( .not.(isFoundKey) ) then
        write(*,*) "    [ERROR] getDictKey::key is not found. Index Error"
        write(*,*) "    ivar : ", ivar
        write(*,*) "    nKey : ", this%ptrDict%subDictTable%nKey
        stop
    end if

End Function

Function getDictReal(this, key, ivar) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    integer,optional                     :: ivar
    real(rp)                             :: value

    Call this%ptrDict%sLTable%getReal(key, value, ivar)

End Function

Function getDictRealArray(this, key) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    real(rp),allocatable                 :: value(:)

    Call this%ptrDict%sLTable%getRealArray(key, value)

End Function

Function getDictInt(this, key, ivar) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    integer,optional                     :: ivar
    integer                              :: value

    Call this%ptrDict%sLTable%getInt(key, value, ivar)

End Function

Function getDictIntArray(this, key) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    integer, allocatable                 :: value(:)

    Call this%ptrDict%sLTable%getIntArray(key, value)

End Function

Function getDictChar(this, key, ivar) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    integer,optional                     :: ivar
    Character(len=CHAR_LEN)              :: value

    Call this%ptrDict%sLTable%getChar(key, value, ivar)

End Function

Function getDictCharArray(this, key) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    Character(len=CHAR_LEN),allocatable  :: value(:)

    Call this%ptrDict%sLTable%getCharArray(key, value)

End Function

Function getDictString(this, key, ivar) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    integer,optional                     :: ivar
    type(typString)                      :: value

    Call this%ptrDict%sLTable%getString(key, value, ivar)

End Function

Function getDictStringArray(this, key) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    type(typString),allocatable          :: value(:)

    Call this%ptrDict%sLTable%getStringArray(key, value)

End Function

Function getDictLogical(this, key, ivar) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    integer,optional                     :: ivar
    logical                              :: value

    Call this%ptrDict%sLTable%getLogical(key, value, ivar)

End Function

Function getDictLogicalArray(this, key) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    logical,allocatable     :: value(:)

    Call this%ptrDict%sLTable%getLogicalArray(key, value)

End Function

Function getDictRealOrDefault(this, key, defaultValue, ivar) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    real(rp), intent(in)                 :: defaultValue
    integer,optional                     :: ivar
    real(rp)                             :: value

    if (this%ptrDict%sLTable%keyExist(key)) then
        Call this%ptrDict%sLTable%getReal(key, value, ivar)
    else
        value = defaultValue
    end if

End Function

Function getDictRealArrayOrDefault(this, key, defaultValue) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    real(rp), dimension(:),intent(in)    :: defaultValue
    real(rp),allocatable                 :: value(:)

    if (this%ptrDict%sLTable%keyExist(key)) then
        Call this%ptrDict%sLTable%getRealArray(key, value)
    else
        value = defaultValue
    end if

End Function

Function getDictIntOrDefault(this, key, defaultValue, ivar) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    integer,intent(in)                   :: defaultValue
    integer,optional                     :: ivar
    integer                              :: value

    if (this%ptrDict%sLTable%keyExist(key)) then
        Call this%ptrDict%sLTable%getInt(key, value, ivar)
    else
        value = defaultValue
    end if

End Function

Function getDictIntArrayOrDefault(this, key, defaultValue) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    integer,dimension(:),intent(in)      :: defaultValue
    integer, allocatable                 :: value(:)

    if (this%ptrDict%sLTable%keyExist(key)) then
        Call this%ptrDict%sLTable%getIntArray(key, value)
    else
        value = defaultValue
    end if

End Function

Function getDictCharOrDefault(this, key, defaultValue, ivar) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    Character(len=*),intent(in)          :: defaultValue
    integer,optional                     :: ivar
    Character(len=CHAR_LEN)              :: value

    if (this%ptrDict%sLTable%keyExist(key)) then
        Call this%ptrDict%sLTable%getChar(key, value, ivar)
    else
        value = ""
        value = defaultValue
    end if

End Function

Function getDictCharArrayOrDefault(this, key, defaultValue) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    Character(len=*),dimension(:),intent(in) :: defaultValue
    Character(len=CHAR_LEN),allocatable  :: value(:)

    if (this%ptrDict%sLTable%keyExist(key)) then
        Call this%ptrDict%sLTable%getCharArray(key, value)
    else
        value = ""
        value = defaultValue
    end if

End Function

Function getDictStringOrDefault(this, key, defaultValue, ivar) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    type(typString),intent(in)           :: defaultValue
    integer,optional                     :: ivar
    type(typString)                      :: value

    if (this%ptrDict%sLTable%keyExist(key)) then
        Call this%ptrDict%sLTable%getString(key, value, ivar)
    else
        value = defaultValue
    end if

End Function

Function getDictStringArrayOrDefault(this, key, defaultValue) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    type(typString),dimension(:), intent(in) :: defaultValue
    type(typString),allocatable          :: value(:)

    if (this%ptrDict%sLTable%keyExist(key)) then
        Call this%ptrDict%sLTable%getStringArray(key, value)
    else
        value = defaultValue
    end if

End Function

Function getDictLogicalOrDefault(this, key, defaultValue, ivar) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    logical, intent(in)                  :: defaultValue
    integer,optional                     :: ivar
    logical                              :: value

    if (this%ptrDict%sLTable%keyExist(key)) then
        Call this%ptrDict%sLTable%getLogical(key, value, ivar)
    else
        value = defaultValue
    end if

End Function

Function getDictLogicalArrayOrDefault(this, key, defaultValue) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    logical, dimension(:), intent(in)    :: defaultValue
    logical,allocatable                  :: value(:)

    if (this%ptrDict%sLTable%keyExist(key)) then
        Call this%ptrDict%sLTable%getLogicalArray(key, value)
    else
        value = defaultValue
    end if

End Function

Subroutine setDictReal(this, key, value)
    Implicit None
    Class(typDictionaryPtr), intent(inout)  :: this
    Character(len=*),intent(in)          :: key
    Real, intent(in)                     :: value
    Call this%ptrDict%sLTable%push(key, value)
End Subroutine

Subroutine setDictRealRP(this, key, value)
    Implicit None
    Class(typDictionaryPtr), intent(inout)  :: this
    Character(len=*),intent(in)          :: key
    Real(RP), intent(in)                 :: value
    Call this%ptrDict%sLTable%push(key, value)
End Subroutine

Subroutine setDictDble(this, key, value)
    Implicit None
    Class(typDictionaryPtr), intent(inout)  :: this
    Character(len=*),intent(in)          :: key
    Double Precision, intent(in)                 :: value
    Call this%ptrDict%sLTable%push(key, real(value,rp))
End Subroutine

Subroutine setDictRealArray(this, key, value)
    Implicit None
    Class(typDictionaryPtr), intent(inout)  :: this
    Character(len=*),intent(in)          :: key
    Real, dimension(:), intent(in)       :: value
    Call this%ptrDict%sLTable%push(key, value)
End Subroutine

Subroutine setDictRealRPArray(this, key, value)
    Implicit None
    Class(typDictionaryPtr), intent(inout)  :: this
    Character(len=*),intent(in)          :: key
    Real(RP), dimension(:), intent(in)   :: value
    Call this%ptrDict%sLTable%push(key, value)
End Subroutine

Subroutine setDictDbleArray(this, key, value)
    Implicit None
    Class(typDictionaryPtr), intent(inout)  :: this
    Character(len=*),intent(in)          :: key
    Double Precision, dimension(:), intent(in) :: value
    Call this%ptrDict%sLTable%push(key, real(value,rp))
End Subroutine

Subroutine setDictInt(this, key, value)
    Implicit None
    Class(typDictionaryPtr), intent(inout)  :: this
    Character(len=*),intent(in)          :: key
    Integer, intent(in)                     :: value
    Call this%ptrDict%sLTable%push(key, value)
End Subroutine

Subroutine setDictIntArray(this, key, value)
    Implicit None
    Class(typDictionaryPtr), intent(inout)  :: this
    Character(len=*),intent(in)          :: key
    Integer, dimension(:), intent(in)       :: value
    Call this%ptrDict%sLTable%push(key, value)
End Subroutine

Subroutine setDictChar(this, key, value)
    Implicit None
    Class(typDictionaryPtr), intent(inout)  :: this
    Character(len=*),intent(in)          :: key
    character(len=*), intent(in)         :: value
    Call this%ptrDict%sLTable%push(key, value)
End Subroutine

Subroutine setDictCharArray(this, key, value)
    Implicit None
    Class(typDictionaryPtr), intent(inout)  :: this
    Character(len=*),intent(in)          :: key
    character(len=CHAR_LEN),dimension(:), intent(in)         :: value
    Call this%ptrDict%sLTable%push(key, value)
End Subroutine

Subroutine setDictLogical(this, key, value)
    Implicit None
    Class(typDictionaryPtr), intent(inout)  :: this
    Character(len=*),intent(in)          :: key
    logical, intent(in)          :: value
    Call this%ptrDict%sLTable%push(key, value)
End Subroutine

Subroutine setDictLogicalArray(this, key, value)
    Implicit None
    Class(typDictionaryPtr), intent(inout)  :: this
    Character(len=*),intent(in)          :: key
    logical, dimension(:), intent(in)     :: value
    Call this%ptrDict%sLTable%push(key, value)
End Subroutine

Subroutine setDictDict(this, subDict)
    Implicit None
    Class(typDictionaryPtr), intent(inout)  :: this
    Type(typDictionaryPtr), intent(in)   :: subDict

    if (subDict%ptrDict%dictID.ge.1) then
        Call this%ptrDict%subDictTable%push(subDict%ptrDict%dictName, subDict%ptrDict%dictID)
    else
        write(*,*) "    [ERROR] setSubDict(subDict), subDict is not stored."
        write(*,*) "        subDict Name : ", subDict%ptrDict%dictName
        write(*,*) "        subDict ID   : ", subDict%ptrDict%dictID
        stop
    end if

End Subroutine

Subroutine addDictDict(this, from)
    Implicit None
    Class(typDictionaryPtr), intent(inout)  :: this
    Type(typDictionaryPtr), intent(in)   :: from

    if (from%ptrDict%dictID.ge.1) then
        Call this%ptrDict%addDict(from%ptrDict)
    else
        write(*,*) "    [ERROR] addDict(fromDict), fromDict is not stored."
        write(*,*) "        fromDict Name : ", from%ptrDict%dictName
        write(*,*) "        fromDict ID   : ", from%ptrDict%dictID
        stop
    end if

End Subroutine

Subroutine writeDict2File(this, filePath)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*), intent(in)         :: filePath
    Type(typFileIO)                      :: writeFileIO

    Character(len=CHAR_LEN)                :: fileDir
    Character(len=CHAR_LEN)                :: fileName
    Character(len=CHAR_LEN)                :: fileExt

    integer :: fid

    Call separateFilePath(filePath, fileDir, fileName, fileExt)

    if (trim(fileName).eq."") then
        write(*,*) "    [ERROR] writeDictFile(filePath)"
        write(*,*) "        No file name is given : "
        write(*,*) "        file path : ", trim(filePath)
        write(*,*) "        file dir  : ", trim(fileDir)
        write(*,*) "        file name : ", trim(fileName)
        write(*,*) "        file ext  : ", trim(fileExt)
        stop
    end if

    if (len(trim(fileName)).ge.1) then
    if (fileName(1:1).eq.".") then
        write(*,*) "    [ERROR] writeDictFile(filePath)"
        write(*,*) "        Wrong file name is given : "
        write(*,*) "        file path : ", trim(filePath)
        write(*,*) "        file dir  : ", trim(fileDir)
        write(*,*) "        file name : ", trim(fileName)
        write(*,*) "        file ext  : ", trim(fileExt)
        stop
    end if
    end if

    Call writeFileIO%initialize(fileDir, fileName, fileExt, "replace")

    fid = writeFileIO%fileIndex

    Call writeDictFileHeader(fid)

    Call writeDict2FileChar(this, fid, "")

    Call writeFileIO%destroy()

End Subroutine

Recursive Subroutine writeDict2FileChar(this, fileIndex, blankTab)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Integer, intent(in) :: fileIndex
    Character(len=*), intent(in) :: blankTab
    !----------------------------------------------------------------
    character(len=CHAR_LEN)               :: key, dictName
    Type(typDataArray)                    :: tmpDataArray
    integer                               :: ikey, idict, ivar
    character(len=CHAR_LEN)               :: tmpChar

    write(fileIndex,1000) ""

    !!... Write Data
    do ikey = 1, this%ptrDict%sLTable%nKey
        if (this%ptrDict%sLTable%isKeyActive(ikey)) then

            key          = this%ptrDict%sLTable%getKey(ikey)
            tmpDataArray = this%ptrDict%sLTable%getDataArray(ikey)

            if (tmpDataArray%nData.ge.1) then
                if (tmpDataArray%nData.eq.1) then
                    if (tmpDataArray%isNum) then
                        if (tmpDataArray%isInt) then
                            write(fileIndex, 1002) blankTab, trim(key), tmpDataArray%arrI(1),";"
                            write(fileIndex,1000) ""
                        else
                            write(fileIndex, 1001) blankTab, trim(key), tmpDataArray%arrR(1),";"
                            write(fileIndex,1000) ""
                        end if
                    else if (tmpDataArray%isStr) then
                        tmpChar = tmpDataArray%arrS(1)%get()
                        write(fileIndex, 1003) blankTab, trim(key), trim(tmpChar),";"
                        write(fileIndex,1000) ""
                        tmpChar = ""
                    else if (tmpDataArray%isLog) then
                        if (tmpDataArray%arrL(1).EQV..TRUE.) then
                            write(fileIndex, 1003) blankTab, trim(key), "TRUE",";"
                        else
                            write(fileIndex, 1003) blankTab, trim(key), "FALSE",";"
                        end if
                        write(fileIndex,1000) ""
                    end if
                else

                    write(fileIndex, 1000) blankTab, trim(key)
                    write(fileIndex, 1000) blankTab, "("

                    if (tmpDataArray%isNum) then
                        if (tmpDataArray%isInt) then
                            do ivar = 1, tmpDataArray%nData
                                write(fileIndex, 1012) blankTab, tmpDataArray%arrI(ivar)
                            enddo
                        else
                            do ivar = 1, tmpDataArray%nData
                                write(fileIndex, 1011) blankTab, tmpDataArray%arrR(ivar)
                            enddo
                        endif

                    else if (tmpDataArray%isStr) then
                        do ivar = 1, tmpDataArray%nData
                            tmpChar = tmpDataArray%arrS(ivar)%get()
                            write(fileIndex, 1000) blankTab, trim(tmpChar)
                            tmpChar = ""
                        enddo

                    else if (tmpDataArray%isLog) then
                        do ivar = 1, tmpDataArray%nData
                            if (tmpDataArray%arrL(ivar).EQV..TRUE.) then
                                write(fileIndex, 1000) blankTab, "TRUE"
                            else
                                write(fileIndex, 1000) blankTab, "FALSE"
                            end if
                        enddo

                    end if

                    write(fileIndex, 1000) blankTab, ");"
                    write(fileIndex,1000) ""

                end if

            end if

        end if
    enddo

    !!... Write SubDictionary


    !!... Write Data
    do ikey = 1, this%ptrDict%subDictTable%nKey
        if (this%ptrDict%subDictTable%isKeyActive(ikey)) then
            dictName = this%ptrDict%subDictTable%getKey(ikey)

            write(fileIndex, 1000) blankTab, trim(dictName)
            write(fileIndex, 1000) blankTab, "{"
            Call writeDict2FileChar(this%subDict(dictName), fileIndex,blankTab//"    ")
            write(fileIndex, 1000) blankTab, "}"
            write(fileIndex,1000) ""

        end if
    enddo

    1000 format(a,a)
    1001 format(a,a,5x,f16.8,a)
    1002 format(a,a,5x,i10,a)
    1003 format(a,a,5x,a,a)

    1011 format(a,f16.8)
    1012 format(a,i10)


End Subroutine
