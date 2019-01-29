Subroutine testDictionary()
    Implicit None
    Character(len=CHAR_LEN) :: dictName

    ! Call testReadCharArray()

    ! Call testFileMakeDict()

    ! Call testMakeDict()

    ! Call mkSLTable(SLTable)
    !
    ! ! Call tmpDict%initialize(dictName)
    !
    ! Call tmpDict%initialize(dictName, SLTable)

    Call testDictPointer()

end subroutine

Subroutine testDictPointer()
    Implicit None
    type(typDictionaryPtr) :: dictPtr, subDict, dict2

    type(typSLookUpTable) :: SLTable

    character(len=CHAR_LEN) ::fileDir
    character(len=CHAR_LEN) ::fileName
    character(len=CHAR_LEN) ::fileExt
    character(len=CHAR_LEN) ::filePath

    integer :: iUnit, jUnit, ios
    logical :: isFileExist
    logical :: isFileOpen

    Character(len=CHAR_LEN) :: tmpChar
    integer :: ii

    real(rp), allocatable :: realArr(:)

    Call dictPtr%initialize("abc")

    Call dict2%initialize("asqf")

    write(*,*) dictPtr%ptrDict%dictID

    Call dict2%set("here", "qsjkldjq")

    Call dictPtr%set("abc", 1.0)

    Call dictPtr%set("def", (/1.0, 2.0, 3.0/))

    Call dictPtr%set("ghf", 1)

    Call dictPtr%set("ghfe", (/1, 2, 5/))

    Call dictPtr%set("ghfeada", "abd")

    Call dictPtr%addDict(dict2)

    Call dictPtr%print()

    ! Call subDict%print()

    ! Call dictPtr%ptrDict%sLTable%print()
    !
    ! Call dictDB(1)%SLTable%print()
    !
    ! Call mkSLTable(SLTable)
    !
    ! Call dictPtr%initialize("abc", SLTable)
    ! write(*,*) dictPtr%ptrDict%dictID
    !
    ! Call dictPtr%ptrDict%sLTable%print()
    !
    ! Call dictDB(2)%SLTable%print()

    fileDir  = "./"
    fileName = "dict"
    fileExt  = ""

    Call dictPtr%initialize(fileDir, fileName, fileExt)

    Call dictPtr%print()

    Call dictPtr%writeFile("./dict.dict")

    ! write(*,*) dictPtr%getReal("varR")
    !
    ! ! realArr = dictPtr%getRealArray("arrR")
    !
    ! realArr = dictPtr%getRealArray("arrR")
    !
    ! write(*,*) realArr
    !
    ! subDict = dictPtr%subDict("dict1")
    !
    ! Call subDict%print()
    !
    ! realArr = dictPtr%getRealOrDefault("arrR1", 3.0_RP)
    !
    ! write(*,*) realArr

    ! fileDir  = ""
    ! fileName = "testDict"
    ! fileExt  = ""
    !
    ! filePath = trim(fileDir)//trim(fileName)//trim(fileExt)
    !
    ! iUnit = 100
    !
    ! open(unit   = iUnit,      &
    !      file   = filePath,   &
    !      status = 'old', &
    !      iostat = ios)
    !
    ! inquire(unit   = 101, &
    !         opened = isFileOpen)
    !
    ! write(*,*) isFileOpen
    !
    ! inquire(file = filePath, &
    !         exist = isFileExist, &
    !         opened = isFileOpen, &
    !         number = jUnit)
    !
    ! write(*,*) trim(filePath)
    ! write(*,*) isFileExist
    ! write(*,*) isFileOpen
    ! write(*,*) jUnit
    !
    ! tmpChar = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    !
    ! do ii = 1,len_trim(tmpChar)
    !     write(*,*) tmpChar(ii:ii), iachar(tmpChar(ii:ii))
    ! enddo
    !
    ! tmpChar = "abcdefghijklmnopqrstuwxyz"

End Subroutine

Subroutine testFileMakeDict()
    Implicit None

    type(typDictionary), pointer :: dict

    type(typSLookUpTable), pointer :: slTable

    integer :: fid
    character(len=CHAR_LEN) ::fileDir
    character(len=CHAR_LEN) ::fileName
    character(len=CHAR_LEN) ::fileExt
    character(len=CHAR_LEN) ::filePath
    integer :: ios

    integer :: nLine, iLine, subNline
    character(len = fileLineLength) :: tmpLine
    character(len = fileLineLength),allocatable :: charArray(:)
    character(len = fileLineLength),allocatable :: subCharArray(:)
    character(len = fileLineLength),allocatable :: tmpCharArray(:)

    integer :: id

    fid      = 101

    fileDir  = ""
    fileName = "testDict"
    fileExt  = ""

    filePath = trim(fileDir)//trim(fileName)//trim(fileExt)

    open(unit   = fid,        &
         file   = filePath,   &
         status = 'old', &
         iostat = ios)

    ! Call dict%initialize(fileName, fid)

    Call readMakeCharArrayFromFile(fid, CharArray, nLine, " ", " ")

    if (nLine.ge.1) then
        Call initDictDB_NameCharArray(fileName, charArray, id)

        Call dictDB(1)%sLTable%print()
        Call dictDB(2)%sLTable%print()
        Call dictDB(3)%sLTable%print()
        Call dictDB(4)%sLTable%print()
        Call dictDB(5)%sLTable%print()

        Call dictDB(1)%subDictTable%print()
        Call dictDB(2)%subDictTable%print()

        Call dictDB(3)%subDictTable%print()
        Call dictDB(5)%subDictTable%print()

    end if

End Subroutine

Subroutine testReadCharArray()
    Implicit None
    integer :: fid
    character(len=CHAR_LEN) ::fileDir
    character(len=CHAR_LEN) ::fileName
    character(len=CHAR_LEN) ::fileExt
    character(len=CHAR_LEN) ::filePath
    integer :: ios
    character(len = fileLineLength),allocatable :: charArray(:)
    character(len = fileLineLength),allocatable :: subCharArray(:)
    integer :: nLine, iLine,jLine
    integer :: subNline

    fid      = 101

    fileDir  = ""
    fileName = "testDict"
    fileExt  = ""

    filePath = trim(fileDir)//trim(fileName)//trim(fileExt)

    open(unit   = fid,        &
         file   = filePath,   &
         status = 'old', &
         iostat = ios)

    Call readMakeCharArrayFromFile(fid, CharArray, nLine, " ", " ")

    do iLine = 1, nLine
        write(*,*) iLine, trim(charArray(iLine))
    enddo

    Call readMakeCharArrayFromCharArray(charArray, subCharArray, subNline, "{","}", .true.)

    do jLine = 1, subNline
        write(*,*) jLine, trim(subCharArray(jLine))
    enddo

End Subroutine

Subroutine testShowIndex()
    Implicit None
    integer :: fid

    character(len=CHAR_LEN) ::fileDir
    character(len=CHAR_LEN) ::fileName
    character(len=CHAR_LEN) ::fileExt
    character(len=CHAR_LEN) ::filePath

    character(len=fileLineLength) :: tmpLine, modLine

    integer :: ios

    integer :: nArg
    character(len = CHAR_LEN), allocatable :: cArg(:), scArg(:), lcArg(:)
    integer, allocatable                   :: iArg(:)
    real(RP), allocatable                  :: rArg(:)
    logical,allocatable                    :: numFlag(:)
    integer,allocatable                    :: scFlag(:)
    logical :: iflag

    fid      = 101

    fileDir  = ""
    fileName = "testDict"
    fileExt  = ""

    filePath = trim(fileDir)//trim(fileName)//trim(fileExt)


    open(unit   = fid,        &
         file   = filePath,   &
         status = 'old', &
         iostat = ios)

    DO
        read(fid, "(a)", iostat = ios) tmpLine
        IF (ios < 0) EXIT
        ! write(*,*) strHeader(trim(tmpLine)), findLineEnd(tmpLine), "    ",trim(tmpLine)

        modLine = insertSpaceSpecialCharacter(tmpLine)
        Call splitLine(modLine, nArg, cArg, scArg, lcArg, iArg, rArg, numFlag, scFlag, iflag)

        if (nArg.ge.1) write(*,*) nArg, trim(modLine)
        if (nArg.ge.1) write(*,*) numFlag, scFlag
        if (nArg.ge.1) write(*,*) "----------------------------"

    END DO

    close(fid)

End Subroutine

Subroutine mkSLTable(SLTable)
    Type(typSLookUpTable) :: SLTable

    Call SLTable%initialize()

    Call SLTable%push("a", 123)
    Call SLTable%push("b", "is a")

End Subroutine
