Subroutine initialize_FileIO(this, fileDir, fileName, ext, fileStatus)
    Implicit None
    Class(typFileIO), intent(inout) :: this
    Character(len=*), intent(in)    :: fileDir
    Character(len=*), intent(in)    :: fileName
    Character(len=*), intent(in)    :: ext
    Character(len=*), intent(in)    :: fileStatus
    logical :: isFileExist, isFileOpen

    this%fileDir    = ""
    this%fileName   = ""
    this%ext        = ""
    this%fileStatus = ""

    this%fileDir    = fileDir
    this%fileName   = fileName
    this%ext        = ext
    this%fileStatus = fileStatus

    Call searchBlankfileIndex(this%fileIndex)

    this%filePath = trim(this%fileDir)//trim(this%fileName)//this%ext

    inquire(file = this%filePath, &
            exist = isFileExist, &
            opened = isFileOpen)

    if ( (isCharCharEqual(this%fileStatus, "replace")).OR.&
         (isCharCharEqual(this%fileStatus, "old")) ) then

         Open(unit = this%fileIndex, &
              file = trim(this%filePath), &
              status = trim(this%fileStatus))

    else if (isCharCharEqual(this%fileStatus, "new")) then

        if (isFileExist.EQV..FALSE.) then
            write(*,*) " "
            write(*,*) "    [ERROR] file not exist. file : "
            write(*,*) "file : ", trim(this%filePath)
            write(*,*) " "
            stop
        end if

        if (isFileOpen.EQV..FALSE.) then
            write(*,*) " "
            write(*,*) "    [ERROR] file is already opened."
            write(*,*) "  file : ", trim(this%filePath)
            write(*,*) "  fid  : ", this%fileIndex
            stop
        end if

        Open(unit = this%fileIndex, &
             file = trim(this%filePath), &
             status = trim(this%fileStatus))
    else

        Open(unit = this%fileIndex, &
             file = trim(this%filePath))

    end if

End Subroutine

Subroutine destroyFileIO(this)
    Implicit None
    Class(typFileIO), intent(inout) :: this

    close(this%fileIndex)

    this%fileDir    = ""
    this%fileName   = ""
    this%ext        = ""
    this%fileStatus = ""
    this%filePath   = ""

    this%isOpened = .FALSE.

End Subroutine

subroutine searchBlankfileIndex(fileIndex)
    Implicit None
    Integer, parameter           :: integerStep = 31
    Integer, intent(out)         :: fileIndex
    logical :: isFileOpen
    fileIndex = 1231
    do
        fileIndex = fileIndex + integerStep
        inquire(unit   = fileIndex, &
                opened = isFileOpen)
        if (isFileOpen.EQV..FALSE.) exit
    enddo

End Subroutine
