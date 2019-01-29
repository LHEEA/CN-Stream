function isStringStringEqual(orgin, target, isStrongCompare) result(isEqual)
    Implicit None
    class(typString), intent(in) :: orgin
    type(typString), intent(in)  :: target
    logical, intent(in),optional :: isStrongCompare
    logical :: isEqual
    isEqual = isCharCharEqual(orgin%str_, target%str_, isStrongCompare)
end function

function isStringCharEqual(orgin, target, isStrongCompare) result(isEqual)
    Implicit None
    class(typString), intent(in) :: orgin
    Character(len=*), intent(in)  :: target
    logical, intent(in),optional :: isStrongCompare
    logical :: isEqual
    isEqual = isCharCharEqual(orgin%str_, target, isStrongCompare)
end function

function isCharStringEqual(orgin, target, isStrongCompare) result(isEqual)
    Implicit None
    Character(len=*), intent(in)  :: orgin
    class(typString), intent(in) :: target
    logical, intent(in),optional :: isStrongCompare
    logical :: isEqual
    isEqual = isCharCharEqual(orgin, target%str_, isStrongCompare)
end function

function isCharCharEqual(orgin, target, isStrongCompare) result(isEqual)
    Implicit None
    Character(len=*), intent(in) :: orgin
    Character(len=*), intent(in) :: target
    logical, intent(in),optional :: isStrongCompare
    logical :: isEqual

    isEqual = .FALSE.

    if (present(isStrongCompare)) then
        if (isStrongCompare) then
            if (trim(orgin).eq.trim(target)) isEqual = .TRUE.
        else
            if (trim(lowerChar(orgin)).eq.trim( lowerChar(target))) isEqual = .TRUE.
        end if
    else
        if (trim(lowerChar(orgin)).eq.trim( lowerChar(target))) isEqual = .TRUE.
    end if

end function

subroutine parseStringString(this, delims, args, nargs)
    Implicit None
    class(typString), intent(in) :: this
    character(len=*)                :: delims
    type(typString),allocatable     :: args(:)
    integer, intent(out)            :: nargs
    character(len=CHAR_LEN),allocatable :: tmpArgs(:)
    integer :: istr
    Call parseCharChar(this%str_, delims, tmpArgs, nargs)

    if (allocated(args)) deallocate(args)
    allocate(args(nargs))

    do istr = 1, nargs
        Call args(istr)%set(tmpArgs(istr),.TRUE.)
    enddo

    if (allocated(tmpArgs)) deallocate(tmpArgs)

end subroutine

subroutine parseStringChar(this, delims, args, nargs)
    Implicit None
    class(typString), intent(in) :: this
    character(len=*)                :: delims
    character(len=*),allocatable    :: args(:)
    integer, intent(out)            :: nargs

    if (allocated(args)) deallocate(args)
    Call parseCharChar(this%str_, delims, args, nargs)

end subroutine

subroutine parseCharString(str, delims, args, nargs)
    Implicit None
    character(len=*) :: str,delims
    type(typString),allocatable     :: args(:)
    integer, intent(out)            :: nargs
    character(len=CHAR_LEN),allocatable :: tmpArgs(:)
    integer :: istr

    Call parseCharChar(str, delims, tmpArgs, nargs)

    if (allocated(args)) deallocate(args)
    allocate(args(nargs))

    do istr = 1, nargs
        Call args(istr)%set(tmpArgs(istr),.TRUE.)
    enddo

    if (allocated(tmpArgs)) deallocate(tmpArgs)

end subroutine

subroutine parseCharCharOne(str, delims, args, nargs)
    Implicit None
    character(len=*) :: str,delims
    character(len=*),allocatable :: args(:)
    integer, intent(out)         :: nargs
    !! .. Local Variables
    integer :: nDelims
    integer :: idel, j
    !!
    character(len=2*len_trim(str)) :: tmpStr

    tmpStr = ""
    tmpStr = str

    nDelims = len_trim(delims)
    if (nDelims.ge.1) then
        do idel = 1, nDelims
            Call parseCharChar(tmpStr, delims(idel:idel), args, nargs)

            tmpStr = ""
            do j = 1, nargs
                tmpStr = trim(tmpStr)//" "//trim(args(j))
            enddo
        enddo
        Call parseCharChar(tmpStr, " ", args, nargs)
    end if

endSubroutine

subroutine parseCharChar(str, delims, args, nargs)
    !
    !  Dr. George Benthiem's string subroutine
    !
    !       See. http://gbenthien.net/about.html
    !
    ! Parses the string 'str' into arguments args(1), ..., args(nargs) based on
    ! the delimiters contained in the string 'delims'. Preceding a delimiter in
    ! 'str' by a backslash (\) makes this particular instance not a delimiter.
    ! The integer output variable nargs contains the number of arguments found.

    character(len=*) :: str,delims
    character(len=len_trim(str)) :: strsav
    character(len=*),allocatable :: args(:)
    character(len=len_trim(str)),allocatable :: tempArgs(:)
    integer, intent(out) :: nargs
    integer :: i, k, lenstr, na
    character(len=1) :: dummy

    strsav=str
    call compactChar(str)
    na = len(str)
    if (na.le.0) then
        nargs = 0
        return
    endif

    if (allocated(args)) deallocate(args)
    if (allocated(tempArgs)) deallocate(tempArgs)

    allocate(args(na), tempArgs(na))
    !na=size(args)
    do i=1,na
        args(i)=' '
    end do
    nargs=0
    lenstr=len_trim(str)
    if(lenstr==0) return
    k=0

    do
        if(len_trim(str) == 0) exit
        nargs=nargs+1
        call splitChar(str,delims,args(nargs))
        call removebkslChar(args(nargs))
        call compactChar(args(nargs))
    end do
    tempArgs = args
    deallocate(args)
    allocate(args(nargs))
    do i = 1, nargs
        args(i) = tempArgs(i)
    enddo
    deallocate(tempArgs)
    str=strsav

end subroutine

!**********************************************************************

subroutine compactChar(str)
    !
    !  Dr. George Benthiem's string subroutine
    !
    !       See. http://gbenthien.net/about.html
    !
    ! Converts multiple spaces and tabs to single spaces; deletes control characters;
    ! removes initial spaces.
    character(len=*):: str
    character(len=1):: ch
    character(len=len_trim(str)):: outstr
    integer :: k, i, lenstr, ich, isp

    str=adjustl(str)
    lenstr=len_trim(str)
    outstr=' '
    isp=0
    k=0
    do i=1,lenstr
        ch=str(i:i)
        ich=iachar(ch)
        select case(ich)
        case(9,32)     ! space or tab character
            if(isp==0) then
                k=k+1
                outstr(k:k)=' '
            end if
            isp=1
        case(33:)      ! not a space, quote, or control character
            k=k+1
            outstr(k:k)=ch
            isp=0
        end select
    end do
    str=adjustl(outstr)
end subroutine

!**********************************************************************

subroutine splitChar(str,delims,before,sep)
    !
    !  Dr. George Benthiem's string subroutine
    !
    !       See. http://gbenthien.net/about.html
    !
    ! Routine finds the first instance of a character from 'delims' in the
    ! the string 'str'. The characters before the found delimiter are
    ! output in 'before'. The characters after the found delimiter are
    ! output in 'str'. The optional output character 'sep' contains the
    ! found delimiter. A delimiter in 'str' is treated like an ordinary
    ! character if it is preceded by a backslash (\). If the backslash
    ! character is desired in 'str', then precede it with another backslash.

    character(len=*) :: str,delims,before
    character,optional :: sep
    logical :: pres
    character :: ch,cha
    integer :: i, k, ibsl, lenstr, ipos, iposa

    pres=present(sep)
    str=adjustl(str)
    call compactChar(str)
    lenstr=len_trim(str)
    if(lenstr == 0) return        ! string str is empty
    k=0
    ibsl=0                        ! backslash initially inactive
    before=' '
    do i=1,lenstr
        ch=str(i:i)
        if(ibsl == 1) then          ! backslash active
            k=k+1
            before(k:k)=ch
            ibsl=0
            cycle
        end if
        if(ch == '\') then          ! backslash with backslash inactive
            k=k+1
            before(k:k)=ch
            ibsl=1
            cycle
        end if
        ipos=index(delims,ch)
        if(ipos == 0) then          ! character is not a delimiter
            k=k+1
            before(k:k)=ch
            cycle
        end if
        if(ch /= ' ') then          ! character is a delimiter that is not a space
            str=str(i+1:)
            if(pres) sep=ch
            exit
        end if
        cha=str(i+1:i+1)            ! character is a space delimiter
        iposa=index(delims,cha)
        if(iposa > 0) then          ! next character is a delimiter
            str=str(i+2:)
            if(pres) sep=cha
            exit
        else
            str=str(i+1:)
            if(pres) sep=ch
            exit
        end if
    end do
    if(i >= lenstr) str=''
    str=adjustl(str)              ! remove initial spaces
    return

end subroutine

!**********************************************************************

subroutine removebkslChar(str)
    !
    !  Dr. George Benthiem's string subroutine
    !
    !       See. http://gbenthien.net/about.html
    !
    ! Removes backslash (\) characters. Double backslashes (\\) are replaced
    ! by a single backslash.
    Implicit None
    character(len=*):: str
    character(len=1):: ch
    character(len=len_trim(str))::outstr
    integer :: i, k, ibsl, lenstr

    str=adjustl(str)
    lenstr=len_trim(str)
    outstr=' '
    k=0
    ibsl=0                        ! backslash initially inactive

    do i=1,lenstr
        ch=str(i:i)
        if(ibsl == 1) then          ! backslash active
            k=k+1
            outstr(k:k)=ch
            ibsl=0
            cycle
        end if
        if(ch == '\') then          ! backslash with backslash inactive
            ibsl=1
            cycle
        end if
        k=k+1
        outstr(k:k)=ch              ! non-backslash with backslash inactive
    end do

    str=adjustl(outstr)

end subroutine

FUNCTION UpperChar(s1)  RESULT (s2)
    CHARACTER(*)       :: s1
    CHARACTER(LEN(s1)) :: s2
    CHARACTER          :: ch
    INTEGER,PARAMETER  :: DUC = ICHAR('A') - ICHAR('a')
    INTEGER            :: i

    DO i = 1,LEN(s1)
        ch = s1(i:i)
        IF (ch >= 'a'.AND.ch <= 'z') ch = CHAR(ICHAR(ch)+DUC)
        s2(i:i) = ch
    END DO
END FUNCTION

FUNCTION LowerChar(s1)  RESULT (s2)
    CHARACTER(*)       :: s1
    CHARACTER(LEN(s1)) :: s2
    CHARACTER          :: ch
    INTEGER,PARAMETER  :: DUC = ICHAR('A') - ICHAR('a')
    INTEGER            :: i

    DO i = 1,LEN(s1)
        ch = s1(i:i)
        IF (ch >= 'A'.AND.ch <= 'Z') ch = CHAR(ICHAR(ch)-DUC)
        s2(i:i) = ch
    END DO
END FUNCTION

function replaceString(str, text, replaceText) result(repStr)
    implicit none
    type(typString), intent(in)  :: str
    character(len=*), intent(in) :: text, replaceText
    type(typString) :: repStr
    Call repStr%set(replaceChar(str%get(), text, replaceText))
end function

function replaceChar(str, text, replaceText) result(repStr)
Implicit None
    character(len=*), intent(in)   :: str, text, replaceText
    character(len=2*len_trim(str) + 100 ) :: repStr
    integer :: nStr, nText, nRep
    integer,allocatable :: repIndex(:)
    integer :: i, nReplace, irep

    nStr   = len_trim(str)
    nText  = len(text)
    nRep   = len(replaceText)

    allocate(repIndex(nStr))
    repIndex = 0

    nReplace = 0
    do i = 1, nStr - nText + 1
        if ( isStringEqual(str(i:i+nText-1), text) ) then
            nReplace = nReplace + 1
            repIndex(nReplace) = i
        end if
    enddo
    repStr = str
    do i = 1, nReplace
        irep = repIndex(i) + (i-1) * (nRep-nText)
        repStr = repStr(:irep-1) // replaceText(:nRep) // repStr(irep+nText:)
    enddo
end function

subroutine splitLine(str, nArg, cArg, scArg, lcArg, iArg, rArg, numFlag, scFlag, iflag)
!!----------------------------------------------------------------------------------
!!
!!  Split character line to character, integer, real argument by space (' ')
!!
!!  YoungMyung Choi, Ecole Centrale de Nantes
!!
!!----------------------------------------------------------------------------------
!!  Input
!!      tLine : Character Line
!!
!!  Output
!!      nArg     : Number of Argument (if nArg = 0, no Argument)
!!      cArg     : Character argument array, cArg(nArg)
!!      scArg    : Lower character argument array, cArg(nArg)
!!      lcArg    : Upper character argument array, cArg(nArg)
!!      iArg     : Integer   argument array, iArg(nArg)
!!      rArg     : Real      argument array, rArg(nArg)
!!      numFlag  : Logical numeric data argument :, numFlag(nArg)
!!                 if given argument if numeric data, numFlag is .TRUE.
!!      iflag    : Check given tLine is blank line (if blank, iflag = .FALSE.)
!!----------------------------------------------------------------------------------
    implicit none
    character(len=*), intent(inout) :: str
    integer, intent(inout) :: nArg
    character(len = CHAR_LEN), allocatable, intent(inout) :: cArg(:), scArg(:), lcArg(:)
    integer, allocatable, intent(inout)                       :: iArg(:)
    real(RP), allocatable, intent(inout)                      :: rArg(:)
    logical,allocatable, intent(inout)                        :: numFlag(:)
    integer,allocatable, intent(inout)                        :: scFlag(:)
    logical, intent(out) :: iflag
    !----------------------------------------------------
    integer :: i, ioInt,ioRealF, ioRealE
    character(len=CHAR_LEN), allocatable :: args(:)
    character(len=CHAR_LEN)              :: dummyChar
    type(typString)                      :: dummyStr
    real(RP) :: dummyRealF, dummyRealE, dummyReal
    Integer  :: dummyInt
    logical :: isError
    integer :: ii

    iflag = .TRUE.
    call parse(str,' ', args, nArg)

    if (nArg.eq.0) then
        nArg  = 0
        iflag = .FALSE.
    else
        if (allocated(cArg))    deallocate(cArg)
        if (allocated(scArg))   deallocate(scArg)
        if (allocated(lcArg))   deallocate(lcArg)
        if (allocated(iArg))    deallocate(iArg)
        if (allocated(rArg))    deallocate(rArg)
        if (allocated(numFlag)) deallocate(numFlag)
        if (allocated(scFlag))  deallocate(scFlag)

        allocate(cArg(nArg), scArg(nArg), lcArg(nArg))
        allocate(iArg(nArg), rArg(nArg), numFlag(nArg), scFlag(nArg))

        iArg    = 0
        rArg    = 0.0
        scFlag  = 0
        numFlag = .FALSE.

        do i = 1, nArg
            dummyChar = trim(args(i))

            Call compactChar(dummyChar)
            Call dummyStr%set(dummyChar,.TRUE.)

            cArg(i)  = dummyStr%get()
            scArg(i) = dummyStr%getLower()
            lcArg(i) = dummyStr%getUpper()

            isError = .TRUE.
            dummyReal = dummyStr%getReal(isError)

            if (isError.EQV..TRUE.) then

              ioInt = mod(dummyReal, 1.0_RP)
              dummyInt = int(dummyReal)

              numFlag(i) = .TRUE.

              ! read(dummyChar, "(i15)", iostat = ioInt) dummyInt
              ! read(dummyChar, "(f20.10)", iostat = ioRealF) dummyReal
              ! if (ioInt.eq.0)  then
              !     dummyReal  = dummyInt
              ! else if (ioRealF.eq.0)  then
              !     dummyInt   = int(dummyReal)
              ! end if
              ! numFlag(i) = .TRUE.

              rArg(i) = dummyReal
              iArg(i) = dummyInt

            end if

            scFlag(i) = testOneChar(dummyChar(1:1))

            if (numFlag(i)) then
                if (scFlag(i).gt.0) scFlag(i) = 2
            end if

        end do

        Call dummyStr%destroy()
    end if

end subroutine

function charHeader(this) result(value)
    character(len=CHAR_LEN), intent(in) :: this
    integer                             :: value
    !!... Local Variable
    character(len=CHAR_LEN) :: tmpChar
    character(len=1) :: oneH
    integer :: nStr

    value = -1

    tmpChar = trim(this)
    Call compactChar(tmpChar)

    nStr = sizeof(tmpChar)/sizeof("a")
    if (nStr.ge.1) then
        oneH = tmpChar(1:1)
    end if

    if (nStr.ge.2) then
        value = testOneChar(oneH)
        if ( isStringEqual(tmpChar(1:2),"/*") ) value = 11
        if ( isStringEqual(tmpChar(1:2),"*/") ) value = 12
    else if (nStr.eq.1) then
        value = testOneChar(oneH)
    end if

end function

function findStringLineEnd(line) result(value)
    implicit none
    type(typString), intent(in) :: line
    integer :: value
    value = findCharLineEnd(line%get())
end function

function findCharLineEnd(line) result(value)
    implicit None
    character(len=*), intent(in) :: line
    character(len=len_trim(line)) :: tmpLine
    integer :: value
    integer :: nStr, i
    tmpLine = line
    Call compactChar(tmpLine)
    nStr  = sizeof(tmpLine)/sizeof("a")
    value = -1
    if (nStr.ge.1) then
        do i = 1,nStr
            if (isStringEqual(line(i:i),";")) then
                value = i
                exit
            end if
        enddo
    end if
end

function testOneChar(oneChar) result(value)
    implicit None
    character(len=1), intent(in) :: oneChar
    integer                      :: value

    value = 1

    if ( isStringEqual(oneChar," ") ) value = -1

    if ( isStringEqual(oneChar,";") ) value = 0

    if ( isStringEqual(oneChar,"$") ) value = 122

    if ( isStringEqual(oneChar,new_line("a")) ) value = -2

    if ( isStringEqual(oneChar,"{") ) then
        value = 1101
    else if ( isStringEqual(oneChar,"}") ) then
        value = 1102
    else if ( isStringEqual(oneChar,"(") ) then
        value = 1201
    else if ( isStringEqual(oneChar,")") ) then
        value = 1202
    else if ( isStringEqual(oneChar,"[") ) then
        value = 1001
    else if ( isStringEqual(oneChar,"]") ) then
        value = 1002
    else if (testOneCharCommment(oneChar)) then
        value = -100
    end if

end function

function isWord(intArray) result(value)
    Implicit None
    Integer,dimension(:) :: intArray
    logical :: value
    integer :: nInt, ii
    nInt = size(intArray)
    value = .FALSE.
    if (nInt.ge.1) then
        value = .TRUE.
        do ii = 1, nInt
            if ((intArray(ii).ne.1).and.(intArray(ii).ne.2)) then
                value = .FALSE.
                exit
            end if
        enddo
    end if
end function

function testOneCharCommment(oneChar) result(isComment)
    implicit None
    character(len=1), intent(in) :: oneChar
    logical                      :: isComment
    integer :: i
    isComment = .FALSE.
    do i = 1, nCommentCharList
        if (isStringEqual(oneChar, commentCharList(i:i))) then
            isComment = .TRUE.
            exit
        end if
    enddo
end function

function insertSpaceSpecialCharacter(string) result(outString)
    Character(len=*), intent(in)     :: string
    Character(len=len(string) + 100) :: outString, tmpString
    character(len=1) :: specChar
    character(len=3) :: replChar
    integer :: i
    outString = ""
    outString = string
    do i = 1, nSpecialCharacter
        specChar = specialCharacter(i:i)
        replChar = " "//specChar//" "

        tmpString = ""
        tmpString = replace(outString, specChar, replChar)
        outString = tmpString
    enddo

End function

Logical Function isAllTrue(logicalArray)
    implicit none
    logical, dimension(:), intent(in) :: logicalArray
    integer :: nn, i
    logical :: dummy
    isAllTrue = .TRUE.
    nn = size(logicalArray)
    if (nn.eq.0) then
        isAllTrue = .FALSE.
        return
    end if
    do i = 1, nn
        if(logicalArray(i).EQV..FALSE.) then
            isAllTrue = .FALSE.
            return
        end if
    enddo
end function

Subroutine readMakeCharArrayFromCharArray(charArray, subCharArray, nLine, charStart, charEnd, isSubSet)
    Implicit None
    !Character(len=fileLineLength), allocatable   :: CharArray(:)
    Character(len=fileLineLength), dimension(:)  :: CharArray
    Character(len=fileLineLength), allocatable   :: subCharArray(:)
    integer, optional                            :: nLine
    Character(len=*), optional                   :: charStart, charEnd
    logical, optional                            :: isSubSet
    !! ----------------------------------------------------------------------
    logical :: readUntilEnd
    logical :: isCharActive
    Character(len=fileLineLength) :: tmpLine, modLine
    Character(len=fileLineLength), allocatable  :: tmpCharArray(:)
    integer :: nn, nTotalLine
    integer :: nSubLine

    integer :: iSub

    logical :: subSetOption
    !! ----------------------------------------------------------------------
    if (allocated(subCharArray)) deallocate(subCharArray)
    if (allocated(tmpCharArray)) deallocate(tmpCharArray)

    nTotalLine = size(CharArray)

    subSetOption = .FALSE.
    iSub = 0

    if (present(isSubSet)) subSetOption = isSubSet

    if (nTotalLine.ge.1) then

        readUntilEnd = .FALSE.
        if ( present(charEnd) ) then
            if (len_trim(charEnd).eq.0) then
                readUntilEnd = .TRUE.
                subSetOption = .FALSE.
            end if
        end if

        if ( present(charStart) ) then
            isCharActive = .FALSE.
            if (len_trim(charStart).eq.0) then
                isCharActive = .TRUE.
                subSetOption = .FALSE.
            end if
        else
            isCharActive = .TRUE.
        end if

        nSubLine = 0
        do nn = 1, nTotalLine
            tmpLine = charArray(nn)
            modLine = tmpLine
            Call compactChar(modLine)

            if ( present(charStart) ) then
                if (isStringEqual(modLine, charStart)) then
                    isCharActive = .TRUE.
                    if (subSetOption) iSub = iSub + 1
                end if

            end if

            if (isCharActive) then
                nSubLine = nSubLine + 1
                if (nSubLine.eq.1) then
                    allocate( subCharArray(1) )
                    subCharArray(1) = tmpLine
                else
                    Call move_alloc(subCharArray, tmpCharArray)

                    allocate( subCharArray(nSubLine) )
                    subCharArray(1:nSubLine-1) = tmpCharArray
                    subCharArray(nSubLine)     = tmpLine

                end if
            end if

            if ((readUntilEnd.EQV..FALSE.).AND.isCharActive) then

                if ( present(charEnd) ) then
                    if (isStringEqual(modLine, charEnd,.TRUE.)) then

                        if (subSetOption) then
                            iSub = iSub - 1
                            if (iSub.eq.0) exit
                        else
                            exit
                        end if
                     end if
                end if
            end if

        enddo

        if (present(nLine)) nLine = nSubLine
        if (allocated(tmpCharArray)) deallocate(tmpCharArray)

    else
        if (present(nLine)) nLine = 0
    end if

End Subroutine

Subroutine readMakeCharArrayFromFile(fileIndex, subCharArray, nLine, charStart, charEnd, fRewind)
    Implicit None
    Integer, intent(in)                         :: fileIndex
    Character(len=fileLineLength), allocatable  :: subCharArray(:)
    integer, optional                           :: nLine
    Character(len=*), optional                  :: charStart, charEnd
    logical, optional                           :: fRewind
    !! ----------------------------------------------------------------------
    logical :: isCharActive
    integer :: ios
    Character(len=fileLineLength) :: tmpLine
    Character(len=fileLineLength), allocatable  :: tmpCharArray(:)
    integer :: nn
    logical :: readUntilEnd
    !! ----------------------------------------------------------------------

    if (allocated(subCharArray)) deallocate(subCharArray)
    if (allocated(tmpCharArray)) deallocate(tmpCharArray)

    readUntilEnd = .FALSE.
    if ( present(charEnd) ) then
        if (len_trim(charEnd).eq.0) readUntilEnd = .TRUE.
    end if

    if ( present(charStart) ) then
        isCharActive = .FALSE.
        if (len_trim(charStart).eq.0) isCharActive = .TRUE.
    else
        isCharActive = .TRUE.
    end if

    nn = 0
    !! File Read
    do
        !!... Read Line and exit if file end
        read(fileIndex, "(a)", iostat = ios) tmpLine; if (ios < 0) exit

        if ( present(charStart) ) then
            if (isStringEqual(tmpLine, charStart,.TRUE.)) isCharActive = .TRUE.
        end if

        if (isCharActive) then
            nn = nn + 1
            if (nn.eq.1) then
                allocate( subCharArray(1) )
                subCharArray(1) = tmpLine
            else
                Call move_alloc(subCharArray, tmpCharArray)
                allocate( subCharArray(nn) )
                subCharArray(1:nn-1) = tmpCharArray
                subCharArray(nn)     = tmpLine
            end if
        end if

        if ((readUntilEnd.EQV..FALSE.).AND.isCharActive) then
            if ( present(charEnd) ) then
                if (isStringEqual(tmpLine, charEnd,.TRUE.)) exit
            end if
        end if

    enddo

    if (present(nLine)) nLine = nn
    if (allocated(tmpCharArray)) deallocate(tmpCharArray)

End Subroutine

Subroutine removeCommentBlock(charArray, removedCharArray)
    Implicit None
    Character(len=fileLineLength),dimension(:), intent(in)  :: charArray
    Character(len=fileLineLength),allocatable, intent(out) :: removedCharArray(:)

    Character(len=fileLineLength) :: tmpLine, ttmpLine
    integer :: nLine, iLine, jLine, nChar
    logical :: isBlockStart, isBlockEnd, isBlock
    integer :: iBlock, jBlock, ich, nch
    logical :: isCommented

    Character(len=fileLineLength),allocatable :: tmpCharArray(:)

    isBlock = .FALSE.

    isBlockStart = .FALSE.
    isBlockEnd   = .FALSE.

    nLine = size(charArray)

    if (nLine.ge.1) then

        if (allocated(removedCharArray)) deallocate(removedCharArray)
        allocate(removedCharArray(nLine))

        if (allocated(tmpCharArray)) deallocate(tmpCharArray)
        allocate(tmpCharArray(nLine))

        removedCharArray = ""

        !! // Remove Comment Block First
        do iLine = 1, nLine
            tmpLine = charArray(iLine)
            if (isBlock.EQV..FALSE.) then
                Call findCommentBlockStart(tmpLine, isBlockStart, iBlock)
                if (isBlockStart) isBlock = .TRUE.
            end if
            if (isBlock.EQV..TRUE.) then
                Call findCommentBlockEnd(tmpLine, isBlockEnd, jBlock)
                if (isBlockEnd) isBlock     = .FALSE.
            end if
            if (isBlock) then
                if (isBlockStart.EQV..TRUE.) then
                    ttmpLine = tmpLine
                    tmpLine  = ""
                    if (iBlock.ge.1) then
                        tmpLine  = ttmpLine(1:iBlock)
                    endif
                else
                    tmpLine  = ""
                end if
                isBlockStart = .FALSE.
            end if

            if ((isBlock.EQV..FALSE.).AND.(isBlockEnd.EQV..TRUE.)) then
                ttmpLine = tmpLine
                tmpLine  = ""
                if (jBlock.ge.1) then
                    tmpLine  = ttmpLine(jBlock:)
                end if
                isBlockEnd = .FALSE.
            end if
            removedCharArray(iLine) = ""
            removedCharArray(iLine) = tmpLine
        enddo

        !! // comment treatement
        do iLine = 1, nLine
            tmpLine = removedCharArray(iLine)
            nChar = len_trim(tmpLine)
            if (nChar.ge.1) then
                if (nChar.ge.2) then
                    nch = nChar
                    isCommented = .FALSE.
                    do ich =1, nChar-1
                        if ( isStringEqual(tmpLine(ich:ich+1),"//") ) then
                            nch = ich-1
                            isCommented = .TRUE.
                            exit
                        endif
                    enddo

                    if (isCommented) then
                        if (nch.ge.1) then
                            removedCharArray(iLine) = ""
                            removedCharArray(iLine) = tmpLine(1:nch)
                        else
                            removedCharArray(iLine) = ""
                        endif
                    else
                        removedCharArray(iLine) = tmpLine
                    endif
                else
                    removedCharArray(iLine) = tmpLine
                end if
            else
                removedCharArray(iLine) = ""
            end if
        enddo

        !! other comment treatement
        do iLine = 1, nLine
            tmpLine = trim(removedCharArray(iLine))
            nChar = len_trim(tmpLine)

            isCommented = .FALSE.
            if (nChar.ge.1) then
                nch = nChar
                do ich =1, nChar
                    if ( testOneCharCommment(tmpLine(ich:ich)) ) then
                        nch = ich-1
                        isCommented = .TRUE.
                        exit
                    endif
                enddo

                if (isCommented) then
                    if (nch.ge.1) then
                        removedCharArray(iLine) = ""
                        removedCharArray(iLine) = tmpLine(1:nch)
                    else
                        removedCharArray(iLine) = ""
                    endif
                else
                    removedCharArray(iLine) = tmpLine
                endif

            else
                removedCharArray(iLine) = ""
            end if

        enddo

        ! jline = 0
        ! do iline = 1, nLine
        !     tmpLine = trim(removedCharArray(iLine))
        !     nChar   = len_trim(tmpLine)
        !     if (nChar.ge.1) then
        !         jLine = jLine + 1
        !         tmpCharArray(jline) = ""
        !         tmpCharArray(jline) = tmpLine
        !     end if
        !
        !     write(*,*) iline, nChar, trim(tmpLine)
        ! enddo
        !
        ! if (allocated(removedCharArray)) deallocate(removedCharArray)
        ! allocate(removedCharArray(jline))
        !
        ! removedCharArray = tmpCharArray(1:jLine)
        !
        ! deallocate(tmpCharArray)
        !
        ! do iline = 1, jline
        !     write(*,*) iline, trim(removedCharArray(iline))
        ! enddo

    else
        write(*,*)  "   [ERROR] removeCommentBlock :: Empty CharArray."
        stop
    endif

End Subroutine

Subroutine findCommentBlockStart(charLine, isBlockStart, nLineBlock)
    Implicit None
    character(len=*),intent(in) :: charLine
    logical, intent(out) :: isBlockStart
    integer, intent(out) :: nLineBlock
    integer :: ichar
    nLineBlock     = len(charLine)
    isBlockStart = .FALSE.
    if (nLineBlock.ge.2) then
        do ichar =1, nLineBlock-1
            if (charLine(ichar:ichar+1).EQ.commentBlockStart) then
                nLineBlock = ichar - 1
                isBlockStart = .TRUE.
                exit
            endif
        enddo
    end if
end subroutine

Subroutine findCommentBlockEnd(charLine, isBlockEnd, nLineBlock)
    Implicit None
    character(len=*),intent(in) :: charLine
    logical, intent(out) :: isBlockEnd
    integer, intent(out) :: nLineBlock
    integer :: ichar
    nLineBlock = len(charLine)
    isBlockEnd = .FALSE.
    if (nLineBlock.ge.2) then
        do ichar =1, nLineBlock-1
            if (charLine(ichar:ichar+1).EQ.commentBlockEnd) then
                nLineBlock = ichar + 2
                isBlockEnd = .TRUE.
                exit
            endif
        enddo
    end if
end subroutine

function stringHeader(this) result(value)
    class(typString), intent(in) :: this
    integer                      :: value
    !!... Local Variable
    character(len=CHAR_LEN)      :: tmpChar

    tmpChar = this%get()
    value = charHeader(tmpChar)

end function

Subroutine separateFilePath(filePath, fileDir, fileName, fileExt)
    Implicit None
    Character(len=*), intent(in)  :: filePath
    Character(len=*), intent(out) :: fileDir
    Character(len=*), intent(out) :: fileName
    Character(len=*), intent(out) :: fileExt
    integer :: ichar, nchar, dirPoint,extPoint

    fileDir = ""
    fileName = ""
    fileExt = ""

    nchar = len(filePath)

    if (nchar.ge.1) then
        dirPoint = 0
        extPoint = 0
        do ichar = 1, nchar
            if ((filePath(ichar:ichar).eq."/").OR.(filePath(ichar:ichar).eq."\")) then
                dirPoint = ichar
            end if

            if ((ichar.ge.2).and.(ichar.le.nchar-1)) then
                if (filePath(ichar:ichar).eq.".") then
                    if       (filePath(ichar-1:ichar).eq."..") then
                    else if  (filePath(ichar-1:ichar).eq."/.") then
                    else if  (filePath(ichar-1:ichar).eq."\.") then
                    else
                        extPoint = ichar
                    end if
                end if
            end if
        enddo

        if (dirPoint.ge.1) then

            fileDir = filePath(1:dirPoint)

            if (extPoint.gt.dirPoint) then
                fileExt  = filePath(extPoint:)
                fileName = filePath(dirPoint+1:extPoint-1)
            else
                fileName = filePath(dirPoint+1:)
            end if
        else

            if (extPoint.ge.2) then
                fileName = filePath(1:extPoint-1)
                fileExt  = filePath(extPoint:)
            else
                fileName = filePath
            end if

        end if

    end if

End Subroutine

Function blankSpace(nSpace) result(val)
    integer, intent(in) :: nSpace
    Character(len=nSpace) :: val
    val(1:nSpace) = " "
End Function

Subroutine writeDictFileHeader(fileIndex)
    Implicit None
    Integer, intent(in) :: fileIndex

    write(fileIndex, "(a)") "/*----------------------------------------------------------------------*\"
    write(fileIndex, "(a)") "|                                                                        |"
    write(fileIndex, "(a)") "|                          Dictionary File                               |"
    write(fileIndex, "(a)") "|                                                                        |"
    write(fileIndex, "(a)") "|                This file is generated by FyMcPack.                     |"
    write(fileIndex, "(a)") "|                                                                        |"
    write(fileIndex, "(a)") "|                   Ecole Centrale de Nantes, LHEEA                      |"
    write(fileIndex, "(a)") "|                            YoungMyung Choi                             |"
    write(fileIndex, "(a)") "|                                                                        |"
    write(fileIndex, "(a)") "\*----------------------------------------------------------------------*/"

End Subroutine

Function findFirstCharacterInString(string, oneChar) result(pos)
  Implicit none
  Character(len=*), intent(in) :: string
  Character(len=1), intent(in) :: oneChar
  Integer :: pos, Nchar, ii
  pos = 0
  Nchar = len_trim(string)
  if (Nchar.ge.1) then
    do ii = 1,Nchar
      if ( string(ii:ii).eq.oneChar ) then
        pos =ii
        exit
      endif
    enddo
  endif
End Function
