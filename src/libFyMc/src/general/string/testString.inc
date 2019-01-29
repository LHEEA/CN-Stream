Subroutine testString
    Implicit None

    ! Call testStringFunc

    ! Call testCharHeader()

    ! Call testStringHeader()

    ! Call testParse()

    Call testReplace()

end Subroutine

Subroutine testReplace()
    Character(len=CHAR_LEN) :: tmpChar1, tmpChar2

    tmpChar1  = "/*arrI (123 (2(5(6);*/"
    tmpChar2 = replace(tmpChar1, "(", " ( " )

    write(*,*) trim(tmpChar1)
    write(*,*) trim(tmpChar2)

end subroutine

Subroutine testParse()
    Character(len=CHAR_LEN) :: tmpChar
    character(len=CHAR_LEN),allocatable :: args(:)
    integer :: nargs
    integer :: iarg

    tmpChar = "/*arrI (123 256);*/"

    Call parse(tmpChar, " ();/*", args, nargs)

    do iarg = 1, nargs
        write(*,*) trim(args(iarg))
    enddo

    Call parseOneChar(tmpChar, " ();/*", args, nargs)

    do iarg = 1, nargs
        write(*,*) trim(args(iarg))
    enddo

end subroutine

Subroutine testCharHeader()
    Character(len=CHAR_LEN) :: tmpChar
    integer :: headerValue
    integer :: i

    tmpChar = ""
    headerValue = strHeader(tmpChar)
    write(*,*) trim(tmpChar), headerValue

    tmpChar = "   $ abc"
    headerValue = strHeader(tmpChar)
    write(*,*) trim(tmpChar), headerValue

    do i = 1, nCommentCharList
        ! tmpChar = commentCharList(i:i)
        tmpChar = commentCharList(i:i)//"abc"
        headerValue = strHeader(tmpChar)
        write(*,*) trim(tmpChar), headerValue
    enddo

    tmpChar = "( a"
    headerValue = strHeader(tmpChar)
    write(*,*) trim(tmpChar), headerValue

    tmpChar = ") c"
    headerValue = strHeader(tmpChar)
    write(*,*) trim(tmpChar), headerValue

    tmpChar = "; a"
    headerValue = strHeader(tmpChar)
    write(*,*) trim(tmpChar), headerValue

    tmpChar = "$"
    headerValue = strHeader(tmpChar)
    write(*,*) trim(tmpChar), headerValue

    tmpChar = "{"
    headerValue = strHeader(tmpChar)
    write(*,*) trim(tmpChar), headerValue

    tmpChar = "}"
    headerValue = strHeader(tmpChar)
    write(*,*) trim(tmpChar), headerValue

    tmpChar = "["
    headerValue = strHeader(tmpChar)
    write(*,*) trim(tmpChar), headerValue

    tmpChar = "]"
    headerValue = strHeader(tmpChar)
    write(*,*) trim(tmpChar), headerValue

End Subroutine

Subroutine testStringHeader()
    Character(len=CHAR_LEN) :: tmpChar
    type(typString) :: tmpString
    integer :: headerValue
    integer :: i

    Call tmpString%set("")
    headerValue = strHeader(tmpString)
    write(*,*) trim(tmpString%get()), headerValue

    tmpChar = "abc"
    Call tmpString%set(tmpChar)
    headerValue = strHeader(tmpString)
    write(*,*) trim(tmpString%get()), headerValue

    do i = 1, nCommentCharList
        ! tmpChar = commentCharList(i:i)
        tmpChar = commentCharList(i:i)//"abc"
        Call tmpString%set(tmpChar)
        headerValue = strHeader(tmpString)
        write(*,*) trim(tmpString%get()), headerValue
    enddo

    tmpChar = "( a"
    Call tmpString%set(tmpChar)
    headerValue = strHeader(tmpString)
    write(*,*) trim(tmpString%get()), headerValue

    tmpChar = ") c"
    Call tmpString%set(tmpChar)
    headerValue = strHeader(tmpString)
    write(*,*) trim(tmpString%get()), headerValue

    tmpChar = "; a"
    Call tmpString%set(tmpChar)
    headerValue = strHeader(tmpString)
    write(*,*) trim(tmpString%get()), headerValue

    tmpChar = "$"
    Call tmpString%set(tmpChar)
    headerValue = strHeader(tmpString)
    write(*,*) trim(tmpString%get()), headerValue

    tmpChar = "{"
    Call tmpString%set(tmpChar)
    headerValue = strHeader(tmpString)
    write(*,*) trim(tmpString%get()), headerValue

    tmpChar = "}"
    Call tmpString%set(tmpChar)
    headerValue = strHeader(tmpString)
    write(*,*) trim(tmpString%get()), headerValue

    tmpChar = "["
    Call tmpString%set(tmpChar)
    headerValue = strHeader(tmpString)
    write(*,*) trim(tmpString%get()), headerValue

    tmpChar = "]"
    Call tmpString%set(tmpChar)
    headerValue = strHeader(tmpString)
    write(*,*) trim(tmpString%get()), headerValue

End Subroutine

Subroutine testStringFunc
    Implicit None
    Character(len=CHAR_LEN) :: tmpChar

    Type(typString) :: tmpString1, tmpString2

    Type(typString), allocatable :: args(:)
    ! Character(len=CHAR_LEN),allocatable :: args(:)
    integer :: nargs, iarg

    integer :: tmpInt
    Real    :: tmpReal

    tmpChar = "ab cd"
    Call tmpString1%set("7.5")
    Call tmpString2%set("3.0")

    tmpInt = tmpString1%getInt(.FALSE.)
    tmpReal = tmpString2%getReal()

    write(*,*) tmpInt
    write(*,*) tmpReal

    ! Call tmpString%set("abc def")
    !
    ! Call parse(tmpChar, " ", args, nargs)
    !
    ! !Call parse(tmpString, " ", args, nargs)
    !
    ! write(*,*) tmpChar
    !
    ! ! do iarg = 1, nargs
    ! !     write(*,*) args(iarg)
    ! ! enddo
    !
    ! do iarg = 1, nargs
    !     write(*,*) iarg, args(iarg)%get()
    ! enddo

End Subroutine
