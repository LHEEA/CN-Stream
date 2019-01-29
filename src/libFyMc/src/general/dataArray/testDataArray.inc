Subroutine testDataArray()
    Implicit None

    type(typDataArray) :: dataArr

    integer :: nData = 5
    real(rp), allocatable :: tmpDataReal(:)
    integer, allocatable :: tmpDataInt(:)
    Character(len=CHAR_LEN), allocatable :: tmpDataChar(:)
    type(typString), allocatable :: tmpDataStr(:)

    integer :: i


    allocate(tmpDataReal(nData))
    do i = 1, nData
        tmpDataReal(i) = i
    enddo


    allocate(tmpDataInt(nData))
    do i = 1, nData
        tmpDataInt(i) = i
    enddo

    allocate(tmpDataChar(nData))
    tmpDataChar(1) = "a"
    tmpDataChar(2) = "bc"
    tmpDataChar(3) = "def"
    tmpDataChar(4) = "ghik"
    tmpDataChar(5) = "jalmn"

    allocate(tmpDataStr(nData))
    do i = 1, nData
        Call tmpDataStr(i)%set(tmpDataChar(i), .TRUE.)
    enddo

    Call dataArr%set(tmpDataReal)
    write(*,*) dataArr%arrR
    write(*,*) dataArr%arrI

    Call dataArr%set(tmpDataChar, .TRUE.)

    do i = 1, nData
         write(*,*) dataArr%arrS(i)%get()
    enddo

    Call dataArr%set(nData, tmpDataStr)

    do i = 1, nData
         write(*,*) dataArr%arrS(i)%get()
    enddo

End Subroutine
