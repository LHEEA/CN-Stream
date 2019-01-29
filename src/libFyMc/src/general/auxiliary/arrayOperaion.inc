Subroutine addSaveArray(nSaveArray, &
                        saveStrArray, saveNumArray, saveNumFlagArray, saveFlagArray, &
                        tmpStrArray,  tmpNumArray,  tmpNumFlagArray, tmpFlagArray)
    Implicit None
    Integer, intent(inout)                               :: nSaveArray
    Character(len=CHAR_LEN), allocatable, intent(inout)  :: saveStrArray(:)
    real(rp), allocatable, intent(inout)                 :: saveNumArray(:)
    logical, allocatable, intent(inout)                  :: saveNumFlagArray(:)
    integer, allocatable, intent(inout)                  :: saveFlagArray(:)

    Character(len=CHAR_LEN), dimension(:), intent(in)  :: tmpStrArray
    real(rp), dimension(:), intent(in)                 :: tmpNumArray
    logical, dimension(:), intent(in)                  :: tmpNumFlagArray
    integer, dimension(:), intent(in)                  :: tmpFlagArray

    !! Local Save
    integer                                 :: nTmpArray

    Character(len=CHAR_LEN), allocatable :: tmpSaveStrArr(:)
    Real(RP), allocatable                :: tmpSaveNumArr(:)
    logical, allocatable                 :: tmpSaveNumFlagArr(:)
    integer, allocatable                 :: tmpSaveFlagArr(:)

    nTmpArray = size(tmpNumFlagArray)

    if (nSaveArray.eq.0) then
        if (allocated(saveStrArray))     deallocate(saveStrArray)
        if (allocated(saveNumArray))     deallocate(saveNumArray)
        if (allocated(saveNumFlagArray)) deallocate(saveNumFlagArray)
        if (allocated(saveFlagArray)) deallocate(saveFlagArray)

        if (nTmpArray.ge.1) then
            saveStrArray     = tmpStrArray
            saveNumArray     = tmpNumArray
            saveNumFlagArray = tmpNumFlagArray
            saveFlagArray    = tmpFlagArray
            nSaveArray = nTmpArray
        end if
    else
        if (nTmpArray.ge.1) then
            if (allocated(tmpSaveStrArr))     deallocate(tmpSaveStrArr)
            if (allocated(tmpSaveNumArr))     deallocate(tmpSaveNumArr)
            if (allocated(tmpSaveNumFlagArr)) deallocate(tmpSaveNumFlagArr)
            if (allocated(tmpSaveFlagArr))    deallocate(tmpSaveFlagArr)

            Call move_alloc(saveStrArray,     tmpSaveStrArr)
            Call move_alloc(saveNumArray,     tmpSaveNumArr)
            Call move_alloc(saveNumFlagArray, tmpSaveNumFlagArr)
            Call move_alloc(saveFlagArray,    tmpSaveFlagArr)

            allocate(saveStrArray(nSaveArray + nTmpArray), &
                     saveNumArray(nSaveArray + nTmpArray), &
                     saveNumFlagArray(nSaveArray + nTmpArray), &
                     saveFlagArray(nSaveArray + nTmpArray))

            saveStrArray(1:nSaveArray)     = tmpSaveStrArr(1:nSaveArray)
            saveNumArray(1:nSaveArray)     = tmpSaveNumArr(1:nSaveArray)
            saveNumFlagArray(1:nSaveArray) = tmpSaveNumFlagArr(1:nSaveArray)
            saveFlagArray(1:nSaveArray)    = tmpSaveFlagArr(1:nSaveArray)

            saveStrArray(nSaveArray+1:nSaveArray + nTmpArray)     = tmpStrArray(1:nTmpArray)
            saveNumArray(nSaveArray+1:nSaveArray + nTmpArray)     = tmpNumArray(1:nTmpArray)
            saveNumFlagArray(nSaveArray+1:nSaveArray + nTmpArray) = tmpNumFlagArray(1:nTmpArray)
            saveFlagArray(nSaveArray+1:nSaveArray + nTmpArray)    = tmpFlagArray(1:nTmpArray)

            nSaveArray = nSaveArray + nTmpArray

            deallocate(tmpSaveStrArr, tmpSaveNumArr, tmpSaveNumFlagArr, tmpSaveFlagArr)
        end if
    end if

End Subroutine
