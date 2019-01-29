Subroutine initializeDictDataBase(dictName, dictID, parentDictID)
    Character(len=*), intent(in)                :: dictName
    integer,intent(out)                         :: dictID
    integer, optional                           :: parentDictID
    !! ------------------------------------------------------------------------
    Type(typDictionary), target, allocatable :: tmpDict(:)
    logical, allocatable             :: tmpDictActive(:)
    integer :: i
    Character(len=len_trim(dictName)) :: tmpDictName
    !! ------------------------------------------------------------------------
    if (nDictDB_.eq.0) then
        allocate(dictDB(1))
        allocate(dictDBActive(1))
    else
        Call move_alloc(dictDBActive, tmpDictActive)
        allocate(dictDBActive(nDictDB_ + 1))
        dictDBActive(1: nDictDB_) = tmpDictActive

        if (allocated(tmpDict)) then
            do i = 1, nDictDB_
                Call tmpDict(i)%destroy()
            enddo
            deallocate(tmpDict)
        end if
        allocate(tmpDict(nDictDB_))

        do i = 1, nDictDB_
            tmpDict(i) = dictDB(i)
        enddo

        if (allocated(dictDB)) then
            do i = 1, nDictDB_
                Call dictDB(i)%destroy()
            enddo
            deallocate(dictDB)
        end if
        allocate(dictDB(nDictDB_ + 1))

        do i = 1, nDictDB_
            dictDB(i) = tmpDict(i)
        enddo

    end if

    tmpDictName = dictName

    Call compactChar(tmpDictName)

    nDictDB_ = nDictDB_ + 1

    dictID                  = nDictDB_
    dictDBActive(dictID)    = .TRUE.

    dictDB(dictID)%dictName = tmpDictName
    dictDB(dictID)%dictID   = dictID

    Call dictDB(dictID)%sLTable%initialize(trim(tmpDictName)//"_lookUpTable")
    Call dictDB(dictID)%subDictTable%initialize(trim(tmpDictName)//"_subDict")

    dictDB(dictID)%parentDictID = 0
    if (present(parentDictID)) then
        dictDB(dictID)%parentDictID = parentDictID
    end if

End Subroutine

Recursive Subroutine initDictDB_NameCharArray(dictName, charArray, dictID, parentDictID)
    Implicit None
    Character(len=*), intent(in)                           :: dictName
    Character(len=fileLineLength),dimension(:), intent(in) :: charArray
    Integer, intent(out)                                   :: dictID
    integer, optional                                      :: parentDictID
    !! integer,intent(in)                  :: fileIOIndex
    !! Local Variables --------------------------------------------------------
    character(len = fileLineLength) :: tmpLine, modLine
    integer :: iLine, jLine, nLine

    !!... Argument Information
    integer :: nArg, nArgValid
    character(len = CHAR_LEN), allocatable :: cArg(:), scArg(:), lcArg(:)
    integer, allocatable                   :: iArg(:)
    real(RP), allocatable                  :: rArg(:)
    logical,allocatable                    :: numFlag(:)
    integer,allocatable                    :: scFlag(:)
    logical :: iflag

    !!... temporary Saved Array
    integer :: nSaveData, nTmpData
    character(len = CHAR_LEN), allocatable :: tmpSaveCArg(:)
    real(RP), allocatable                  :: tmpSaveRArg(:)
    logical,allocatable                    :: tmpSaveNumFlag(:)
    integer,allocatable                    :: tmpSaveFlag(:)

    !! Multi Line Operation
    logical :: isKeyActive, isArrayActive, isDictActive, isKeyExist
    character(len=CHAR_LEN)                :: keySaved, shortKey

    !! Sub Dict
    Character(len=fileLineLength),allocatable :: subDictCharArray(:)
    integer :: nLineSubDict
    integer :: subDictID, shortKeyDictID
    !! Dummy Integer
    integer :: ii
    character(len = CHAR_LEN), allocatable :: tmpKeyCArg(:)
    real(RP), allocatable                  :: tmpKeyRArg(:)
    integer :: tmpParentDictID
    integer :: nArr

    Real(RP), allocatable :: tempRealField(:)
    character(len=CHAR_LEN), allocatable :: tempCharField(:)

    Character(len=fileLineLength),allocatable :: removedCharArray(:)
    !! Subroutine Start -------------------------------------------------------

    Call initializeDictDataBase(dictName, dictID, parentDictID)

    Call removeCommentBlock(charArray, removedCharArray)

    nLine = size(removedCharArray)

    isKeyActive   = .FALSE.
    isDictActive  = .FALSE.
    isArrayActive = .FALSE.

    iLine = 0
    do
        iLine = iLine + 1
        if (iLine.gt.nLine) exit

        !! Get Line
        tmpLine = removedCharArray(iLine)

        !! Modify Line to be separated correctly
        modLine = insertSpaceSpecialCharacter(tmpLine)

        !! Split Line
        Call splitLine(modLine, nArg, cArg, scArg, lcArg, iArg, rArg, numFlag, scFlag, iflag)

        nArgValid = nArg
        !! No blank line
        if (nArg.ge.1) then
            !! Get Valid Line
            do ii = 1, nArg
                if (scFlag(ii).lt.0) exit
            enddo
            nArgValid = ii - 1
        end if

        !!... KeyWord Search
        if (nArgValid.ge.1) then
            shortKeyDictID = 0

            !! Keyword Start
            if (isKeyActive.EQV..FALSE.) then

                !! Word only can be keyword
                if (scFlag(1).eq.1) then

                    keySaved    = cArg(1);      isKeyActive = .TRUE.

                    Call compactChar(keySaved)

                    !! If Keyword and End Exist
                    if (scFlag(nArgValid).eq.0) then

                        !! Key, Value, End in One Line
                        !!   - format : key  val1 ;
                        if (nArgValid.eq.3) then

                            !! Check Value
                            if ( isWord(scFlag(2:2)) ) then

                                if (numFlag(2)) then
                                    Call dictDB(dictID)%sLTable%push(keySaved, rArg(2))   !! Push Number Value
                                else
                                    Call dictDB(dictID)%sLTable%push(keySaved, cArg(2))   !! Push Char Value
                                end if

                                keySaved = "";              isKeyActive = .FALSE.

                            !! Short Key
                            else if (scFlag(2).eq.122) then

                                shortKey = cArg(2)
                                shortKey = shortKey(2:)

                                isKeyExist = .FALSE.
                                tmpParentDictID = dictID
                                do

                                    !! Search Dictionary in parent dictionary
                                    isKeyExist = dictDB(tmpParentDictID)%sLTable%keyExist(shortKey)

                                    if (isKeyExist) then

                                        if (dictDB(tmpParentDictID)%sLTable%isNumber(shortKey)) then

                                            !! Get Real Array
                                            Call dictDB(tmpParentDictID)%sLTable%getRealArray(shortKey, tmpKeyRArg)

                                            !! Push Real Array
                                            Call dictDB(dictID)%sLTable%push(keySaved, tmpKeyRArg)

                                        else if (dictDB(tmpParentDictID)%sLTable%isCharacter(shortKey)) then

                                            !! Get Character Array
                                            Call dictDB(tmpParentDictID)%sLTable%getCharArray(shortKey, tmpKeyCArg)

                                            !! Push Character Array
                                            Call dictDB(dictID)%sLTable%push(keySaved, tmpKeyCArg)
                                        else
                                            Call writeErrorFileDict("Wrong Short Key Type is defined. ", shortKey)
                                        end if

                                        keySaved = "";              isKeyActive = .FALSE.

                                        exit

                                    end if

                                    !! Search Dictionary by up shooting direction ( subSubDict -> subDict -> dict )
                                    tmpParentDictID = dictDB(tmpParentDictID)%parentDictID

                                    if (tmpParentDictID.eq.0) then
                                        Call writeErrorFileDict("Short Key is not defined previously. : ", tmpLine)
                                        exit
                                    end if

                                end do

                                if (isKeyExist.EQV..FALSE.) then
                                    Call writeErrorFileDict("Short Key is not defined previously. : ", tmpLine)
                                end if

                            !! Write Error
                            else
                                Call writeErrorFileDict("Wrong value", tmpLine)
                            end if

                        !! Key, Array, End in One Line
                        !!   - format : key  ( val1 val2 ... valN ) ;
                        else
                            if (nArgValid.ge.5) then

                                if ( numFlag(2).AND.(scFlag(nArgValid).EQ.0) ) then

                                    !! key nArray ( val1 val2 ... valN );
                                    if ((scFlag(3).eq.1201).or.(scFlag(nArgValid-1).eq.1202)) then

                                        if (isAllTrue(numFlag(4:nArgValid-2))) then
                                            !! Push Number Value
                                            Call dictDB(dictID)%sLTable%push(keySaved, rArg(4:nArgValid-2))
                                        else
                                            !! Push Char Value
                                            Call dictDB(dictID)%sLTable%push(keySaved, cArg(4:nArgValid-2))
                                        end if
                                        keySaved = "";              isKeyActive = .FALSE.

                                    !! key nArray { val };
                                    elseif ( (scFlag(3).eq.1101).or.(scFlag(5).eq.1102) ) then

                                        nArr = iArg(2)

                                        if (nArr.ge.1) then

                                            if ( numFlag(4) ) then
                                                allocate(tempRealField(nArr))
                                                tempRealField(1:nArr) = rArg(4)
                                                !! Push Number Value
                                                Call dictDB(dictID)%sLTable%push(keySaved, tempRealField )
                                                deallocate(tempRealField)
                                            else
                                                allocate(tempCharField(nArr))
                                                tempCharField(1:nArr) = cArg(4)
                                                !! Push Char Value
                                                Call dictDB(dictID)%sLTable%push(keySaved, tempCharField)
                                                deallocate(tempCharField)
                                            end if

                                            keySaved = "";              isKeyActive = .FALSE.

                                        else
                                            Call writeErrorFileDict("Wrong contents in array.", tmpLine)
                                        end if

                                    else

                                         Call writeErrorFileDict("Wrong contents in array.", tmpLine)

                                    end if

                                else if ( isWord(scFlag(3:nArgValid-2)) ) then
                                    if (isAllTrue(numFlag(3:nArgValid-2))) then
                                        !! Push Number Value
                                        Call dictDB(dictID)%sLTable%push(keySaved, rArg(3:nArgValid-2))
                                    else
                                        !! Push Char Value
                                        Call dictDB(dictID)%sLTable%push(keySaved, cArg(3:nArgValid-2))
                                    end if
                                    keySaved = "";              isKeyActive = .FALSE.
                                else
                                    Call writeErrorFileDict("Wrong contents in array.", tmpLine)
                                end if

                            else
                                Call writeErrorFileDict("Wrong contents in array.", tmpLine)
                            end if

                        end if

                    !! If Keyword exist, but end not exist
                    else

                        if (nArgValid.ge.3) then

                            if (isWord(scFlag(2:2))) then

                                if (scFlag(3).eq.0) then
                                    nSaveData = 0
                                    Call addSaveArray(nSaveData,        &
                                                      tmpSaveCArg,      &
                                                      tmpSaveRArg,      &
                                                      tmpSaveNumFlag,   &
                                                      tmpSaveFlag,      &
                                                      cArg(2:2),        &
                                                      rArg(2:2),        &
                                                      numFlag(2:2),     &
                                                      scFlag(2:2))

                                else
                                    Call writeErrorFileDict("Wrong Format. ; should be located after the value.", tmpLine)
                                end if

                            else
                                Call writeErrorFileDict("Wrong contents in array. ", tmpLine)
                            end if

                        end if

                        !! If Argument is larger than 2.
                        if (nArgValid.ge.2) then

                            !! Dictionary Start
                            if (scFlag(2).eq.1101) then

                                Call writeErrorFileDict(" Dictionary character { should be separated.", tmpLine)

                            !! Array Start
                            elseif ((scFlag(2).eq.1201).or.(scFlag(2).eq.1001)) then

                                !! [ERROR] If Array End is Given
                                !!   - format : key ( val val2 ... val 3)
                                if ((scFlag(nArgValid).eq.1202).or.(scFlag(nArgValid).eq.1002)) then
                                    Call writeErrorFileDict("Wrong Format. ; should be located after the value.", tmpLine)

                                !! If Array End is Not given
                                !!   - format : key ( val val2 ... val 3
                                else if (nArgValid.eq.2) then
                                    nSaveData     = 0
                                    isArrayActive = .TRUE.
                                else
                                    if (isWord( scFlag(3:nArgValid) )) then
                                        nTmpData = nArgValid - 2
                                        !! Save Data
                                        if (nTmpData.ge.1) then
                                            nSaveData     = 0
                                            Call addSaveArray(nSaveData,            &
                                                              tmpSaveCArg,          &
                                                              tmpSaveRArg,          &
                                                              tmpSaveNumFlag,       &
                                                              tmpSaveFlag,          &
                                                              cArg(3:nArgValid),    &
                                                              rArg(3:nArgValid),    &
                                                              numFlag(3:nArgValid), &
                                                              scFlag(3:nArgValid))
                                            isArrayActive = .TRUE.
                                        end if
                                    else
                                        Call writeErrorFileDict("Wrong line description for array. ", tmpLine)
                                    end if

                                end if

                            !! Error : Dictionary End
                            else if (scFlag(2).eq.1102) then
                                Call writeErrorFileDict(" Dictionary character } should be separated.", tmpLine)
                            !! Error : Array End
                            else if ((scFlag(2).eq.1202).or.(scFlag(2).eq.1002)) then
                                Call writeErrorFileDict("Wrong Format. Wrong Array End.", tmpLine)
                            !! Error : Wrong line
                            else
                                Call writeErrorFileDict("Wrong contents in array. ", tmpLine)
                            end if

                        end if

                    end if


                !! Keyword End
                else if (scFlag(1).eq.0) then

                    Call writeErrorFileDict("Wrong Format. Wrong End.", tmpLine)

                !! Dictionary Input
                else if  (scFlag(1).eq.122) then

                    if (nArgValid.eq.1) then

                        shortKey   = cArg(1)
                        shortKey   = shortKey(2:)

                        isKeyExist = .FALSE.
                        tmpParentDictID = dictID
                        do
                            !! Search Dictionary by up shooting direction ( subSubDict -> subDict -> dict )
                            tmpParentDictID = dictDB(tmpParentDictID)%parentDictID

                            if (tmpParentDictID.eq.0) then
                                Call writeErrorFileDict("Short Key is not defined previously. : ", tmpLine)
                                exit
                            end if

                            !! Search Dictionary in parent dictionary
                            isKeyExist = dictDB(tmpParentDictID)%subDictTable%keyExist(shortKey)

                            if (isKeyExist) then

                                !! Get Dictionary ID
                                Call dictDB(tmpParentDictID)%subDictTable%getInt(shortKey, shortKeyDictID)

                                !! Add Dictionary
                                Call dictDB(dictID)%addDict( dictDB(shortKeyDictID) )
                                keySaved = "";              isKeyActive = .FALSE.

                                exit

                            end if

                        end do

                        if (isKeyExist.EQV..FALSE.) then
                            Call writeErrorFileDict("No dictionary is defined previously. ", tmpLine)
                        end if

                    else
                        Call writeErrorFileDict("Wrong End. No end after dictionary short key.", tmpLine)
                    end if

                end if

            else
                if (nArgValid.eq.1) then

                    !! Add Word to Array
                    if (isWord(scFlag(1:1))) then
                        if (isArrayActive) then
                            Call addSaveArray(nSaveData,            &
                                              tmpSaveCArg,          &
                                              tmpSaveRArg,          &
                                              tmpSaveNumFlag,       &
                                              tmpSaveFlag,          &
                                              cArg(1:1),    &
                                              rArg(1:1),    &
                                              numFlag(1:1), &
                                              scFlag(1:1))
                        else
                            Call writeErrorFileDict("Bad line. Key : ", trim(keySaved))
                        end if

                    !! Dictionary Start
                    else if (scFlag(1).eq.1101) then

                        iLine = iLine - 1

                        !!... Need Add
                        Call readMakeCharArrayFromCharArray(charArray(iLine:), &
                                                            subDictCharArray, &
                                                            nLineSubDict, &
                                                            "{",&
                                                            "}" ,.TRUE.)

                        if (nLineSubDict.ge.1) then

                            !! Call Sub Dict
                            Call initDictDB_NameCharArray(keySaved, &
                                                        subDictCharArray(2:nLineSubDict),&
                                                        subDictID, &
                                                        dictID)

                            !! Skip line
                            iLine = iLine + nLineSubDict

                            !! Add SubDict
                            Call dictDB(dictID)%subDictTable%push(keySaved, subDictID)
                            isKeyActive = .FALSE.;       keySaved=""

                        end if

                    !! Dictionary End
                    else if (scFlag(1).eq.1102) then

                        Call writeErrorFileDict("Bad line. Key : ", trim(keySaved))

                    !! Array Start
                    else if ((scFlag(1).eq.1201).or.(scFlag(1).eq.1001)) then

                        if (isArrayActive.EQV..FALSE.) then
                            nSaveData     = 0
                            isArrayActive = .TRUE.
                        else
                            Call writeErrorFileDict("Wrong Format. Bad array start (Another array exist.)", tmpLine)
                        end if

                    !! Array End
                    else if ((scFlag(1).eq.1202).or.(scFlag(1).eq.1002)) then
                        Call writeErrorFileDict("Wrong Format. Bad array end", tmpLine)

                    !! Bad Line
                    else
                        Call writeErrorFileDict("Bad line.", tmpLine)
                    end if

                else if (nArgValid.ge.2) then

                    !! Word Start
                    if (isWord(scFlag(1:1))) then

                        if (scFlag(nArgValid).eq.0) then

                            if (isArrayActive) then

                                if ((scFlag(nArgValid-1).eq.1202).or.(scFlag(nArgValid-1).eq.1002)) then

                                    nTmpData = nArgValid - 2
                                    !! Save Data
                                    if (nTmpData.ge.1) then
                                        Call addSaveArray(nSaveData,            &
                                                          tmpSaveCArg,          &
                                                          tmpSaveRArg,          &
                                                          tmpSaveNumFlag,       &
                                                          tmpSaveFlag,          &
                                                          cArg(1:nArgValid-2),    &
                                                          rArg(1:nArgValid-2),    &
                                                          numFlag(1:nArgValid-2), &
                                                          scFlag(1:nArgValid-2))
                                    end if

                                    !! Push
                                    if ( isWord(tmpSaveFlag) ) then
                                        if (isAllTrue(tmpSaveNumFlag)) then
                                            !! Push Number Value
                                            Call dictDB(dictID)%sLTable%push(keySaved, tmpSaveRArg)
                                        else
                                            !! Push Char Value
                                            Call dictDB(dictID)%sLTable%push(keySaved, tmpSaveCArg)
                                        end if
                                        keySaved = "";              isKeyActive = .FALSE.
                                    else
                                        Call writeErrorFileDict("Wrong contents in array. key : ", keySaved)
                                    end if
                                    isArrayActive = .FALSE.

                                else
                                    Call writeErrorFileDict("Wrong Format. Bad array end", tmpLine)
                                end if

                            else
                                Call writeErrorFileDict("Bad line.", tmpLine)
                            end if

                        else if ((scFlag(nArgValid).eq.1202).or.(scFlag(nArgValid).eq.1002)) then
                            Call writeErrorFileDict("Wrong Format. Bad array end", tmpLine)

                        else

                            !!! Add Array
                            if (isArrayActive) then
                                if (isWord(scFlag)) then
                                    nTmpData = nArgValid - 2
                                    !! Save Data
                                    if (nTmpData.ge.1) then
                                        Call addSaveArray(nSaveData,        &
                                                          tmpSaveCArg,      &
                                                          tmpSaveRArg,      &
                                                          tmpSaveNumFlag,   &
                                                          tmpSaveFlag,      &
                                                          cArg,             &
                                                          rArg,             &
                                                          numFlag,          &
                                                          scFlag)
                                    end if
                                else
                                    Call writeErrorFileDict("Bad line.", tmpLine)
                                end if
                            end if

                        end if

                    !! Dictionary Start
                    else if (scFlag(1).eq.1101) then

                        Call writeErrorFileDict(" Dictionary character { should be separated.", tmpLine)

                    !! Dictionary End
                    else if (scFlag(1).eq.1102) then

                        Call writeErrorFileDict(" Dictionary character } should be separated.", tmpLine)

                    !! Array Start
                    else if ((scFlag(1).eq.1201).or.(scFlag(1).eq.1001)) then

                        if (isArrayActive.EQV..FALSE.) then

                            if (scFlag(nArgValid).eq.0) then

                                !! Corret Array End
                                if ((scFlag(nArgValid-1).eq.1202).or.(scFlag(nArgValid-1).eq.1002)) then
                                    !! Push
                                    if ( isWord(scFlag(2:nArgValid-2)) ) then
                                        if (isAllTrue(numFlag(2:nArgValid-2))) then
                                            !! Push Number Value
                                            Call dictDB(dictID)%sLTable%push(keySaved, rArg(2:nArgValid-2) )
                                        else
                                            !! Push Char Value
                                            Call dictDB(dictID)%sLTable%push(keySaved, cArg(2:nArgValid-2) )
                                        end if
                                        keySaved = "";              isKeyActive = .FALSE.
                                    else
                                        Call writeErrorFileDict("Wrong contents in array. key : ", keySaved)
                                    end if
                                !! Wrong Array End
                                else
                                    Call writeErrorFileDict("Wrong Format. Bad array end", tmpLine)
                                end if

                            else if ((scFlag(nArgValid).eq.1202).or.(scFlag(nArgValid).eq.1002)) then

                                Call writeErrorFileDict("Wrong Format. Bad array end", tmpLine)

                            else
                                !! Add Array
                                if (isWord( scFlag(2:nArgValid) )) then
                                    nTmpData = nArgValid - 1
                                    !! Save Data
                                    if (nTmpData.ge.1) then
                                        nSaveData     = 0
                                        Call addSaveArray(nSaveData,            &
                                                          tmpSaveCArg,          &
                                                          tmpSaveRArg,          &
                                                          tmpSaveNumFlag,       &
                                                          tmpSaveFlag,          &
                                                          cArg(2:nArgValid),    &
                                                          rArg(2:nArgValid),    &
                                                          numFlag(2:nArgValid), &
                                                          scFlag(2:nArgValid))
                                        isArrayActive = .TRUE.
                                    end if
                                else
                                    Call writeErrorFileDict("Wrong line description for array. ", tmpLine)
                                end if

                            end if

                        else
                            Call writeErrorFileDict("Wrong Format. Bad array start (Another array exist.)", tmpLine)
                        end if

                    !! Array End
                    else if ((scFlag(1).eq.1202).or.(scFlag(1).eq.1002)) then

                        if (isArrayActive.and.(scFlag(2).eq.0)) then
                            !! Push
                            if ( isWord(tmpSaveFlag) ) then
                                if (isAllTrue(tmpSaveNumFlag)) then
                                    !! Push Number Value
                                    Call dictDB(dictID)%sLTable%push(keySaved, tmpSaveRArg)
                                else
                                    !! Push Char Value
                                    Call dictDB(dictID)%sLTable%push(keySaved, tmpSaveCArg)
                                end if
                                keySaved = "";              isKeyActive = .FALSE.
                            else
                                Call writeErrorFileDict("Wrong contents in array. key : ", keySaved)
                            end if
                            isArrayActive = .FALSE.
                        else
                            Call writeErrorFileDict("Wrong Format. Bad array end", tmpLine)
                        end if

                    !! Bad Line
                    else
                        Call writeErrorFileDict("Bad line.", tmpLine)
                    end if

                else

                end if
            end if
        end if

    end do

End Subroutine

Subroutine writeErrorFileDict(errorMessage, fileLine, lineNumber)
    Implicit None
    CHARACTER(len=*), intent(in) :: errorMessage
    CHARACTER(len=*), optional   :: fileLine
    integer, optional            :: lineNumber

    write(*,*) ""
    write(*,1000) trim(errorMessage)
    write(*,*) ""
    if (present(fileLine)) then
    write(*,1001) trim(fileLine)
    write(*,*) ""
    end if
    if (present(lineNumber)) then
    write(*,1002) lineNumber
    end if
    stop

1000 format("    [ERROR] ",a)
1001 format(a)
1002 format("    n-th line : ", i10 )

End Subroutine
