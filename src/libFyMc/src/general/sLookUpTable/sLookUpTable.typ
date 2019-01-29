Type, public :: typSLookUpTable

private

    !! Number of Keys (only increase)
    integer, public :: nKey = 0

    !! Number of Tables (Saving Space)
    integer :: nTable = 0

    !! sLookUpTable Name
    Character(len=CHAR_LEN) :: name

    !! Strong Search Option (true : check upper and lower)
    logical :: strongSearch = .FALSE.

    !! key array saved in sLookUpTable
    type(typString), allocatable    :: keys(:)

    !! data array saved in sLookUpTable
    type(typDataArray), allocatable :: dataArray(:)

    !! key activation array
    logical, allocatable            :: isKeyActive(:)

contains

    !!!... Private Procedures

    !!!... initialize sLookUpTable with default setting
    !!
    !! Call initializeSLtable(this, char=name, opt = .strongSearch., int=tableSize)
    !!
    procedure, pass, public :: initialize => initializeSLtable

    !!... push real or real array to sLookUpTable
    !!
    !! Call pushSLTableReal(this, "key", real )
    !!
    !! Call pushSLTableReal(this, "key", realArray )
    !!
    procedure, pass, private :: pushSLTableReal, pushSLTableRealArray


    !!... push real or real array to sLookUpTable
    !!
    !! Call pushSLTableReal(this, "key", real_RP )
    !!
    !! Call pushSLTableReal(this, "key", realArray_RP )
    !!
    procedure, pass, private :: pushSLTableRealRP, pushSLTableRealRPArray


    !!... push integer or integer array to sLookUpTable
    !!
    !! Call pushSLTableInt(this, "key", integer )
    !!
    !! Call pushSLTableIntArray(this, "key", integerArray )
    !!
    procedure, pass, private :: pushSLTableInt, pushSLTableIntArray

    !!... push character or character array to sLookUpTable
    !!
    !! Call pushSLTableChar(this, "key", character )
    !!
    !! Call pushSLTableCharArray(this, "key", characterArray )
    !!
    procedure, pass, private :: pushSLTableChar, pushSLTableCharArray

    !!... push logical or logical array to sLookUpTable
    !!
    !! Call pushSLTableLogical(this, "key", .logical. )
    !!
    !! Call pushSLTableLogicalArray(this, "key", .logicalArray. )
    !!
    procedure, pass, private :: pushSLTableLogical, pushSLTableLogicalArray

    !!... Print LookUpTable Contents
    !!
    !! Call printSearchError(this)
    !!
    procedure, pass, private :: printSearchError

    !!... Search keyIndex in sLookUpTable
    !!
    !! Call searchKeyIndex(this, "key", int = keyIndex, logical = isExist)
    !!
    procedure, pass, private :: searchKeyIndex

    !!... Search keyIndex in sLookUpTable and add if it exist not
    !!
    !! Call searchKeyIndexAndAdd(this, "key", int = keyIndex, logical = isExist)
    !!
    procedure, pass, private :: searchKeyIndexAndAdd

    !!... Reallocate LookUpTable with given tableSize
    !!
    !! Call reallocateSLTable(this, int = tableSize)
    !!
    procedure, pass, private :: reallocateSLTable

    !!... Copy LookUpTable to LookUpTable
    !!
    !! Call reallocateSLTable(this, int = tableSize)
    !!
    procedure, pass, private :: copySLTable

    !!!... Public Procedures

    !!... Initiaize sLookUpTable
    !! generic :: initialize => initializeSLtable

    !!... Push data to sLookUpTable
    generic :: push => pushSLTableReal, pushSLTableRealArray, &
                       pushSLTableRealRP, pushSLTableRealRPArray, &
                       pushSLTableInt, pushSLTableIntArray, &
                       pushSLTableChar, pushSLTableCharArray, &
                       pushSLTableLogical, pushSLTableLogicalArray

    !!... Get data array
    !!
    !! Call getDataArray(this, ikey)
    !!
    procedure, pass, public :: getDataArray => getSLTableDataArray

    !!... Get key character for given keyIndex
    !!
    !! Call getReal(this, "key", real)
    !!
    procedure, pass, public :: getKey => getSLTableKey

    !!... Get real from given sLookUpTable with keyword
    !!
    !! Call getReal(this, "key", real)
    !!
    procedure, pass, public :: getReal => getSLTableReal

    !!... Get real array from given sLookUpTable with keyword
    !!
    !! Call getRealArray(this, "key", realArray)
    !!
    procedure, pass, public :: getRealArray => getSLTableRealArray

    !!... Get integer from given sLookUpTable with keyword
    !!
    !! Call getRealArray(this, "key", integer)
    !!
    procedure, pass, public :: getInt => getSLTableInt

    !!... Get integer array from given sLookUpTable with keyword
    !!
    !! Call getRealArray(this, "key", integerArray)
    !!
    procedure, pass, public :: getIntArray => getSLTableIntArray

    !!... Get string from given sLookUpTable with keyword
    !!
    !! Call getRealArray(this, "key", typString)
    !!
    procedure, pass, public :: getString => getSLTableString

    !!... Get string array from given sLookUpTable with keyword
    !!
    !! Call getRealArray(this, "key", typStringArray)
    !!
    procedure, pass, public :: getStringArray => getSLTableStringArray

    !!... Get character from given sLookUpTable with keyword
    !!
    !! Call getChar(this, "key", "character")
    !!
    procedure, pass, public :: getChar => getSLTableChar

    !!... Get character array from given sLookUpTable with keyword
    !!
    !! Call getChar(this, "key", "characterArray")
    !!
    procedure, pass, public :: getCharArray => getSLTableCharArray

    !!... Get logical from given sLookUpTable with keyword
    !!
    !! Call getChar(this, "key", .logical.)
    !!
    procedure, pass, public :: getLogical => getSLTableLogical

    !!... Get logical array from given sLookUpTable with keyword
    !!
    !! Call getChar(this, "key", .logicalArray.)
    !!
    procedure, pass, public :: getLogicalArray => getSLTableLogicalArray

    !!... delete keyword defined in sLookUpTable
    !!
    !! Call delte(this, "key")
    !!
    procedure, pass, public :: delete  => deleteSLTableData

    !!... Search keyword defined in sLookUpTable
    !!
    !! .logical. = keyExist(this, "key")
    !!
    procedure, pass, public :: keyExist => keyExistSLTableData


    !!... Search keyword defined in sLookUpTable and Return Logical type
    !!
    !! .logical. = isNumber(this, "key")
    !!
    procedure, pass, public :: isNumber => isNumberSLTableData

    !!... Search keyword defined in sLookUpTable and Return Logical type
    !!
    !! .logical. = isInt(this, "key")
    !!
    procedure, pass, public :: isInt => isIntSLTableData

    !!... Search keyword defined in sLookUpTable and Return Logical type
    !!
    !! .logical. = isCharacter(this, "key")
    !!
    procedure, pass, public :: isCharacter => isCharSLTableData

    !!... Search keyword defined in sLookUpTable and Return Logical type
    !!
    !! .logical. = isLogical(this, "key")
    !!
    procedure, pass, public :: isLogical => isLogicalSLTableData

    !!... Write sLookUpTable contents
    !!
    !! Call print(this)
    !!
    procedure, pass, public :: print => printSLTable

    !!... Copy Operator
    !!
    !! sLookUpTable = sLookUpTable
    !!
    generic :: assignment(=) => copySLTable

    !!... Add Key and Data from another sLTable
    !!
    !! Call add(this, target)
    !!
    procedure, pass, public :: add => addSLTable


    !!... Destroy LookUpTable
    !!
    !! Call destroy(this)
    !!
    procedure, pass, public :: destroy => destroySLTable

End Type
