Type, public :: typDictionary

private

    character(len=CHAR_LEN), public  :: dictName

    integer, public                  :: dictID

    integer, public                  :: parentDictID

    type(typSLookUpTable), public    :: sLTable

    type(typSLookUpTable), public    :: subDictTable

Contains

    procedure, pass, public :: destroy => destroyDictionary

    !! Add dictionary to another
    !!
    !!  Call addDict(this = typDict, from = typDict)
    !!
    procedure, pass, public :: addDict

    !! Copy Subroutine
    !!
    !!  Call copyDict(to = typDict, from = typDict)
    !!
    procedure, pass, private :: copyDict

    !! Copy Operator
    !!
    !!  typDict to = from
    !!
    generic :: assignment(=) => copyDict


End Type
