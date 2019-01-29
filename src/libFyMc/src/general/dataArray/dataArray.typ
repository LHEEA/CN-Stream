type, public ::  typDataArray

    integer :: nData = 0

    logical :: isNum = .FALSE.

    logical :: isInt = .FALSE.

    logical :: isStr = .FALSE.

    logical :: isLog = .FALSE.

    Real(RP), allocatable :: arrR(:)

    integer, allocatable :: arrI(:)

    type(typString), allocatable :: arrS(:)

    logical, allocatable :: arrL(:)

Contains

    !!... Private Procedures

    procedure, pass, private :: setDataArrayInt

    procedure, pass, private :: setDataArrayintArray

    procedure, pass, private :: setDataArrayReal

    procedure, pass, private :: setDataArrayRealArray

    procedure, pass, private :: setDataArrayString

    procedure, pass, private :: setDataArrayStringArray

    procedure, pass, private :: setDataArrayChar

    procedure, pass, private :: setDataArrayCharArray

    procedure, pass, private :: setDataArrayLogical

    procedure, pass, private :: setDataArrayLogicalArray

    procedure, pass, private :: setAllfalseDataArray

    !!... Public Procedures

    !! Set Values (Overloading)
    generic :: set => setDataArrayInt, setDataArrayIntArray, &
                      setDataArrayReal, setDataArrayRealArray, &
                      setDataArrayString, setDataArrayStringArray, &
                      setDataArrayChar, setDataArrayCharArray, &
                      setDataArrayLogical, setDataArrayLogicalArray

    procedure, pass, private :: copyDataArray

    generic :: assignment(=) => copyDataArray

    procedure, pass, private :: destroy => destroyDataArray

    ! final                   :: terminateDataArray

end type
