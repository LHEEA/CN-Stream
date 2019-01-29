Type, public :: typDictionaryPtr

    Type(typDictionary), pointer :: ptrDict

Contains

    !!!... Initialize dictionary with name
    !!
    !! Call initDict_Name(typDict, "dictName")
    !!
    procedure, pass, private :: initDict_Name

    !... Initialize dictionary with name and lookUpTable
    !
    ! Call initDict_Name(typDict, "dictName", typSLookUpTable)
    !
    procedure, pass, private :: initDict_NameLookUpTable

    !... Initialize dictionary with name and lookUpTable
    !
    ! Call initDict_Name(typDict, fileDir, fileName, ext)
    !
    procedure, pass, private :: initDict_File


    !! Public Procedure ---------------------------------------

    !... Initializer Overloading
    !
    ! Call initDict_Name(typDict, "dictName", typSLookUpTable)
    !
    generic :: initialize => initDict_Name, &
                             initDict_NameLookUpTable, &
                             initDict_File

    !!... Print Dictionary
    procedure, pass, public :: print => printDictionary

    !!... Return SubDictionary
    procedure, pass, public :: subDict => subDictionaryPtr

    !!... nData
    procedure, pass, public :: nData => nData_Dict

    !!... nSubDict
    procedure, pass, public :: nSubDict => nSubDict_Dict

    !!... Get Key Name
    procedure, pass, public :: getKey => getDictKey

    !!... Get Sub Dict Name
    procedure, pass, public :: getSubDictName => getDictSubDictName

    !!... Get Real Values
    procedure, pass, public :: getReal => getDictReal

    !!... Get Real Array
    procedure, pass, public :: getRealArray => getDictRealArray

    !!... Get Int Values
    procedure, pass, public :: getInt => getDictint

    !!... Get Int Array
    procedure, pass, public :: getIntArray => getDictIntArray

    !!... Get Char Values
    procedure, pass, public :: getChar => getDictChar

    !!... Get Char Array
    procedure, pass, public :: getCharArray => getDictCharArray

    !!... Get String Values
    procedure, pass, public :: getString => getDictString

    !!... Get String Array
    procedure, pass, public :: getStringArray => getDictStringArray

    !!... Get Logical Values
    procedure, pass, public :: getLogical => getDictLogical

    !!... Get Logical Array
    procedure, pass, public :: getLogicalArray => getDictLogicalArray

    !!... Get Real Values or set value with given value if key is not exist
    procedure, pass, public :: getRealOrDefault => getDictRealOrDefault

    !!... Get Real Array or set value with given value if key is not exist
    procedure, pass, public :: getRealArrayOrDefault => getDictRealArrayOrDefault

    !!... Get Int Values or set value with given value if key is not exist
    procedure, pass, public :: getIntOrDefault => getDictintOrDefault

    !!... Get Int Array or set value with given value if key is not exist
    procedure, pass, public :: getIntArrayOrDefault => getDictIntArrayOrDefault

    !!... Get Char Values or set value with given value if key is not exist
    procedure, pass, public :: getCharOrDefault => getDictCharOrDefault

    !!... Get Char Array or set value with given value if key is not exist
    procedure, pass, public :: getCharArrayOrDefault => getDictCharArrayOrDefault

    !!... Get String Values or set value with given value if key is not exist
    procedure, pass, public :: getStringOrDefault => getDictStringOrDefault

    !!... Get String Array or set value with given value if key is not exist
    procedure, pass, public :: getStringArrayOrDefault => getDictStringArrayOrDefault

    !!... Get Logical Values or set value with given value if key is not exist
    procedure, pass, public :: getLogicalOrDefault => getDictLogicalOrDefault

    !!... Get Logical Array or set value with given value if key is not exist
    procedure, pass, public :: getLogicalArrayOrDefault => getDictLogicalArrayOrDefault

    !!... Set integer Values as a dictionary member
    procedure, pass, private :: setInt => setDictInt

    !!... Set integer array se be a dictionary member
    procedure, pass, private :: setIntArray => setDictIntArray

    !!... Set real Values to be a dictionary member
    procedure, pass, private :: setReal => setDictReal,     &
                                           setDictRealRP,   &
                                           setDictDble

    !!... Set real array to be a dictionary member
    procedure, pass, private :: setRealArray => setDictRealArray,   &
                                                setDictRealRPArray, &
                                                setDictDbleArray

    !!... Set character to be a dictionary member
    procedure, pass, private :: setChar => setDictChar

    !!... Set character array to be a dictionary member
    procedure, pass, private :: setCharArray => setDictCharArray

    !!... Set character to be a dictionary member
    procedure, pass, private :: setLogical => setDictLogical

    !!... Set character array to be a dictionary member
    procedure, pass, private :: setLogicalArray => setDictLogicalArray

    !!... Set sub-dictionary
    procedure, pass, public  :: setSubDict => setDictDict

    !!... Add dictionary
    procedure, pass, public  :: addDict => addDictDict

    !!... Generic procedure
    generic :: set => setInt, setIntArray, &
                      setReal, setRealArray, &
                      setChar, setCharArray, &
                      setLogical, setLogicalArray

    procedure, pass, public :: writeFile => writeDict2File

End Type
