type, private :: typFileIO

    Character(len=1000)     :: fileDir

    Character(len=1500)     :: filePath

    Character(len=CHAR_LEN) :: fileName

    Character(len=CHAR_LEN) :: ext

    Character(len=CHAR_LEN) :: fileStatus

    Integer                 :: fileIndex

    logical                 :: isOpened

Contains

    procedure, pass, public :: initialize => initialize_FileIO

    procedure, pass, public :: destroy => destroyFileIO

end type
