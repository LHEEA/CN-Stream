type, public :: typString

private

    integer :: nStr_

    Character(len=CHAR_LEN) :: str_

Contains

    procedure, pass, public :: set => setString

    procedure, pass, public :: get => getString

    procedure, pass, public :: clear => clearString

    procedure, pass, public :: getLower => getStringLower

    procedure, pass, public :: getUpper => getStringUpper

    procedure, pass, public :: getInt  => getStringInt

    procedure, pass, public :: getReal => getStringReal

    procedure, pass, private :: copyString

    procedure, pass, public :: destroy => clearString

    generic :: assignment(=) => copyString

end type typString

!! modGeneral Character Parameter

Character(len=7), parameter, private :: specialCharacter = ";(){}[]"

integer, parameter, private :: nSpecialCharacter = 7    !! It should be same with length of specialCharacter

Character(len=2), parameter, private :: commentBlockStart = "/*"

Character(len=2), parameter, private :: commentBlockEnd   = "*/"

Character(len=2),parameter, private :: commentCharList = "!#"

integer, parameter, private :: nCommentCharList = 2    !! It should be same with length of commentCharList

!! Return string header type
!!
!!  -1 : blank line
!!   1 : word
!!   0 : ";" is used.
!!
!!  -100  : line comment
!!
!!  111 : comment block Start
!!  112 : comment block end
!!
!!  122 : "$" is used.
!!
!!  1001 : "[" is used.
!!  1002 : "]" is used.

!!  1101 : "{" is used.
!!  1102 : "}" is used.
!!
!!  1201 : "(" is used.
!!  1202 : ")" is used.
!!
