public :: separateFilePath

public :: isStringEqual

    !! Compare String & Character
    interface isStringEqual

        module procedure isStringStringEqual

        module procedure isStringCharEqual

        module procedure isCharStringEqual

        module procedure isCharCharEqual

    End interface

public :: replace

    interface replace

        module procedure replaceChar

    end interface

public :: parse


    !! Parse String & Character
    interface parse

        module procedure parseStringString

        module procedure parseCharChar

        module procedure parseStringChar

        module procedure parseCharString

    end interface

public :: parseOneChar

    !! Parse String & Character
    interface parseOneChar

        module procedure parseCharCharOne

    end interface

public :: compact

interface compact

    module procedure compactChar

end interface

private :: strHeader

    interface strHeader

        module procedure stringHeader

        module procedure charHeader

    end interface

private :: findLineEnd

interface findLineEnd

    module procedure findStringLineEnd

    module procedure findCharLineEnd

end interface
