Subroutine addDict(this, from)
    Implicit None
    Class(typDictionary), intent(inout) :: this
    Type(typDictionary), intent(in)     :: from

    !! Data Add
    Call this%sLTable%add(from%sLTable)

    !! Sub Dict Add
    Call this%subDictTable%add(from%subDictTable)

End Subroutine

Subroutine copyDict(to, from)
    Implicit None
    Class(typDictionary), intent(inout) :: to
    Type(typDictionary), intent(in)     :: from

    !! Distroy
    Call to%destroy()

    to%dictName     = from%dictName
    to%dictID       = from%dictID
    to%parentDictID = from%parentDictID

    to%sLTable      = from%sLTable
    to%subDictTable = from%subDictTable

End Subroutine

Subroutine destroyDictionary(this)
    Implicit None
    Class(typDictionary), intent(inout) :: this

    this%dictName = ""
    this%dictID   = 0

    Call this%sLTable%destroy()
    Call this%subDictTable%destroy()

End Subroutine
