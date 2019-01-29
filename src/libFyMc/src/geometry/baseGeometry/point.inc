!! ------------------------------------------------------------------ !!
!! Procedures : Type Point
!! ------------------------------------------------------------------ !!

    Subroutine setPoint(this, pidx, x, y, z)
        Implicit None
        class(typPoint), intent(inout) :: this
        Integer, intent(in)            :: pidx
        Real(RP),intent(in)            :: x, y, z
        this%idx_ = pidx
        this%vec_(1) = x;    this%vec_(2) = y;    this%vec_(3) = z
    End Subroutine

    Subroutine movePoint(this, x, y, z)
        Implicit None
        class(typPoint), intent(inout) :: this
        Real(RP),intent(in)            :: x, y, z
        this%vec_(1) = x;    this%vec_(2) = y;    this%vec_(3) = z
    End Subroutine

    Real(RP) Function pointX(this)
        Implicit None
        class(typPoint), intent(in) :: this
        pointX = this%vec_(1)
    End Function

    Real(RP) Function pointY(this)
        Implicit None
        class(typPoint), intent(in) :: this
        pointY = this%vec_(2)
    End Function

    Real(RP) Function pointZ(this)
        Implicit None
        class(typPoint), intent(in) :: this
        pointZ = this%vec_(3)
    End Function

    Function pointVec(this) result(vec)
        Implicit None
        class(typPoint), intent(in) :: this
        Real(RP), Dimension(3) :: vec
        vec = this%vec_
    End Function

!! ------------------------------------------------------------------ !!
!! End Procedures
!! ------------------------------------------------------------------ !!
