Subroutine getGauss1Coordinates(nGaussP, N1, N2, W)
    Implicit None
    Integer, intent(in)                       :: nGaussP
    Real(RP), dimension(nGaussP), intent(out) :: N1, N2, W
    !! Local Variables
    real(rp),dimension(nGaussP) :: xi
    integer :: i

    !! x - directional
    if (nGaussP.eq.1) then
        xi = gaussXi1
        W = gaussW1 / 2.0_RP
    else if (nGaussP.eq.2) then
        xi = gaussXi2
        W = gaussW2 / 2.0_RP
    else if (nGaussP.eq.3) then
        xi = gaussXi3
        W = gaussW3 / 2.0_RP
    else if (nGaussP.eq.4) then
        xi = gaussXi4
        W = gaussW4 / 2.0_RP
    else if (nGaussP.eq.5) then
        xi = gaussXi5
        W = gaussW5 / 2.0_RP
    else
        write(*,*) " "
        write(*,*) "    [ERROR] getGauss1Coordinates()"
        write(*,*) "        nXGaussPoint should be less than ", nGaussMax
        write(*,*) "        given nX Gauss Point : ", nGaussP
        write(*,*) " "
        stop
    end if

    do i = 1, nGaussP
        N1(i) =  (1.0_RP - xi(i)) / 2.0_RP
        N2(i) =  (1.0_RP + xi(i)) / 2.0_RP
    enddo

End Subroutine

Subroutine getGauss2Coordinates(nXGaussP, nYGaussP, Nx1, Nx2, Ny1, Ny2, W1, W2)
    Implicit None
    Integer, intent(in)           :: nXGaussP, nYGaussP
    Real(RP), dimension(nXGaussP) :: nX1, nX2, W1
    Real(RP), dimension(nYGaussP) :: Ny1, Ny2, W2

    !! Local Variables
    Call getGauss1Coordinates(nXGaussP, Nx1, Nx2, W1)

    Call getGauss1Coordinates(nYGaussP, Ny1, Ny2, W2)

End Subroutine
