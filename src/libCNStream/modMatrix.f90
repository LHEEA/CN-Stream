module modMatrix
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!    Copyright (C) 2019 - LHEEA Res. Dept, Ecole Centrale de Nantes, UMR CNRS 6598
!
!    This program is part of CN-Stream
!
!    CN-Stream is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
use modType

implicit none

contains
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    Subroutine leastSquareInv(m,n,A,b,x)
!-------------------------------------------------------------------------------------------------
!
!   Subroutine for m by n matrix solve by least square method
!
!   Input
!       M, N : Integer
!       A : M by N Matrix
!       B : M by 1 Vector
!
!   Output
!       x : Solution by least square method
!
!               Coded by Y. CHOI (ECN : Youngmyung.Choi@ec-nantes.fr)
!
!-------------------------------------------------------------------------------------------------
    Implicit None
    integer,intent(in)  :: m,n
    real(RP),intent(in) :: A(m,n)
    real(RP),intent(in) :: b(m)

    real(RP),intent(out) :: x(n)

    real(RP),dimension(n,m) :: transA

    real(RP),dimension(n,n) :: leastA
    real(RP),dimension(n)   :: leastB

    integer :: ii,jj,kk
!-------------------------------------------------------------------------------------------------

    do ii = 1,m
        do jj = 1,n
            transA(jj,ii) = A(ii,jj)
        enddo
    enddo

    do ii = 1,n
        do jj = 1,n
            leastA(ii,jj) = 0.0d0
            do kk = 1,m
                leastA(ii,jj) = leastA(ii,jj) + transA(ii,kk) * A(kk,jj)
            enddo
        enddo
    enddo

    do ii = 1,n
        leastB(ii) = 0.d0
        do kk = 1,m
            leastB(ii) = leastB(ii) + transA(ii,kk) * b(kk)
        enddo
        x(ii) = 0.0
    enddo

    Call GaussJ(leastA,n,leastB,1)

    do ii = 1,n
        x(ii) = real(leastB(ii))
    enddo

!-------------------------------------------------------------------------------------------------
    End Subroutine
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    Subroutine GaussJ(A,N,B,M)
!-------------------------------------------------------------------------------------------------
!
!   MATRIX INVERSE SUBROUTINE GAUSSJ
!
!   DESCRIPTION :
!
!   LINEAR EQUATION SOLUTION BY GAUSS-JORDAN ELIMINATION. A IS AN INPUT MATRIX OF N BY N ELEMENTS,
!   STORED IN AN ARRAY OF PHYSICAL DIMENSIONS n BY n. B IS AN INPUT MATRIX OF N BY M CONTAINING
!   THE M RIGHT HAND SIDE VECTORS, STORED IN AN ARRAY OF PHYSICAL DIMENSIONS n BY m. ON OUTPUT,
!   A IS REPLACED BY ITS MATRIX INVERSE, AND B IS REPLACED BY THE CORRESPONDING SET OF SOLUTION
!   VECTORS.
!
!   BOOK :
!
!       NUMERICAL RECIPES
!
!   CODING BY Y. CHOI IN 18TH, JULY, 2014
!
!-------------------------------------------------------------------------------------------------
    Implicit None
    Integer,parameter  :: Nmax = 10000
    Integer,intent(in) :: N,M
    real(RP),intent(inOUT) :: A(n,n),B(n,m)

    integer :: IPTV(Nmax),INDXR(Nmax),INDXC(Nmax)

    integer :: I,j,K,L,LL

    real(RP) :: BIG,DUM
    INTEGER  :: IROW,ICOL

    real(RP) :: PIVINV
!-------------------------------------------------------------------------------------------------

    do j = 1,N
        IPTV(J) = 0
    enddo

    DO I = 1,N
        BIG = 0.D0
        DO J=1,N
            IF (IPTV(J).NE.1) THEN
                DO K = 1,N
                    IF (IPTV(K).EQ.0) THEN
                        IF (DABS(A(J,K)).GE.BIG) THEN
                            BIG = DABS(A(J,K))
                            IROW = J
                            ICOL = K
                        ENDIF
                    ELSEIF (IPTV(K).GT.1) THEN
                        WRITE(*,*) 'SINGULAR MATRIX'
                        READ(*,*)
                    ENDIF
                ENDDO
            ENDIF
        ENDDO

        IPTV(ICOL) = IPTV(ICOL) + 1

        IF (IROW.NE.ICOL) THEN
            DO L = 1,N
                DUM = A(IROW,L)
                A(IROW,l) = A(ICOL,L)
                A(ICOL,L) = DUM
            ENDDO

            DO L = 1,M
                DUM = B(IROW,L)
                B(IROW,L) = B(ICOL,L)
                B(ICOL,L) = DUM
            ENDDO
        ENDIF

        INDXR(I) = IROW
        INDXC(I) = ICOL

        IF (ABS(A(ICOL,ICOL)).LT.MAX(MAXVAL(ABS(A)),MAXVAL(ABS(B)))/huge(1.0_rp)) THEN ! The problem arises when the ratio A(ICOL,L)*PIVINV or  B(ICOL,M)*PIVINV is too large
           WRITE(*,*) 'SINGULAR MATRIX'
           READ(*,*)
        ENDIF


        PIVINV = 1.D0/A(ICOL,ICOL)
        A(ICOL,ICOL) = 1.D0

        DO L = 1,N
            A(ICOL,L) = A(ICOL,L)*PIVINV
        ENDDO

        DO L = 1,M
            B(ICOL,L) = B(ICOL,L)*PIVINV
        ENDDO

        DO LL = 1,N
            IF (LL.NE.ICOL) THEN
                DUM = A(LL,ICOL)
                A(LL,ICOL) = 0.D0
                DO L = 1,N
                    A(LL,L) = A(LL,L) - A(ICOL,L)*DUM
                ENDDO

                DO L = 1,M
                    B(LL,L) = B(LL,L) - B(ICOL,L)*DUM
                ENDDO
            ENDIF
        ENDDO
    ENDDO

    DO L = N,1,-1
        IF (INDXR(L).NE.INDXC(L)) THEN
            DO K=1,N
                DUM = A(K,INDXR(L))
                A(K,INDXR(L)) = A(K,INDXC(L))
                A(K,INDXC(L)) = DUM
            ENDDO
        ENDIF
    ENDDO
!-------------------------------------------------------------------------------------------------
    End Subroutine
!-------------------------------------------------------------------------------------------------

end module modMatrix
