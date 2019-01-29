MODULE modtype
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
!
! Definition of symbols for real type (RP) and complex type (CP)
!
! real numbers are simple or double precision
INTEGER, PARAMETER :: SP = KIND(1.0)
INTEGER, PARAMETER :: DP = KIND(1.0D0)
!
! complex numbers are simple or double precision
INTEGER, PARAMETER :: SPC = KIND(1.0)
INTEGER, PARAMETER :: DPC = KIND(1.0D0)
!
! current type
INTEGER, PARAMETER :: RP = DP
INTEGER, PARAMETER :: CP = DPC
!
! the usual mathematical constants: pi, 2 pi, pi/2, square root of 2.
REAL(RP), PARAMETER    :: PI    = 3.141592653589793238462643383279502888419_rp
REAL(RP), PARAMETER    :: g     = 9.81_rp
REAL(RP), PARAMETER    :: PIO2  = 1.570796326794896619231321691639751442098_rp
REAL(RP), PARAMETER    :: TWOPI = 6.283185307179586476925286766559005768394_rp
REAL(RP), PARAMETER    :: SQ2   = 1.414213562373095048801688724209698078569_rp
!
! For comparison of real numbers
REAL(RP), PARAMETER    :: tiny = epsilon(1.0_rp)
!
REAL(RP), PARAMETER    :: infinite_depth = 1000.0_rp
!
INTEGER, PARAMETER :: StringLength = 200

CONTAINS
!
CHARACTER(LEN=12) FUNCTION  int2str(int)
INTEGER          :: int
!
WRITE(int2str,*) int
int2str = TRIM(ADJUSTL(int2str))
!
END FUNCTION int2str
!
END MODULE modtype
