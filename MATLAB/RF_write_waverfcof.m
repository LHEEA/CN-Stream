function RF_write_waverfcof(RF)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    Copyright (C) 2019 - LHEEA Res. Dept, Ecole Centrale de Nantes, UMR CNRS 6598
%
%    This program is part of CN-Stream
%
%    CN-Stream is free software: you can redistribute it and/or modify
%    it under the terms of the GNU General Public License as published by
%    the Free Software Foundation, either version 3 of the License, or
%    (at your option) any later version.
%
%    This program is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU General Public License for more details.
%
%    You should have received a copy of the GNU General Public License
%    along with this program.  If not, see <http://www.gnu.org/licenses/>.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%% Writes old waverf.cof file (cf. original Fortran implementation)
% WRITE(13,*) DBLE(2*PI/XK) * XL_dim, DBLE(H) * XL_dim, DBLE(XK) / XL_dim,&
%             DBLE(T), DBLE(C), DBLE(CP), DBLE(CC),N1,N2, DBLE(R), DBLE(depth)*XL_dim ! modif_febo octobre 15
XL_dim = 1;
XT_dim = 1;
XV_dim = XL_dim / XT_dim;
%
c_E = RF.c + RF.b(1);
c_S = c_E - RF.Q/RF.h;
%
fid = fopen('waverf.cof', 'w');
fprintf(fid, '%.14e %.14e %.14e\r\n', 2*pi/RF.k * XL_dim, RF.H * XL_dim, RF.k / XL_dim);
c_S = c_S + RF.c; % in the fixed frame
fprintf(fid, '%.14e %.14e %.14e\r\n', RF.T * XT_dim, RF.c * XV_dim, c_S * XV_dim);
fprintf(fid, '%.14e %d %d %.14e\r\n', c_E * XV_dim, RF.N1, RF.N2, RF.R * XV_dim^2);
fprintf(fid, '%.14e\r\n', RF.h * XL_dim);
for n=0:RF.N1
    fprintf(fid, '%d %.14e\r\n', n, RF.b(n+1) * XV_dim * XL_dim);
end
for n=0:RF.N2
    fprintf(fid, '%d %.14e\r\n', n, RF.a(n+1) * XL_dim);
end
fclose(fid);
