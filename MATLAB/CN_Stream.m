function RF = CN_Stream(h, H, type, T, N1, display, N2, n_H)
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
if nargin < 6
    display = 0;
end
if nargin < 1
    RF = RF_initialize;
else
    RF = RF_initialize(h,H,type,T,N1,0,0);
end
%
if nargin < 7
    RF = RF_solve_auto(RF, display);
elseif nargin < 8
    RF = RF_solve_auto(RF, display, N2);
else
    RF = RF_solve_auto(RF, display, N2, n_H);
end
%
% Eulerian current
RF.c_E = RF.c + RF.b(1);
% mass transport
RF.c_S = RF.c_E - RF.Q/RF.h;
%
