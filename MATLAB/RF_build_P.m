function P = RF_build_P(RF, t_loc, z_loc, x_loc, t_0, hydrostatic)
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
U = RF_build_U(RF, t_loc, z_loc, x_loc, t_0);
W = RF_build_W(RF, t_loc, z_loc, x_loc, t_0);
P = zeros(length(z_loc),5);
% estimates pressure at position (x_loc,z_loc) and time t_loc
P(:,1)  = - RF.c^2/2 + RF.R;
for m = 1:RF.N1
    P(:,2) = P(:,2) + RF.b(m+1) * m*RF.k*RF.c * cosh(m*RF.k*(z_loc+RF.h)).' ./  cosh(m*RF.k*RF.h) * cos(m*RF.k*(x_loc -RF.c*(t_loc-t_0)));
end
P(:,3) = - (U.^2 + W.^2) / 2; % non-lin�aire
if hydrostatic
    P(:,4) = - RF.g*z_loc; % hydrostatique
else
    P(:,4) = 0;
end
P(:,5) = P(:,1) + P(:,2) + P(:,3) + P(:,4);
end
