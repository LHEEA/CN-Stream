function F = RF_calcer(U,V,C1,RF)
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
% RF_calcer estimates the error terms in the Rienecker and
% Fenton computation
% F = RF_calcer(U,V,C1,RF)
%
% Description
% The RF_calcer function is part of Riencker and Fenton solution package.
% it is based on paper by M.M. Rienecker and J.D. Fenton,
% A Fourier approximation method for steady water waves.
% J. Fluid Mech., Vol. 104, pp. 119-137, 1981.
%
% implemented by Pierre FERRANT and modified by Felicien BONNEFOY - LHEEA - 2015
%
% See also RF_solve, RF_calcA, RF_calcuvcs.
%
g = RF.g;
B = RF.b;
Y = RF.eta;
Q = RF.Q;
R = RF.R;
H = RF.H;
k = RF.k;
c = RF.c;
T = RF.T;
N2 = RF.N2;
N1 = RF.N1;
depth = RF.h;

F = zeros(N2+N2+6,1);
% KFSBC
for I = 1:N2+1
    F(I) = sum(B(2:N1+1) .* C1(2:N1+1,I).') + B(1) * Y(I) + Q;
end
% DFSBC
I = 1:N2+1;
F(N2+1+I) = 0.5 * (U(I).^2 + V(I).^2) + g*Y(I) - R;
% Volume
F(N2+N2+3) = Y(1) + 2 * sum(Y(2:N2)) + Y(N2+1);
% Wave height
F(N2+N2+4) = Y(1) - Y(N2+1) - H;
% Current, equation for c
if RF.current
    % U = mass transport velocity
    F(N2+N2+5) = c - (Q/depth - B(1)) - RF.U;
else
    % U = eulerian current
    F(N2+N2+5) = c + B(1) - RF.U;
end
% wavelength or period
F(N2+N2+6) = k*c*T-2*pi; % equation for T if RF.lorT == 1 or k otherwise
