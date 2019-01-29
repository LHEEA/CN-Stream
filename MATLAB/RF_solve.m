function [RF, F_max, DY_max] = RF_solve(RF)
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
% RF_solve solve the equations in the Rienecker and
% Fenton computation
% [RF, F_max] = RF_solve(RF)
%
% Description
% The RF_solve function is part of Riencker and Fenton solution package.
% it is based on paper by M.M. Rienecker and J.D. Fenton,
% A Fourier approximation method for steady water waves.
% J. Fluid Mech., Vol. 104, pp. 119-137, 1981.
%
% implemented by Pierre FERRANT and modified by Felicien BONNEFOY - LHEEA - 2015
%
% See also RF_calcA, RF_calcuvcs, RF_calcer.
%
N1 = RF.N1;
N2 = RF.N2;
%
% Computation of auxiliary tables
[U,V,S1,C1,S2,C2] = RF_calcuvcs(RF);
% Computation of the error vector
F = RF_calcer(U, V, C1, RF);
F_max = max(abs(F));
% Computation of the derivatives' matrix for Newtoin's method
A = RF_calcA(U, V, S1, C1, S2, C2, RF);
% Computation of the correction and new estimation
% Y_0 = zeros(N1+N2+6,1);
% [DY, flag] = lsqr(-A, F, RF.eps_err, RF.err_max, [], [], Y_0);
if rank(A) < size(A,2)
    F_max = 2* RF.err_max; % Divergence
    DY_max = 0;
    warning('Divergence detected...')
    return
end
DY = - A\F;
DY_max = max(abs(DY));
% Redistribution of the solution on natural variables
RF.eta(1:N2+1) = RF.eta(1:N2+1) + DY(1:N2+1).';
RF.b(1:N1+1)   = RF.b(1:N1+1)   + DY(N2+1+(1:N1+1)).';
RF.R = RF.R + DY(N1+N2+3);
RF.c = RF.c + DY(N1+N2+4);
if RF.lorT == 1
    RF.T = RF.T + DY(N1+N2+5);
else
    RF.k = RF.k + DY(N1+N2+5);
end
RF.Q = RF.Q + DY(N1+N2+6);
