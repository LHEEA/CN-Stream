function RF = RF_initial_solution(RF, H)
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
% RF_initial_solution gives second-order solution
% RF = RF_initial_solution(RF, H_start)
%
% Description
% The RF_initial_solution function is part of Riencker and Fenton solution package.
% it is based on paper by M.M. Rienecker and J.D. Fenton,
% A Fourier approximation method for steady water waves.
% J. Fluid Mech., Vol. 104, pp. 119-137, 1981.
%
% Input
% RF structure which contains 7 parameters
%   RF.g        dimensional gravity acceleration
%   RF.h        dimensional depth
%   RF.H        dimensional wave height
%   RF.N1       number of modes for stream function
%   RF.N2       number of modes for wave elevation
%   RF.lorT     input wavelength (=1) or period (=0)
%   RF.lambda or RF.T wavelength or period
% H             initial wave height
%
% Output
% RF structure which 7 more parameters
% RF.k      wavenumber
% RF.c      phase velocity
% RF.eta    free surface elevation
% RF.a      free surface modes
% RF.b      potential modes
% RF.R      Bernoulli constant
% RF.Q      mass flow
% RF.slope  wave slope
%
% implemented by Pierre FERRANT and modified by Felicien BONNEFOY - LHEEA - 2015
%
% See also RF_solve, RF_solve_iterate, RF_solve_auto, RF_calcuvcs, RF_calcer, RF_calcA.
%
if RF.lorT == 1 % wavelength is given
    RF.k = 2*pi/ RF.lambda;
end
% second-order initial solution
%   linear dispersion
if RF.lorT == 1 % wavelength is given
    RF.T = 2*pi/sqrt(RF.g*RF.k*tanh(RF.k * RF.h));
else
    if RF.h == Inf
        RF.k = (2*pi/RF.T)^2 / RF.g;
    else
        RF.k = wave_number(1/RF.T, RF.h);
    end
end
RF.c = 2*pi / (RF.k*RF.T);
%   second-order solution
a = zeros(1,RF.N2+1);
sigma = tanh(RF.k * RF.h);
a(2) = H / 2;
a(3) = RF.k * a(2)^2 * (3 - sigma^2) / (4 * sigma^3);
% build elevation
M = 0:RF.N2;
phase = M*pi/RF.N2;
eta = a(2) * cos(phase) + a(3) * cos(2*phase) + a(4) * cos(3*phase);
% build potential modes
b = zeros(1,RF.N1+1);
b(1) = - RF.c;
b(2) = RF.g * a(2) / (RF.k * RF.c);
b(3) = 3/4 * RF.k * a(2)^2 * (1 - sigma^4) / sigma^3;
Q = 0;
R = RF.c^2/2;
%   storing initial solution
RF.eta = eta;
RF.a   = a;
RF.b   = b;
RF.R   = R;
RF.Q   = Q;
RF = RF_calc_slope(RF);
