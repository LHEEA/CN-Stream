function [RF, result] = RF_solve_auto(RF, display, N2, n_H)
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
% RF_solve_auto gives Rienecker and Fenton solution
% RF = RF_solve_auto(RF, display, N2, n_H)
%
% Description
% The RF_solve_auto function is part of Riencker and Fenton solution package.
% it is based on paper by M.M. Rienecker and J.D. Fenton,
% A Fourier approximation method for steady water waves.
% J. Fluid Mech., Vol. 104, pp. 119-137, 1981.
%
% Input
% RF structure which contains 4 parameters
%   RF.h        dimensional depth
%   RF.H        dimensional wave height
%   RF.N1       number of modes for stream function
%   RF.lorT     input wavelength (=1) or period (=0)
%   RF.lambda or RF.T wavelength or period
% display       1 or 0 when intermediate display is required or not
% N2            (optional) number of modes for eta
% n_H           (optional) number of intermediate steps in wave height
%
% implemented by Pierre FERRANT and modified by Felicien BONNEFOY - LHEEA - 2015
%
% See also RF_solve, RF_calcuvcs, RF_calcer, RF_calcA.
%
N1 = RF.N1;
auto_N2 = 0;
if nargin < 3
    auto_N2 = 1;
elseif isempty(N2)
    auto_N2 = 1;
end
if auto_N2
    N2 = max(N1+5, 2*N1);
end
RF.N2 = N2;
if nargin < 4
    n_H = 10;
end
% store
RF.eps_err = 1e-14;
RF.err_max    = 1e1;
RF.eps_inc = 7e-14;
RF.eps_N1  = 1e-13;
RF.itermax = 999;
% wave height increments
H_target = RF.H;
increment_type = 1;
if increment_type % exponential
    H_start  = 0.01 * H_target;
    dH = (exp(1)-1) / n_H;
else % linear
    H_start  = H_target / (n_H+1);
    dH = H_start;
end
% initial solution
RF = RF_initial_solution(RF, H_start);
% Loop on wave heights
iter.result = zeros(n_H+1,RF.itermax+1);
iter.N_it   = zeros(n_H+1,1);
for i1=1:n_H+1
    if n_H==0
        H = H_target;
    else
        if increment_type
            H = H_start + (H_target-H_start)*log(1+(i1-1)*dH);
        else
            H = H_start + (i1-1)*dH;
        end
    end
    RF.H  = H;
    modes = 0;
    loop = [0,0];
    while modes < 1
        [RF, iter, exit_type] = RF_solve_iterate(RF, iter, i1, n_H, display, modes);
        iteration=['it ' num2str(i1) 'solved for H ' num2str(H)]
        RF = RF_calc_a(RF);
        [modes, N1, N2, RF] = RF_decide(RF, exit_type);
        % adjust array size (zero-pad if necessary)
        if length(RF.b) <= N1
            RF.b(length(RF.b)+1:N1+1) = 0;
        end
        RF.N1 = N1;
        if length(RF.a) <= N2
            RF.a(length(RF.a)+1:N2+1) = 0;
        end
        RF.N2 = N2;
        % recompute usefull quantities on refined modes
        RF = RF_calc_eta(RF);
        RF = RF_calc_slope(RF);
    end
    result.H(i1) = RF.H;
    result.slope(i1) = max(abs(RF.slope));
    result.N1(i1) = RF.N1;
    result.N2(i1) = RF.N2;
    result.N1_eff(i1) = RF.N1_eff;
    result.N2_eff(i1) = RF.N2_eff;
    result.T(i1) = RF.T;
    result.k(i1) = RF.k;
    %     % screen display
    %     fprintf(1, 'R : %g, Q: %g\n', RF.R, RF.Q);
end
end
