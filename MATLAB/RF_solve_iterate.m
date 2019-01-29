function [RF, iter, exit_type] = RF_solve_iterate(RF, iter, i_H, n_H, display, modes)
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
% RF_solve_iterate gives Rienecker and Fenton solution
% RF = RF_solve_iterate(RF, display, N2, n_H)
%
% Description
% The RF_solve_iterate function is part of Riencker and Fenton solution package.
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
% See also RF_solve, RF_solve_auto, RF_calcuvcs, RF_calcer, RF_calcA.
%

% initialisation
iter.N_iter = 0;
iter.F_max  = 1;
iter.DY_max = 1;
% screen display
if modes == 0
    text = 'starting new height';
elseif modes == -1
    text = 'reducing N1';
elseif modes == -2
    text = 'increasing N1';
else
    text = '';
end
fprintf(1, 'H = %g (%d over %d, %s)\n', RF.H, i_H-1, n_H, text);
screen_display(iter.N_iter, iter.F_max, RF.N1, RF.N2)
if display
    iter = RF_iter_store(iter, i_H, RF);
    RF_display_results(RF)
end
while (iter.F_max > RF.eps_err && iter.DY_max > RF.eps_inc && iter.N_iter < RF.itermax)
    % find new solution
    [RF, iter.F_max, iter.DY_max] = RF_solve(RF);
    % wave elevation Fourier components
    RF = RF_calc_a(RF);
    % pente
    RF = RF_calc_slope(RF);
    iter.N_iter = iter.N_iter+1;
    % screen display
    screen_display(iter.N_iter, iter.F_max, RF.N1, RF.N2)
    if iter.F_max > RF.err_max
        fprintf(1, '\b, increm: %1.1e Divergence detected\n', iter.DY_max);
        exit_type = 'divergence';
        break
    end
    if iter.DY_max <= RF.eps_inc
        fprintf(1, '\b DY reach minimum detected\n');
        exit_type = 'min DY';
        break
    end
    exit_type = 'normal';
    if display
        iter = RF_iter_store(iter, i_H, RF);
        iter.N_it(i_H) = iter.N_iter;
        if mod(iter.N_iter,50) == 0
            RF_iter_plot(RF, n_H, iter)
        end
    end
end
if strcmp(exit_type, 'normal')
    % screen display
%     fprintf(1, '\n');
    screen_display(iter.N_iter, iter.F_max, RF.N1, RF.N2, 'final display')
    fprintf(1, '\n');
    %     fprintf(1, [format_next, '\b', format_comm, '\n'], iter.N_iter, iter.F_max, RF.N1, RF.N2);
end
if display
    RF_iter_plot(RF, n_H, iter)
    RF_display_results(RF);
    figure(5),
    subplot(2,1,1)
    add_horizontal(RF.eps_err);
    subplot(2,1,2)
    add_horizontal(RF.eps_inc);
end

function screen_display(N_iter, F_max, N1, N2, test)

format_comm = '%3d, residu: %1.1e, N1: %3d, N2: %3d';
format_1st  = 'Iteration: ';
if N_iter == 0
    fprintf(1, [format_1st, format_comm, '\n'], N_iter, F_max, N1, N2);
elseif mod(N_iter, 10) == 0 | nargin > 4
    format_next = '';
    standard_text = sprintf(format_comm,200,3e-14,100,200);
    for n=1:length(standard_text)
        format_next = [format_next '\b'];
    end
    fprintf(1, [format_next, format_comm], N_iter, F_max, N1, N2);
end
