function [modes, N1, N2, RF] = RF_decide(RF, exit_type)
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
RF = RF_calc_slope(RF);
if 0
    N1_1 = find(abs(RF.b(2:end)/max(abs(RF.b(2:end)))) <= RF.eps_N1, 1, 'first');
    N1_2 = find(abs(RF.b(2:end)/max(abs(RF.b(2:end)))) > RF.eps_N1, 1, 'last')+1;
    if N1_2 == RF.N1
        modes_1 = 0;
    elseif N1_1 == N1_2
        modes_1 = 1;
    elseif loop(1) == 1
        modes_1 = 1;
        loop(1) = 0;
    else
        modes_1 = 0;
    end
    RF.N1_eff = N1_2;
    N2_1 = find(abs(RF.a(2:end)/max(abs(RF.a))) <= RF.eps_N1, 1, 'first');
    N2_2 = find(abs(RF.a(2:end)/max(abs(RF.a))) > RF.eps_N1, 1, 'last')+1;
    if N2_2 == RF.N1
        modes_2 = 0;
    elseif N2_1 == N2_2
        modes_2 = 1;
    elseif loop(2) == 1
        modes_2 = 1;
        loop(2) = 0;
    else
        modes_2 = 0;
    end
    RF.N2_eff = N2_2;
    if strcmp(exit_type, 'divergence')
        modes = 1; % quit the while loop
    elseif modes_1 && modes_2
        % mode numbers are correct
        modes = 1; % quit the while loop
    else
        % increase the number of modes
        if modes_1 == 0
            if RF.N1_eff >= RF.N1
                N1 = floor(1.2*N1);
            elseif RF.N1_eff < RF.N1-10
                N1 = RF.N1_eff+5;
                loop(1) = 1;
            else
                N1 = N1+2;
            end
        end
        if modes_2 == 0
            if RF.N2_eff >= RF.N2
                N2 = floor(1.2*N2);
            elseif RF.N2_eff < RF.N2-10
                N2 = RF.N2_eff+5;
                loop(2) = 1;
            end
        end
    end
else
    % find the usefull modes
    RF.N1_eff = find(abs(RF.b(2:end)/max(abs(RF.b(2:end)))) > RF.eps_N1, 1, 'last')+1;
    RF.N2_eff = find(abs(RF.a(2:end)/max(abs(RF.a))) > RF.eps_N1, 1, 'last')+1;
    % find the last values
    N1 = RF.N1;
    N2 = RF.N2;
    % decide what to do
    if strcmp(exit_type, 'divergence')
        modes = 1; % quit the while loop (last solution was not converged)
        'divergence'
    elseif RF.N1_eff < 0.4*RF.N1
        % mode numbers are too large
        modes = -1;
        N1 = RF.N1 - 5;
        'decrease modes'
    elseif RF.N1_eff < RF.N1
        % mode numbers are correct
        modes = 1; % quit the while loop
        'modes ok'
    else % increase the number of modes
        modes = -2;
        N1 = RF.N1 + 5;
        'increase modes'
    end
    if modes < 1
        N2 = floor(N1 * (1.5 + max(abs(RF.slope)) / 0.3*1.5));
    end
    RF.N1_eff = find(abs(RF.b(2:end)/max(abs(RF.b(2:end)))) <= RF.eps_N1, 1, 'first');
    RF.N2_eff = find(abs(RF.a(2:end)/max(abs(RF.a))) <= RF.eps_N1, 1, 'first');
    if isempty(RF.N1_eff), RF.N1_eff=NaN; end
    if isempty(RF.N2_eff), RF.N2_eff=NaN; end
    RF.N1_eff
    RF.N2_eff
end
