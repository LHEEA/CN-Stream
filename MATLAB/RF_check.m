function stop = RF_check(RF)
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
if RF.lorT == 1
    eps_max = 0.45;
else
    eps_max = 0.03;
end
%
if RF.h == Inf
    if RF.lorT == 1
        eps = 2*pi/RF.lambda * RF.H / 2;
    else
        eps = RF.H / RF.g / RF.T^2;
    end
else
    if RF.lorT == 1
        mu  = 2*pi/RF.lambda * RF.h;
        eps = 2*pi/RF.lambda * RF.H / 2;
        if mu < 1
            eps_max = 0.8 * 2*pi/RF.lambda * RF.h;
        end
    else
        mu  = RF.h / RF.g / RF.T^2;
        eps = RF.H / RF.g / RF.T^2;
        if mu < 0.05
            eps_max = 0.8 * RF.h / RF.g / RF.T^2;
        end
    end
end
stop = ~(eps < eps_max);
if isfield(RF, 'slope')
    stop = ~(eps < 0.55);
end
