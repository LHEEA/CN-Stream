function RF = RF_initialize(h, H, lambdaorT, value, N1, U, type)
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
% RF_initialize gives Rienecker and Fenton solution
% RF = RF_initialize(h, H, lambdaorT, value, N1, U, type)
%
% Description
% The RF_initialize function is part of Riencker and Fenton solution package.
% it is based on paper by M.M. Rienecker and J.D. Fenton,
% A Fourier approximation method for steady water waves.
% J. Fluid Mech., Vol. 104, pp. 119-137, 1981.
%
% the function runs without input (reads the CN_Stream.ini configuration file)
% or with all inputs
% h            dimensional depth
% H            dimensional wave height
% lambdaorT    input wavelength (=1) or period (=0)
% value        wavelength or period
% N1           number of modes for the stream function
% U            current velocity in m/s
% type         current type
%
% Output
% RF structure which contains 6 elements
%   RF.input    values read from CN_Stream.ini file
%   RF.g        gravity acceleration
%   RF.h        depth
%   RF.N1       number of modes for stream function
%   RF.H        wave height
%   RF.lorT     input wavelength (=1) or period (=0)
%   RF.lambda or RF.T wavelength or period
%
% implemented by Pierre FERRANT and modified by Felicien BONNEFOY - LHEEA - 2015
%
% See also RF_solve, RF_calcuvcs, RF_calcer, RF_calcA, RF_iterate.
%
if nargin > 0
    % dimensional input
    RF.input.General.Dimension = 1;
    RF.input.General.Depth     = h;
    RF.input.General.Modes     = N1;
    RF.input.Wave.Input        = lambdaorT;
    RF.input.Wave.Value        = value;
    RF.input.Wave.Height       = H;
    RF.input.Current.Value     = U;
    RF.input.Current.Type      = type;
else
    RF.input = ini2struct('CN_Stream.ini');
end
%
RF.h   = RF.input.General.Depth;
RF.dim = RF.input.General.Dimension;
RF.N1  = RF.input.General.Modes;
%
RF.lorT = RF.input.Wave.Input; % 1 for wavelength, 0 for period
%
if RF.dim == 0
    if RF.lorT == 1 % wavelength
        RF.lambda = 1;
    else % period
        RF.T = 1;
    end
else
    if RF.lorT == 1 % wavelength
        RF.lambda = RF.input.Wave.Value;
    else % period
        RF.T = RF.input.Wave.Value;
    end
end
%
RF.H = RF.input.Wave.Height;
RF.U = RF.input.Current.Value;
RF.current = RF.input.Current.Type;
if RF.h < 0
    RF.h = Inf;
end
if RF.dim == 0
    RF.g = 1;
else
    RF.g = 9.81;
end
