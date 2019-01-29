function tests_CN_Stream_routines
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
h = 1.04278318713810; % depth in m
H = 0.399733555069604; % wave height in m
type = 0; % period is given in s
T = 4.67481393541603; % period in s
N1 = 40; % number of modes for phi
%
display = 1;
%
RF = CN_Stream(h,H,type,T,N1, display);
%
RF_write_waverfcof(RF);
