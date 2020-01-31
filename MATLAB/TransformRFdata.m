function RW = TransformRFdata(RF)
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


RW.H = RF.H;
RW.lambda=2*pi/RF.k;

RW.Hoverlambda = RW.H/RW.lambda;

RW.h=RF.h;
RW.k=RF.k;
RW.lambda=2*pi/RW.k;
RW.c =RF.c;
RW.g = RF.g;
RW.eta=RF.eta;
RW.slope=RF.slope;
RW.a=RF.a;
RW.b=RF.b;


