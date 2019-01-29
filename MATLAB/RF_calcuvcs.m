function [U,V,S1,C1,S2,C2] = RF_calcuvcs(RF)
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
% RF_calcuvcs estimates the exponential terms required for Rienecker and
% Fenton computation
% [U,V,S1,C1,S2,C2] = RF_calcuvcs(RF)
%
% Description
% The RF_calcuvcs function is part of Riencker and Fenton solution package.
% it is based on paper by M.M. Rienecker and J.D. Fenton,
% A Fourier approximation method for steady water waves.
% J. Fluid Mech., Vol. 104, pp. 119-137, 1981.
%
% implemented by Pierre FERRANT and modified by Felicien BONNEFOY - LHEEA - 2015
%
% See also RF_solve, RF_calcA, RF_calcer.
%
B = RF.b;
Y = RF.eta;
k   = RF.k;
N2 = RF.N2;
N1 = RF.N1;
depth = RF.h;
%
U = zeros(1, N2+1);
V = zeros(1, N2+1);
C1 = zeros(N1+1,N2+1);
if depth ~= Inf
    C1(1,1:N2+1) = 1;
end
C2 = zeros(N1+1,N2+1);
C2(1,1:N2+1) = 1;
S1 = zeros(N1+1,N2+1);
S2 = zeros(N1+1,N2+1);
if N1*k*max(Y) > 650
    warning('Exponential argument is high')
end
for M=1:N2+1
    J = 1:N1;
    if depth == Inf %	deep water case
        C2(J+1,M)=cos((M-1)*J*pi/N2) .* exp(J*k*Y(M));
        S2(J+1,M)=sin((M-1)*J*pi/N2) .* exp(J*k*Y(M));
        C1 = C2;
        S1 = S2;
    else % finite depth
        choverch = (exp(J*k*Y(M)) + exp(-J*k*(Y(M) + 2*depth))) ./ (1 + exp(-2*J*k*depth));
        shoverch = (exp(J*k*Y(M)) - exp(-J*k*(Y(M) + 2*depth))) ./ (1 + exp(-2*J*k*depth));
        C2(J+1,M)=cos((M-1)*J*pi/N2) .* choverch;
        C1(J+1,M)=cos((M-1)*J*pi/N2) .* shoverch;
        S1(J+1,M)=sin((M-1)*J*pi/N2) .* choverch;
        S2(J+1,M)=sin((M-1)*J*pi/N2) .* shoverch;
    end
    U(M) = sum(J.*B(J+1).*C2(J+1,M).') * k + B(1);
    V(M) = sum(J.*B(J+1).*S2(J+1,M).') * k;
end
