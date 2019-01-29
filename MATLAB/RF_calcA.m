function A = RF_calcA(U,V,S1,C1,S2,C2,RF)
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
% RF_calcA estimates the Jacobian matrix in the Rienecker and
% Fenton computation
% A = RF_calcA(U,V,S1,C1,S2,C2,RF)
%
% Description
% The RF_calcA function is part of Riencker and Fenton solution package.
% it is based on paper by M.M. Rienecker and J.D. Fenton,
% A Fourier approximation method for steady water waves.
% J. Fluid Mech., Vol. 104, pp. 119-137, 1981.
%
% implemented by Pierre FERRANT and modified by Felicien BONNEFOY - LHEEA - 2015
%
% See also RF_solve, RF_calcuvcs, RF_calcer.
%
g = RF.g;
B = RF.b;
Y = RF.eta;
k = RF.k;
c = RF.c;
T = RF.T;
N2 = RF.N2;
N1 = RF.N1;
depth = RF.h;
%
A = zeros(N2+N2+6,N1+N2+6);
%
% KFSBC
for I = 1:N2+1
    M=I;
    A(I,I)    = U(M);
    A(I,N2+2) = Y(M);
    for J=1:N1
        A(I,N2+2+J) = C1(J+1,M);
    end
    A(I,N1+N2+6) = 1;
end
if RF.lorT == 0 % k is unknown # N1+N2+5
    for I=1:N2+1 % for both deep water and finite depth
        M = I;
        A(I,N1+N2+5) = Y(M) / k * (U(M)-B(1));
    end
    if depth ~= Inf
        J  = 1:N1;
        for I=1:N2+1 % for finite depth only
            M = I;
            A(I,N1+N2+5) = A(I,N1+N2+5) ...
                - depth * sum(J.*B(J+1).*C1(J+1,M).'.*tanh(J*k*depth));
        end
    end
end
% DFSBC
J  = 1:N1;
for I = 1:N2+1
    M  = I;
    AA = sum(J.^2.*B(J+1).*C1(J+1,M).');
    BB = sum(J.^2.*B(J+1).*S1(J+1,M).');
    A(N2+1+I,I)   = g + k^2 * (U(M)*AA+V(M)*BB);
    A(N2+1+I,N2+2) = U(M);
    A(N2+1+I, N2+2+J) = J.*k.*(U(M)*C2(J+1,M).'+V(M)*S2(J+1,M).');
    A(N2+1+I,N1+N2+3) = -1;
end
if RF.lorT == 0 % k is unknown # N1+N2+5
    for I=1:N2+1 % for both deep water and finite depth
        M  = I;
        A(N2+1+I,N1+N2+5) = (U(M) * (U(M)-B(1)) + V(M)^2) / k;
    end
    if depth ~= Inf
        for I=1:N2+1; % for finite depth only
            M  = I;
            A(N2+1+I,N1+N2+5) = A(N2+1+I,N1+N2+5) + ...
                k*U(M) * (Y(M)*sum(J.^2.*B(J+1).*C1(J+1,M).') - ...
                depth*sum(J.^2.*B(J+1).*C2(J+1,M).'.*tanh(J*k*depth))) + ...
                k*V(M) * (Y(M)*sum(J.^2.*B(J+1).*S1(J+1,M).') - ...
                depth*sum(J.^2.*B(J+1).*S2(J+1,M).'.*tanh(J*k*depth)));
        end
    end
end
% Volume
A(N2+N2+3,1)    = 1;
A(N2+N2+3,2:N2) = 2;
A(N2+N2+3,N2+1) = 1;
% Wave height
A(N2+N2+4,1)    =  1;
A(N2+N2+4,N2+1) = -1;
% Current
if RF.current % mass transport velocity
    A(N2+N2+5,N1+N2+6) = -1/depth;
    A(N2+N2+5,N2+2)    = 1;
else % eulerian current
    A(N2+N2+5,N2+2)    = 1;
end
A(N2+N2+5,N1+N2+4) = 1;
% wavelength or period
if RF.lorT == 1 % equation for T is required
    A(N2+N2+6,N1+N2+4) = k*T;
    A(N2+N2+6,N1+N2+5) = k*c;
else % equation for k is required
    A(N2+N2+6,N1+N2+4) = k*T;
    A(N2+N2+6,N1+N2+5) = c*T;
end
