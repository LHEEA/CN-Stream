function RF = RF_calc_a(RF)
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
% Computation of Fourier coefficients of free surface elevation
M = 0:RF.N2;
a = zeros(1,RF.N2+1);
for I=1:RF.N2+1
    a(I) = trapz(RF.eta(1:RF.N2+1) .* cos((I-1)*M*pi/RF.N2));
end
RF.a = 2 * a / RF.N2;
%
% % Computation of Fourier coefficients of free surface elevation
% for I=1:RF.N2+1
%     a2(I) = RF.eta(1) + 2*sum(RF.eta(2:RF.N2).*cos((I-1)*(1:RF.N2-1)*pi/RF.N2)) + RF.eta(RF.N2+1)*cos((I-1)*pi);
%     a2(I) = a2(I) / RF.N2;
% end
%
