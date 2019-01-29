function iter = RF_iter_store(iter_in, i_H, RF)
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
iter = iter_in;
iter.result(i_H,iter.N_iter+1,1) = RF.R;
iter.result(i_H,iter.N_iter+1,2) = RF.b(1);
iter.result(i_H,iter.N_iter+1,3) = RF.c;
if RF.lorT == 1
    iter.result(i_H,iter.N_iter+1,4) = RF.T;
else
    iter.result(i_H,iter.N_iter+1,4) = 2*pi / RF.k;
end
iter.result(i_H,iter.N_iter+1,5) = RF.Q;
iter.result(i_H,iter.N_iter+1,6) = RF.b(2);
iter.result(i_H,iter.N_iter+1,7) = RF.a(2);
iter.result(i_H,iter.N_iter+1,8) = RF.b(5);%RF.N1+1);
iter.result(i_H,iter.N_iter+1,9) = RF.a(5);%RF.N2+1);
iter.result(i_H,iter.N_iter+1,10) = iter.F_max;
iter.result(i_H,iter.N_iter+1,11) = iter.DY_max;
end
