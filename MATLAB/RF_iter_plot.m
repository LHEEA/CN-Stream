function RF_iter_plot(RF, n_H, iter)
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
figure(3),clf
if RF.lorT == 1
    label_y = char('R', 'b(1)', 'c', 'T', 'Q');
else
    label_y = char('R', 'b(1)', 'c', 'lambda', 'Q');
end
for n=1:5
    subplot(5,1,n),hold all
    for i_H=1:n_H
        plot(0:iter.N_it(i_H)-1, squeeze(iter.result(i_H,1:iter.N_it(i_H),n)))
    end
    ylabel(label_y(n,:))
end
xlabel('Iterations')
drawnow
figure(4),clf
label_y = char('b(2)', 'a(2)', 'b(N1+1)', 'a(N2+1)');
for n=1:4
    subplot(4,1,n),hold all
    for i_H=1:n_H
        plot(0:iter.N_it(i_H)-1, squeeze(iter.result(i_H,1:iter.N_it(i_H),n+5)))
    end
    ylabel(label_y(n,:))
end
xlabel('Iterations')
drawnow
figure(5),clf
label_y = char('||F||', '||DY||');
for n=1:2
    ax = subplot(2,1,n); hold all
    for i_H=1:n_H
        plot(0:iter.N_it(i_H)-1, squeeze(iter.result(i_H,1:iter.N_it(i_H),n+9)))
    end
    ylabel(label_y(n,:))
    set(ax, 'YScale', 'log');
end
xlabel('Iterations')
drawnow
end
