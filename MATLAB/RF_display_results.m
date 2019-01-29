function RF_display_results(RF)
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
N2 = RF.N2;
N1 = RF.N1;
eta = RF.eta;
a   = RF.a;
b   = RF.b;
slope = RF.slope;
%
figure(1), clf
[ax,h1,h2] = plotyy((0:2*N2)/N2*pi, [eta(1:N2+1), fliplr(eta(1:N2))], ...
    (0:2*N2)/N2*pi, [slope(1:N2+1), -fliplr(slope(1:N2))]);
set(h1, 'LineWidth', 1.5)
set(h2, 'LineWidth', 1.5)
axes(ax(1)), grid on, xlim([0,2*N2+1]/N2*pi), xlabel('kx'),ylabel('\eta')
y_lim = get(gca, 'YLim');
N_y = 4;
y_tick = linspace(y_lim(1),y_lim(2),2*N_y+1);
set(gca, 'YTick', y_tick)
axes(ax(2)), xlim([0,2*N2+1]/N2*pi), xlabel('kx'),ylabel('slope')
y_lim = get(gca, 'YLim');
N_y = 4;
y_tick = linspace(y_lim(1),y_lim(2),2*N_y+1);
set(gca, 'YTick', y_tick)
if RF.lorT == 1
    file_name = sprintf('L%g_H%g_h%g.eps', RF.lambda, RF.H, RF.h);
else
    file_name = sprintf('T%g_H%g_h%g.eps', RF.T, RF.H, RF.h);
end
set(gcf, 'FileName', ['D:\Félicien\Programs\docs\Rienecker_Fenton\Figures\RF_space_' file_name])
%
figure(2), clf
plot(0:N1, abs(b(1:N1+1))/max(abs(b(1:N1+1))), 0:N2, abs(a(1:N2+1))/max(abs(a(1:N2+1))), 'LineWidth', 1.5)
xlim([0,N2])
ylim([1e-20,1])
hold on
plot(floor(N1*[0.5,1]), RF.eps_N1*[1,1], 'b--')
plot(floor(N2*[0.5,1]), RF.eps_N1*[1,1], '--', 'Color', [0,0.5,0])
plot([0,N2], RF.eps_inc*[1,1], 'k--')
hold off
grid on
set(gca, 'YScale', 'log')
xlabel('Modes'),ylabel('Amplitude')
legend('Potential', 'Elevation')
set(gcf, 'FileName', ['D:\Félicien\Programs\docs\Rienecker_Fenton\Figures\RF_modes_' file_name])
% figure(3), clf
% plot(0:N1, b, 0:N2, a, 'LineWidth', 1.5)
% xlim([0,N2])
% grid on
end
