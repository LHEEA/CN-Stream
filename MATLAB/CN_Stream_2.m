function RF = CN_Stream_2(h,H,type,T,N1,N2,n_H)
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
display = 1;
if nargin < 1
    RF_i = RF_initialize;
else
    RF_i = RF_initialize(h,H,type,T,N1,N2,0);
end
%
if 0
    Ts = 10; %2:2:20; % s
    if RF_i.h == Inf
        k = (2*pi./Ts).^2/RF_i.g;
    else
        k = wave_number(1./Ts, RF_i.h);
    end
    k_NL = k / 1.2;

    if RF_i.h == Inf % deep water
        Hs = min(1000, 2 * 0.4./k_NL);
    else
        Hs = min(15, min(2 * 0.39./k_NL, 0.78 * RF_i.h));
    end
else
    Ts = RF_i.T;
    Hs = RF_i.H;
end
figure(6),clf,for n=1:4, subplot(4,1,n), hold all, grid on, end
figure(7), clf, hold all
figure(8), clf, hold all
figure(9), clf, hold all
for T=Ts
    RF = RF_i;
    RF.T = T;
    RF.H = Hs(T==Ts);
    if RF_check(RF), exit, end
    if nargin < 6
        [RF, result] = RF_solve_auto(RF, display);
    elseif nargin < 7
        [RF, result] = RF_solve_auto(RF, display, N2);
    else
        [RF, result] = RF_solve_auto(RF, display, N2, n_H);
    end
    %
    RF.c_E = RF.c + RF.b(1);
    RF.c_S = RF.c_E - RF.Q/RF.h;

    %% Displays
    RF_display_results(RF)
    Hprime = RF.H/RF.g/RF.T^2
    hprime = RF.h/RF.g/RF.T^2
    % kh = RF.k*RF.h
    % ka = max(abs(RF.slope))
    % c_S = c_S
    % c_E = c_E
    struct2ini('CN_Stream_last.ini',RF.input);
    %
    figure(6)
    H_prime = result.H/9.81/T^2;
    subplot(4,1,1)
    plot(result.slope, result.N1)
    subplot(4,1,2)
    % plot(result.slope, result.N2 ./ result.N1)
    plot(result.slope, result.N2)
    subplot(4,1,3)
    plot(H_prime, result.k .* result.H/2)
    subplot(4,1,4)
    plot(result.slope, result.k(1)./result.k)
    figure(7)
    plot((0:RF.N2)/RF.N2, RF.eta/RF.H)
    figure(8)
    plot(H_prime, result.k .* result.H/2, H_prime, result.slope)
    figure(9)
    plot(result.slope, result.k(1)./result.k)
end
%
figure(8), grid on
xlabel('H''=H/gT^2')
ylabel('Steepness')
legend('kH/2', 'max|\partial \eta/\partial x|', 'Location','NorthWest')
set(gcf, 'FileName', 'D:\Félicien\Programs\docs\Rienecker_Fenton\Figures\RF_steepness.eps')
figure(9), grid on
x_lim = get(gca, 'Xlim');
y_lim = get(gca, 'Ylim');
eps = 0:0.05:0.5;
if RF.h == Inf
    plot(eps, 1 + eps.^2, 'k--')
else
    sigma = tanh(RF.k * RF.h);
    plot(eps, 1 + eps.^2*(9-10*sigma.^2+9*sigma.^4)./(4*sigma.^4), 'k--')
end

legend('Nonlinear', 'Third-order', 'Location','NorthWest')
xlim(x_lim)
ylim(y_lim)
xlabel('max|\partial \eta/\partial x|')
ylabel('\lambda_N_L/\lambda_L')
set(gcf, 'FileName', 'D:\Félicien\Programs\docs\Rienecker_Fenton\Figures\RF_k_NL.eps')
