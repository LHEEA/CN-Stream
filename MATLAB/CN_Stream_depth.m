function CN_Stream_depth
%
display = 1;
RF_i = RF_initialize;
%
if 1
    RF_i.T  = 8; % s
    hprimes = 0.2 * 2.^(0:4)/40;
    hs = hprimes * RF_i.g * RF_i.T^2;
else
    hs = RF_i.h;
    Hs = RF_i.H;
end
figure(6),clf,for n=1:4, subplot(4,1,n), hold all, grid on, end
figure(7), clf, hold all
figure(8), clf, hold all
figure(9), clf, hold all
figure(10), clf, for n=1:2, subplot(1,2,n), hold all, grid on, end
figure(11), clf, hold all, grid on
for h=hs
    RF = RF_i;
    RF.h = h;
    k = wave_number(1./RF.T, RF.h);
    RF.H = 0.5/20 * RF_i.g * RF_i.T^2;
    RF.H = min([RF.H, 0.75*RF.h, 0.14*2*pi/k]);
    if RF_check(RF), exit, end
    [RF, result] = RF_solve_auto(RF, display);
    %
    T = RF.T;
    %% Displays
    RF_display_results(RF)
    RF;
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
    X = H_prime*2*pi^2;
    X = result.slope;
    subplot(4,1,1)
    plot(X, result.N1_eff)
    subplot(4,1,2)
    % plot(H_prime*2*pi^2, result.N2 ./ result.N1)
    plot(X, result.N2_eff)
    subplot(4,1,3)
    plot(X, result.slope)
    subplot(4,1,4)
    plot(X, result.k(1)./result.k)
    figure(7)
    plot((0:RF.N2)/RF.N2*pi, RF.eta/RF.H)
    figure(8)
    plot(result.slope, H_prime) % , result.k .* result.H/2, H_prime, result.slope)
    figure(9)
    plot(result.slope, result.k(1)./result.k)
    figure(10)
    subplot(1,2,1)
    plot(X, result.N1_eff)
    subplot(1,2,2)
    plot(X, result.N2_eff)
    figure(11)
    plot(X, result.N2_eff./result.N1_eff)

end
%
figure(6)
subplot(4,1,1),ylabel('N_1')
subplot(4,1,2),ylabel('N_2')
subplot(4,1,3),ylabel('max|\partial \eta/\partial x|')
subplot(4,1,4),ylabel('\lambda_N_L/\lambda_L')
figure(7)
xlabel('kx')
ylabel('\eta/H')

figure(8), grid on
ylabel('H''=H/gT^2')
xlabel('Steepness')
legend(num2str(hprimes.'))
% legend('kH/2', 'max|\partial \eta/\partial x|', 'Location','NorthWest')
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

figure(10), subplot(1,2,1), ylabel('N_1'), grid on
xlabel('max|\partial \eta/\partial x|')
subplot(1,2,2), ylabel('N_2'), grid on
xlabel('max|\partial \eta/\partial x|')
set(gcf, 'FileName', 'D:\Félicien\Programs\docs\Rienecker_Fenton\Figures\RF_N1_N2.eps')
figure(11)
ylabel('N_2/N_1'), grid on
xlabel('max|\partial \eta/\partial x|')
set(gcf, 'FileName', 'D:\Félicien\Programs\docs\Rienecker_Fenton\Figures\RF_N2oN1.eps')
