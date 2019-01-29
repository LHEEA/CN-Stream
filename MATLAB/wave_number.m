function k_wn = wave_number(freq, depth)
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
% WAVE_NUMBER gives the wave number at given frequency 'freq' and depth
% through first order dispersion relation
%
%   k = WAVE_NUMBER(FREQ) calculates the wave number assuming
%   dimensionless frequency
%   k = WAVE_NUMBER(FREQ, DEPTH) uses the given depth and dimensionful
%   frequency
%

% Initialisation of variables
approx = 1;
if nargin == 1
    gravity = 1;
    depth = 1;
elseif depth == -1
    gravity = 1;
    depth = 1;
    approx = 0;
else
    gravity = 9.81;
end
frequency  = freq .* sqrt(depth / gravity);
%
n_row = size(freq,1);
n_col = size(freq,2);
warning off MATLAB:fzero:UndeterminedSyntax
for n = 1:n_row
    for m = 1:n_col
        rhs = (2 .* pi .* frequency(n,m)).^2;
        if approx == 1
            order       = choose_order(frequency(n,m));
            result(n,m) = wnb_approx(rhs,order);
        else
            result(n,m) = abs(fzero(@disp_relation, rhs,[],rhs));
        end
        % abs value for low frequency values
    end
end
warning on MATLAB:fzero:UndeterminedSyntax
if nargout < 1
    result ./ depth
else
    k_wn = result ./ depth;
end
%
%
%
function y = disp_relation(k, rhs)
%DISP_RELATION gives the dimensionless dipersion relation at first order :  k tanh (k) - w^2
%
y = k .* tanh(k) - rhs;
%
function order = choose_order(freq)
if freq > 0.62
    order = 0;
elseif freq > 0.4
    order = 1;
elseif freq > 0.33
    order = 2;
else
    order = -1;
end
%
function k = wnb_approx(w2,order)
%
if order == 0
    k = w2;
elseif order == 1
    k     = w2;
    sigma = tanh(k);
    k     = k * (1 + (1 - sigma) / (sigma + k*(1-sigma^2)));
elseif order == 2
    k      = w2;
    sigma  = tanh(k);
    sigma2 = k*(1 - sigma^2);
    k      = k * (1 + (- sigma - sigma2 + ...
                    sqrt((sigma + sigma2)^2 -4*(sigma-1)*sigma2*(1- k*sigma))) / ...
                    (2*sigma2*(1- k*sigma)));
else
    k = abs(fzero(@disp_relation, w2,[],w2));
end
