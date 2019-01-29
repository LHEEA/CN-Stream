function add_horizontal(data, style)
%
if nargin > 1
    s = style;
else
    s = 'k--';
end
% ajoute des traits horizontaux
x_seg = get(gca, 'XLim');
hold on
for n=1:length(data)
    plot(x_seg, data(n) * [1,1], s)
end
hold off
