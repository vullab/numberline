clear all
close all

%% plot data and bilinear model fit.
figure();

% this draws the log grid.
for j = [1:10, 20:10:100 200 300]
    plot([0 2.5], [log10(j) log10(j)], 'k-', 'Color', [0.5 0.5 0.5]);
    hold on;
    plot([log10(j) log10(j)], [0 2.5], 'k-', 'Color', [0.5 0.5 0.5]);
end
for er = [norminv([0.51:0.01:0.99], 0, 0.1)]
fill([0 2.5 2.5, 0], [0-er 2.5-er 2.5+er, 0+er], [0 0 1], 'FaceAlpha', 0.05, 'EdgeColor', 'none')
end
plot([0 2.5], [0 2.5], 'b-', 'LineWidth', 3);

xlim([0 2.5]); ylim([0 2.5]); 
axis square
%
set(gca, 'XTick', [], 'YTick', [])

%%

clf;

% this draws the log grid.
for j = [1:10, 20:10:100 200 300]
    plot([0 2.5], [log10(j) log10(j)], 'k-', 'Color', [0.5 0.5 0.5]);
    hold on;
    plot([log10(j) log10(j)], [0 2.5], 'k-', 'Color', [0.5 0.5 0.5]);
end

plot([0 2.5], [0 2.5], 'k-')
%
map = @(x,slope,crit)((x>crit).*(crit+(x-crit).*slope)+(x<=crit).*x);

X = [0:0.01:2.5];
S = 0.8;
s = 1.0;
c = 0.8;
plot(X, map(X, S, c), 'g-', 'LineWidth', 3, 'Color', [0 0.6 0])
% for i = 1:10
%     s = s*0.9+randn(1)*0.05 + 0.1*S;
%     plot(X, map(X, s, c), 'g-', 'LineWidth', 1.5, 'Color', [0 1-i/10 0]);
% end

xlim([0 2.5]); ylim([0 2.5]); 
axis square
%
set(gca, 'XTick', [], 'YTick', [])