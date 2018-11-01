clear all
close all

x = [-25:25];
cv = wishrnd(eye(length(x)), length(x));
imagesc(cv)
colorbar;

K = @(x1,x2)(exp(-0.5./s0.^2.*(x1-x2).^2);
%%
w = mvnrnd(eye(length(x)).*0, cv);
y = x*w;
plot(x,y, 'k.')


% s0 = 1;
% sn = 0.5;
% b = 0.05;
% nx = 100;
% x = [1:nx]';
% yS = randn(size(x)).*s0;
% % y = cumsum(y);
% yT = filter(normpdf([-3:0.25:3], 0, 0.5), 1, yS)+x.*b-0.5*nx*b;
% 
% x = [x;x];
% y = [yT+randn(size(yT)).*sn; yT+randn(size(yT)).*sn];
% plot(x, y, 'k.')

% K = @(x1,x2)(normpdf(x1-x2,0,%

%%

