%% model based on constant linear regression with noise.
% ln(n) = a*ln(m)+b
close all;

n = 1000;
sm = 0.075;
a = 0.75;
b = 0.2;


stims = round(10.^(rand(n,1).*log10(1000)));

    ms = randn([n,1]).*sm+log10(stims);
    ns = round(10.^(ms.*a+b));
loglog([1, 1000], [1, 1000], 'k-', 'LineWidth', 2);
hold on
loglog(stims+rand(size(stims)).*0.1+0.05, ns+rand(size(stims)).*0.1+0.05, 'r.', 'MarkerSize', 6);
loglog([1, 1000], 10.^(log10([1,1000]).*a+b), 'g-', 'LineWidth', 2, 'Color', [0 0.6 0])

%% constant bilinear regression plus noise.
% k = ak+b
% k-ak = b
% k(1-a) = b
% k = b/(1-a)

close all

n = 1000;
sm = 0.075;
a = 0.75;
k = log10(15);

fxm = @(m)((m<=k).*m + (m>k).*(m.*a+k-a*k));

stims = round(10.^(rand(n,1).*log10(1000)));
ms = randn([n,1]).*sm+log10(stims);
ns = round(10.^fxm(ms));

loglog([1, 1000], [1, 1000], 'k-', 'LineWidth', 2);
hold on
loglog(stims+rand(size(stims)).*0.1+0.05, ns+rand(size(stims)).*0.1+0.05, 'r.', 'MarkerSize', 6);
loglog([1:1000], 10.^(fxm(log10([1:1000]))), 'g-', 'LineWidth', 2, 'Color', [0 0.6 0])


%% variable slope bilinear regression plus noise.
% k = ak+b
% k-ak = b
% k(1-a) = b
% k = b/(1-a)

close all

n = 1000;
sm = 0.01;
a = 0.75;
k = log10(15);
sa = 0.075;

fxm = @(m,a,k)((m<=k).*m + (m>k).*(m.*a+k-a*k));

stims = round(10.^(rand(n,1).*log10(1000)));
as = randn(n,1).*sa+a;
ms = randn([n,1]).*sm+log10(stims);
ns = round(10.^fxm(ms, as, k));

loglog([1, 1000], [1, 1000], 'k-', 'LineWidth', 2);
hold on
loglog(stims+rand(size(stims)).*0.1+0.05, ns+rand(size(stims)).*0.1+0.05, 'r.', 'MarkerSize', 6);
loglog([1:1000], 10.^(fxm(log10([1:1000]), a, k)), 'g-', 'LineWidth', 2, 'Color', [0 0.6 0])


%%
% 

sd = 0.1;
md = 0.1;

x = [0:0.01:3];
y = [0:0.01:3];
s = ones(size(x)).*0.1;
% s = [ones(1,10).*0.01, ones(1,21).*0.5] ; % later change to vary;
% y(20) = 1.5;
% s(20) = 0.01;

yh = [0:0.01:3];

for i = [1:25]
%         idx = ceil(rand()*(length(x)-1)+1);
for idx = [2:length(x)]
        clf;
        plot([0 3], [0 3], 'k-', 'LineWidth', 2);
        hold on;
        plot(x, yh, 'b.-');
        drawnow();
        my = y(idx);
        vy = s(idx).^2;
        useidx = [max(1,idx-10):min(length(x), idx+10)];
        ws = normpdf(useidx, 0, 5);
        xs = x(useidx);
        ys = yh(useidx);
        [bs] = lscov([ones(length(xs),1) xs'], ys', ws');
        ml = bs(1)+bs(2).*x(idx);
        vl = var(diff(yh));
        mn = (my./vy+ml./vl)./(1./vy+1./vl);
        sn = sqrt(vy*vl/(vy+vl));
        yh(idx) = randn().*sn+mn;
end
end

plot(x, yh);



