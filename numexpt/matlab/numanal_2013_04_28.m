clear all
close all

fpath = 'data/';
files = dir(sprintf('%s*.csv',fpath));

nsubs = 0;

ause = 4;  % a1
% ause = 5;  % a2
% ause = 9;  % geo mean

%% file headers
% 1 run
% 2 index
% 3 num_dots
% 4 answer1
% 5 answer2
% 6 points1
% 7 points2
% 8 time

%% load files into large cell structure.
nsubs = 0;
for fi = [1:length(files)]
    curfile = fopen(sprintf('%s%s', fpath, files(fi).name));
    discard = fgetl(curfile);
    q = cell2mat(textscan(curfile,'%f %f %f %f %f %f %f %f ', 'delimiter', ','));
    fclose(curfile);
    nsubs = nsubs+1;
    z{nsubs} = double(q);
    z{nsubs}(:,9) = 10.^(mean([log10(z{nsubs}(:,4)+1), log10(z{nsubs}(:,5)+1)],2))-1;
end

%% bilinear model
% parameters: slope, crit, err.
fxpred = @(x,slope,crit)((x>crit).*(crit+(x-crit).*slope)+(x<=crit).*x);
lik1 = @(y,x,slope,crit)(sum(min(0.5,(y-fxpred(x,slope,crit)).^2)));
lik = @(y,x,slope,crit)(lik1(y,x,slope,crit) + 2.*(slope-1)^2+3.*(crit-1)^2);
for i = [1:length(z)]
    fmfx = @(params)(lik(log10(z{i}(:,ause)), log10(z{i}(:,3)), params(1), params(2)));
    params = fminsearch(fmfx, [1 1]);
    ss(i) = params(1);
    sc(i) = max(0, params(2));
    biline_lik(i) = fmfx([ss(i), sc(i)]);
    biline_r(i) = 1-mean((fxpred(log10(z{i}(:,3)), ss(i), sc(i))-log10(z{i}(:,ause))).^2)./var(log10(z{i}(:,ause)));
end

%% just a line
fxpred = @(x,slope,crit)(x.*slope+crit);
lik1 = @(y,x,slope,crit)(sum(min(0.5,(y-fxpred(x,slope,crit)).^2)));
lik = @(y,x,slope,crit)(lik1(y,x,slope,crit) + 0.1.*(slope-1)^2+0.1.*(crit-0)^2);
for i = [1:length(z)]
    fmfx = @(params)(lik(log10(z{i}(:,ause)), log10(z{i}(:,3)), params(1), params(2)));
    params = fminsearch(fmfx, [1 1]);
    sb1(i) = params(1);
    sb0(i) = max(0, params(2));
    line_lik(i) = fmfx([sb1(i), sb0(i)]);
    line_r(i) = 1-mean((fxpred(log10(z{i}(:,3)), sb1(i), sb0(i))-log10(z{i}(:,ause))).^2)./var(log10(z{i}(:,ause)));
    phys_r(i) = 1-mean((log10(z{i}(:,3))-log10(z{i}(:,ause))).^2)./var(log10(z{i}(:,ause)));
end

%% compare r values.
[sum(line_r<biline_r) length(biline_r)]
[mean(line_r) mean(biline_r)]

%% compare (visually) linear and bilinear fits.

close all;
figure()
for i = [1:length(z)];
    idxuse = 1:300;
    subplot(6,4,i);
% figure();
    for j = [1:10, 20:10:100 200 300]
        plot([0 2.5], [log10(j) log10(j)], 'k-', 'Color', [0.5 0.5 0.5]);
        hold on;
        plot([log10(j) log10(j)], [0 2.5], 'k-', 'Color', [0.5 0.5 0.5]);
    end
    plot([0 2.5], [0 2.5], 'k-', 'LineWidth', 2);
    plot(min(2.5, log10(z{i}(idxuse,3)+rand(length(idxuse),1).*0.5-0.25)), min(2.5,log10(z{i}(idxuse,ause)+rand(length(idxuse),1).*0.5-0.25)),'r.');
    hold on;
    x = [0:0.01:2.5];
    plot([0 2.5], [0 2.5].*sb1(i)+sb0(i), 'b-', 'LineWidth', 2);
    s = ss(i);
    c = sc(i);
    plot(x, (x>c).*(c + (x-c).*s)+(x<=c).*x, 'g-', 'Color', [0 0.5 0], 'LineWidth', 2);
    xlim([0 2.5]); ylim([0 2.5]);
    axis square
end

%% compare split nths.

fxpred = @(x,slope,crit)((x>crit).*(crit+(x-crit).*slope)+(x<=crit).*x);
lik1 = @(y,x,slope,crit)(sum(min(0.5,(y-fxpred(x,slope,crit)).^2)));
lik = @(y,x,slope,crit)(lik1(y,x,slope,crit) + 2.*(slope-1)^2+3.*(crit-1)^2);

nfs = [2, 30];
for q = [1:length(nfs)];
    clear ss5 sc5 ss5c sc5c;
    nf = nfs(q);
    for f = [1:nf]
        idx = ((f-1)*300/nf)+[1:300/nf];
        idxc = f:nf:300;
        for i = [1:length(z)]
            % this is split sequentially
            fmfx = @(params)(lik(log10(z{i}(idx,ause)), log10(z{i}(idx,3)), params(1), params(2)));
            params = fminsearch(fmfx, [1 1]);
            ss5{f}(i) = params(1);
            sc5{f}(i) = params(2);
            % this is split by modulus
            fmfx = @(params)(lik(log10(z{i}(idxc,4)), log10(z{i}(idxc,3)), params(1), params(2)));
            params = fminsearch(fmfx, [1 1]);
            ss5c{f}(i) = params(1);
            sc5c{f}(i) = params(2);
        end
    end
    
    for f1 = [1:nf]
        for f2 = [1:nf]
            css{q}(f1,f2) = corr(ss5{f1}', ss5{f2}');
            csc{q}(f1,f2) = corr(sc5{f1}', sc5{f2}');
            
            cssc{q}(f1,f2) = corr(ss5c{f1}', ss5c{f2}');
            cscc{q}(f1,f2) = corr(sc5c{f1}', sc5c{f2}');
        end
    end
    for f = [1:nf-1]
        mcs{q}(f) = mean(diag(css{q}, f));
        mcc{q}(f) = mean(diag(csc{q}, f));
        scs{q}(f) = std(diag(css{q}, f))./sqrt(length(diag(css{q},f)));
        scc{q}(f) = std(diag(csc{q}, f))./sqrt(length(diag(css{q},f)));
        
        mcsc{q}(f) = mean(diag(cssc{q}, f));
        mccc{q}(f) = mean(diag(cscc{q}, f));
        scsc{q}(f) = std(diag(cssc{q}, f))./sqrt(length(diag(css{q},f)));
        sccc{q}(f) = std(diag(cscc{q}, f))./sqrt(length(diag(css{q},f)));
    end
end
%% first: show correlation matrices
figure();
imagesc(cssc{1}, [-1, 1]), colormap gray

%% plot of correlation over distance, unnormalized, slope.
usenfs = [1:length(nfs)];
% unnormalized
figure();
for q = [usenfs];
    nf = nfs(q);
    errorbar([1:nf-1]./(nf), mcs{q}, scs{q}, 'r.-');
    hold on;
    errorbar([1:nf-1]./(nf), mcsc{q}, scsc{q}, 'k.-');
end
ylim([0.4, 1])
xlim([0 1]);

% normalized
figure();
for q = [usenfs];
    nf = nfs(q);
    errorbar([1:nf-1]./(nf), mcs{q}./mean(mcsc{q}), scs{q}./mean(mcsc{q}), 'r.-');
    hold on;
    errorbar([1:nf-1]./(nf), mcsc{q}./mean(mcsc{q}), scsc{q}./mean(mcsc{q}), 'k.-');
end
ylim([0.4, 1.05])
xlim([0 1]);

%% random walk calculations
clear deltaS vk
useblocks = [1:30];
S = cell2mat(ss5');
S = S(useblocks,:);
v1 = mean(diff(S).^2);
v1se = std(diff(S).^2)./sqrt(length(useblocks)-1);
for i = [1:length(z)]
    [s1, s2] = ndgrid(S(:,i), S(:,i));
    deltaS(:,:,i) = s1-s2;
    for fi = [1:(length(useblocks)-1)]
        vk(fi,i) = mean(diag(deltaS(:,:,i).^2,fi));
    end
end
%% plot var_k / (var_1 * k)
fxf = @(x)(log10(x)); % (log10(x))
fxb = @(x)(10.^x); % (10.^x)

figure();
varnorm = vk./(repmat(v1, [length(useblocks)-1 1]).*repmat([1:(length(useblocks)-1)]', [1 nsubs]));
for i = [1:nsubs]
    plot([1:(length(useblocks)-1)], fxb(fxf(varnorm(:,i))), 'k-', 'Color', [0.5 0.5 0.5], 'LineWidth', 1);
    hold on;
end
mlv = mean(fxf(varnorm),2);
selv = std(fxf(varnorm),[],2)./sqrt(nsubs);
errorbar([1:(length(useblocks)-1)], fxb(mlv), fxb(mlv+selv)-fxb(mlv), fxb(mlv-selv)-fxb(mlv), 'k.-', 'MarkerSize', 50, 'LineWidth', 4)
xlabel('k','FontSize',16)
ylabel('var_k / (var_1 * k)','FontSize',16);
ylim([0 1])

%% sample variance models
figure();
plot([1:(length(useblocks)-1)], fxb(fxf(ones(29,1))), 'b.-', 'MarkerSize', 30, 'LineWidth', 3, 'Color', [0 0 0.8])
hold on;
z = 0.5; p1 = 0.75;
z2 = 0.97; p2 = 0.25;
plot([1:(length(useblocks)-1)], fxb(fxf(z.^[0:28])), 'r.-', 'MarkerSize', 30, 'LineWidth', 3, 'Color', [0.6 0 0])
plot([1:(length(useblocks)-1)], fxb(fxf(p1.*z.^[0:28] + p2.*z2.^[0:28])), 'g.-', 'MarkerSize', 30, 'LineWidth', 3, 'Color', [0 0.6 0])
xlabel('k','FontSize',16)
ylabel('var_k / (var_1 * k)','FontSize',16);
% ylim([0 1])

%% fit model
fx = @(p)(log10(p(1).*p(2).^[0:28]+(1-p(1)).*p(3).^[0:28]));

p = fminsearch(@(p)(nansum((fx(p)-mlv').^2./selv'.^2)), [0.5 0.25 0.75])

%% try fitting decaying exponentials to normalized variance.
plot(varnorm)
hold on
for i = [1:size(varnorm,2)]
    d = varnorm(:,i);
    fx = @(p)((exp(-[0:28]./exp(p(1)))).*p(2)+(1-p(2)));
    ps(i,:) = fminsearch(@(p)((p(2)-0.5).^2+sum((fx(p)-d').^2.*sqrt(fliplr(1:29)))), [0, 1]);
    plot(fx(ps(i,:)), 'k:')
end
% 95% confidence interval on decayed random walk
mean(ps(:,2))+[-2 0 2].*std(ps(:,2))./sqrt(length(ps(:,2)))
% 95% confidence interval on time constant (in 10 trial units)
exp(mean(ps(:,1))+[-2 0 2]*std(ps(:,1))./sqrt(length(ps(:,1))))
