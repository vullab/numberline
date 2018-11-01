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
for fi = [1:length(files)]
    curfile = fopen(sprintf('%s%s', fpath, files(fi).name));
    discard = fgetl(curfile);
    q = cell2mat(textscan(curfile,'%f %f %f %f %f %f %f %f ', 'delimiter', ','));
    fclose(curfile);
    nsubs = nsubs+1;
    z{nsubs} = double(q);
    z{nsubs}(:,9) = 10.^(mean([log10(z{nsubs}(:,4)+1), log10(z{nsubs}(:,5)+1)],2))-1;
end

%% correlations between answers, and responses.  
for i = [1:nsubs]
    q = corrcoef(log10(z{i}(:,3)+1), log10(z{i}(:,4)+1));
    ca1(i) = q(1,2);
    q = corrcoef(log10(z{i}(:,3)+1), log10(z{i}(:,5)+1));
    ca2(i) = q(1,2);
    q = corrcoef(log10(z{i}(:,4)+1), log10(z{i}(:,5)+1));
    caa(i) = q(1,2);
end

% plot correlations.
bins = [0.025:0.05:0.975];
figure();
subplot(3,1,1);
hist(ca1, bins)
subplot(3,1,2);
hist(ca2, bins)
subplot(3,1,3);
hist(caa, bins);

%%
% simple bilinear model
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

%%

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


%% coefficient of variation increasing?

% hist(log10(z{i}(:,ause)), [0:0.25:2.5])

bins = log10([1  5    11    17    25    34    45    59    79   113 1000]);

for i = [1:length(z)]
    for b = [1:length(bins)-1]
        % first based only on answers.
        inbin = log10(z{i}(:,ause))>bins(b) & log10(z{i}(:,ause))<=bins(b+1);
        n1(b,i) = sum(inbin);
        logvar1(b,i) = std(log10(z{i}(inbin,ause)));
        linvar1(b,i) = std((z{i}(inbin,ause)));
        cov1a(b,i) = linvar1(b,i)/mean((z{i}(inbin,ause)));
        cov1p(b,i) = linvar1(b,i)/mean((z{i}(inbin,3)));
        % first based only on physical.
        n2(b,i) = sum(inbin);
        inbin = log10(z{i}(:,3))>bins(b) & log10(z{i}(:,3))<=bins(b+1);
        logvar2(b,i) = std(log10(z{i}(inbin,ause)));
        linvar2(b,i) = std((z{i}(inbin,ause)));
        cov2a(b,i) = linvar2(b,i)/mean((z{i}(inbin,ause)));
        cov2p(b,i) = linvar2(b,i)/mean((z{i}(inbin,3)));
    end
end

% correct for 0 bin s
logvar2(n2<2) = nan;
linvar2(n2<2) = nan;
cov2a(n2<2) = nan;
cov2p(n2<2) = nan;

logvar1(n1<2) = nan;
linvar1(n1<2) = nan;
cov1a(n1<2) = nan;
cov1p(n1<2) = nan;

%%
figure();
subplot(2,2,1);
plot(logvar2./repmat(logvar2(3,:), [length(bins)-1 1]))
subplot(2,2,2);

subplot(2,2,3);
plot(log10(cov2p./repmat(cov2a(3,:), [length(bins)-1 1])))
subplot(2,2,4);

%% slopes on CoV

use = [1:length(bins)-1]

for i = [1:length(z)]
    [b0 b1] = regress(cov2a(use,i), [ones(length(use),1), use']);
    SLOPE(i) = b0(2);
end

[h p ci stats] = ttest(SLOPE)

%% compare split first second, vs odd even.

fxpred = @(x,slope,crit)((x>crit).*(crit+(x-crit).*slope)+(x<=crit).*x);
lik1 = @(y,x,slope,crit)(sum(min(0.5,(y-fxpred(x,slope,crit)).^2)));
lik = @(y,x,slope,crit)(lik1(y,x,slope,crit) + 2.*(slope-1)^2+3.*(crit-1)^2);

for i = [1:length(z)]
    idx = 1:150;
    fmfx = @(params)(lik(log10(z{i}(idx,ause)), log10(z{i}(idx,3)), params(1), params(2)));
    params = fminsearch(fmfx, [1 1]);
    ss1a(i) = params(1);
    sc1a(i) = max(0, params(2));
end

for i = [1:length(z)]
    idx = 151:300;
    fmfx = @(params)(lik(log10(z{i}(idx,ause)), log10(z{i}(idx,3)), params(1), params(2)));
    params = fminsearch(fmfx, [1 1]);
    ss2a(i) = params(1);
    sc2a(i) = max(0, params(2));
end
for i = [1:length(z)]
    idx = 1:2:299;
    fmfx = @(params)(lik(log10(z{i}(idx,ause)), log10(z{i}(idx,3)), params(1), params(2)));
    params = fminsearch(fmfx, [1 1]);
    ss1b(i) = params(1);
    sc1b(i) = max(0, params(2));
end

for i = [1:length(z)]
    idx = 2:2:300;
    fmfx = @(params)(lik(log10(z{i}(idx,ause)), log10(z{i}(idx,3)), params(1), params(2)));
    params = fminsearch(fmfx, [1 1]);
    ss2b(i) = params(1);
    sc2b(i) = max(0, params(2));
end

[corr(sc1a', sc2a'), corr(ss1a', ss2a')]
[corr(sc1b', sc2b'), corr(ss1b', ss2b')]

%% compare split fifths.


fxpred = @(x,slope,crit)((x>crit).*(crit+(x-crit).*slope)+(x<=crit).*x);
lik1 = @(y,x,slope,crit)(sum(min(0.5,(y-fxpred(x,slope,crit)).^2)));
lik = @(y,x,slope,crit)(lik1(y,x,slope,crit) + 2.*(slope-1)^2+3.*(crit-1)^2);



nfs = [30];
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
    %
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
imagesc(cssc{1}, [0.5, 1]), colormap gray


%% plot of correlation over distance, unnormalized, slope.

usenfs = [1:length(nfs)];
% usenfs = [1,6];
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



%%
close all;
figure()
for i = 10;%[1:length(z)];
    idxuse = 1:300;
%     idxuse = 1:150;
%     idxuse = 2:2:300;%51:300;
%     subplot(1,8,i);
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
    % plot([0 2.5], [0 2.5].*slope(i), 'b-', 'LineWidth', 2);
    s = ss2a(i);
    c = sc2a(i);
%     plot(x, (x>c).*(c + (x-c).*s)+(x<=c).*x, 'g-', 'Color', [0 0.5 0], 'LineWidth', 2);
    xlim([0 2.5]); ylim([0 2.5]);
end

%% try to do some drift calculations

S = cell2mat(ss5');

v1 = mean(diff(S).^2);
v1se = std(diff(S).^2)./sqrt(29);

for i = [1:length(z)]
    [s1 s2] = ndgrid(S(:,i), S(:,i));
    deltaS(:,:,i) = s1-s2;
    for fi = [1:29]
        vk(fi,i) = mean(diag(deltaS(:,:,i).^2,fi));
    end
end
%%
figure();
errorbar([1:29],mean(vk./(repmat(v1, [29 1]).*repmat([1:29]', [1 24])),2), std(vk./(repmat(v1, [29 1]).*repmat([1:29]', [1 24])),[],2)./sqrt(24))
xlabel('k','FontSize',16)
ylabel('var_k / (var_1 * k)','FontSize',16);
ylim([0 1])

%% var things
varnorm = vk./(repmat(v1, [29 1]).*repmat([1:29]', [1 24]));
%%
close all;
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

%%
figure();
errorbar([1:29], ([1:29]*mean(v1)), (sqrt([1:29]).*(std(v1)./sqrt(24)+mean(v1))))
hold on;
errorbar([1:29], mean(vk, 2), std(vk,[],2)./sqrt(24), 'r')

%%

% %%
% err1 = [];
% nlags = 51;
% for i = [1:length(z)]
%     % ok = log10(z{i}(:,3)) > -1;%sc(i);
%     % bad = [];%abs(x)>quantile(abs(x),0.95);
%     stims = log10(z{i}(:,3));
%     resps = nanmean(log10(z{i}(:,[4 5])+1),2);
%     resp1 = log10(max(1,z{i}(:,4)));
%     resp2 = log10(max(1,z{i}(:,5)));
% %     q = rand(size(resp1))>0.5;
% %     resps = resp1.*q + (1-q).*resp2;
%     err = resps-stims;
%     err1 = resp1-stims;
%     err2 = resp2-stims;
%     derr = err1 - err2;
%     [acf] = autocorr(derr, nlags);
%     ac_derr(:,i) = acf;
%     cerrs(i) = corr(err1,err2);
%     
%     
%     errperm = err(randperm(length(err)));%randsample(err, size(err));
%     
%     [xcf] = crosscorr(err,resps,nlags);
%     cc_err_resp(:,i) = xcf(nlags+1:end);
%     [xcf] = crosscorr(err,stims,nlags);
%     cc_err_stim(:,i) = xcf(nlags+1:end);
%     [xcf] = crosscorr(resps,stims,nlags);
%     cc_resp_stim(:,i) = xcf(nlags+1:end);
%     
%     [xcf] = autocorr(err,nlags);
%     ac_err(:,i) = xcf;
%     [xcf] = autocorr(err1,nlags);
%     ac_err1(:,i) = xcf;
%     [xcf] = autocorr(err2,nlags);
%     ac_err2(:,i) = xcf;
%     [xcf] = autocorr(errperm,nlags);
%     ac_errperm(:,i) = xcf;
%     [xcf] = autocorr(stims,nlags);
%     ac_stim(:,i) = xcf;
%     xcf = autocorr(resps,nlags);
%     ac_resp(:,i) = xcf;%corr(resps(1:end-1), resps(2:end));
%     xcf = autocorr(resp2,nlags);
%     ac_resp2(:,i) = xcf;%corr(resps(1:end-1), resps(2:end));
%     % [B Bint Rr] = regress(resps(2:end), resps(1:end-1));
%     % [B Bint Rs] = regress(stims(2:end), stims(1:end-1));
%     % deltar = resps(2:end)-resps(1:end-1);
%     % deltas = stims(2:end)-stims(1:end-1);
%     % errr = Rr-Rs;
%     % errr = deltar-deltas;
%     % ac_res = corr(errr(1:end-1), errr(2:end));
% end
% % fprintf('\nerror:\t%0.3f\t%0.3f', mean(ac), std(ac)./sqrt(length(ac)));
% % fprintf('\nresps:\t%0.3f\t%0.3f', mean(ac_resp), std(ac_resp)./sqrt(length(ac)));
% % fprintf('\nstims:\t%0.3f\t%0.3f', mean(ac_stim), std(ac_stim)./sqrt(length(ac)));
% % fprintf('\nzips:\t%0.3f\t%0.3f', mean(ac_zip), std(ac_zip)./sqrt(length(ac)));
% % fprintf('\nreser:\t%0.3f\t%0.3f', mean(ac_res), std(ac_res)./sqrt(length(ac)));
% %%
% figure();
% errorbar([1:nlags], mean(ac_err(2:end,:),2), std(ac_err(2:end,:),[],2)./sqrt(length(z)), 'r.-', 'LineWidth', 1.5, 'MarkerSize', 30)
% hold on;
% % errorbar([0:nlags], mean(ac_err1,2), std(ac_err2,[],2)./sqrt(length(z)), 'r.-', 'Color', [1.0 0.4 0.4])
% % errorbar([0:nlags], mean(ac_err2,2), std(ac_err2,[],2)./sqrt(length(z)), 'r.-', 'Color', [1.0 0.4 0.4])
% errorbar([1:nlags], mean(ac_stim(2:end,:),2), std(ac_stim(2:end,:),[],2)./sqrt(length(z)), 'b.-', 'LineWidth', 1.5, 'MarkerSize', 30)
% errorbar([1:nlags], mean(ac_resp(2:end,:),2), std(ac_resp(2:end,:),[],2)./sqrt(length(z)), 'g.-', 'Color', [0 0.5 0], 'LineWidth', 1.5, 'MarkerSize', 30)
% % errorbar([0:nlags], mean(ac_resp2,2), std(ac_resp2,[],2)./sqrt(length(z)), 'g.-', 'Color', [0.2 0.7 0.2])
% % errorbar([0:nlags], mean(ac_errperm,2), std(ac_errperm,[],2)./sqrt(length(z)), 'k.-')
% hold on;
% plot([0 nlags], [0 0], 'k-');
% xlim([0 nlags]);
% ylim([-0.025 0.1]);
% %%
% figure();
% errorbar([1:nlags], mean(ac_err(2:end,:),2), std(ac_err(2:end,:),[],2)./sqrt(length(z)), 'r.-', 'LineWidth', 1.5, 'MarkerSize', 20)
% hold on;
% % errorbar([0:nlags], mean(ac_err1,2), std(ac_err2,[],2)./sqrt(length(z)), 'r.-', 'Color', [1.0 0.4 0.4])
% % errorbar([0:nlags], mean(ac_err2,2), std(ac_err2,[],2)./sqrt(length(z)), 'r.-', 'Color', [1.0 0.4 0.4])
% errorbar([1:nlags], mean(cc_err_stim(2:end,:),2), std(cc_resp_stim(2:end,:),[],2)./sqrt(length(z)), 'b.-', 'LineWidth', 1.5, 'MarkerSize', 20)
% % errorbar([1:nlags], mean(cc_err_resp(2:end,:),2), std(cc_err_resp(2:end,:),[],2)./sqrt(length(z)), 'g.-', 'Color', [0 0.5 0], 'LineWidth', 1.5, 'MarkerSize', 20)
% % errorbar([0:nlags], mean(ac_resp2,2), std(ac_resp2,[],2)./sqrt(length(z)), 'g.-', 'Color', [0.2 0.7 0.2])
% % errorbar([0:nlags], mean(ac_errperm,2), std(ac_errperm,[],2)./sqrt(length(z)), 'k.-')
% hold on;
% plot([0 nlags], [0 0], 'k-');
% xlim([0 nlags]);
% ylim([-0.025 0.1]);
% 
% 
% % subplot(4,1,4);
% % errorbar([-20:20], mean(ac_zip,2), std(ac_zip,[],2)./sqrt(length(z)), 'r')
% % hold on;
% % plot([-20 20], [0 0], 'k-');
% % xlim([-5 5]);

%% do something about dropping
% < sc(i)



%% increasing weber fraction?
% 
% bins = [0:10:200];
% 
% for i = [1:length(z)]
%     