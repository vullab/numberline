clear all
close all

fpath = '../data/';
files = dir(sprintf('%s*.csv',fpath));

nsubs = 0;

aidx = 3;
nidx = 2;

%% file headers
% 1 run
ridx = 1;
% 2 num_dots
nidx = 2;
% 3 answer
aidx = 3;
% 4 feedback
% 5 a
sidx = 5;
% 6 fbn
fidx = 6;
% 7 points
% 8 time

for fi = [1:length(files)]
    curfile = fopen(sprintf('%s%s', fpath, files(fi).name));
    discard = fgetl(curfile);
    
    zip = textscan(curfile,'%f %f %f %s %f %f %f %f %f ', 'delimiter', ',');
    q1 = cell2mat(zip(1:3));
    q2 = cell2mat(zip(5:7));
    fclose(curfile);
    if(length(q1)==500)
        nsubs = nsubs+1;
        z{nsubs} = double([q1 q2]);
    else
        sprintf('%d\t%s\n', length(q1), files(fi).name)
    end
end

grp1 = [];
grp2 = [];
for i = [1:nsubs]
    if(z{i}(150,4)==1.25)
        grp1(end+1) =  i;
    else
        grp2(end+1) = i;
    end
end


%%  plot data!
close all;
figure()
subsets = {[1:100], [101:200], [201:300], [301:400], [401:500]};

for a = [1:length(subsets)]
    idxuse = subsets{a};
    figure();
    for i = 1:nsubs;%[1:length(z)];
        subplot(4,8,i);
        for j = [1:10, 20:10:100 200]
            plot([0 2.5], [log10(j) log10(j)], 'k-', 'Color', [0.5 0.5 0.5]);
            hold on;
            plot([log10(j) log10(j)], [0 2.5], 'k-', 'Color', [0.5 0.5 0.5]);
        end
        plot([0 2.5], [0 2.5], 'k-', 'LineWidth', 2);
        plot(min(2.5, log10(z{i}(idxuse,nidx)+1+rand(length(idxuse),1).*0.5-0.25)), min(2.5,log10(z{i}(idxuse,aidx)+1+rand(length(idxuse),1).*0.5-0.25)),'r.');
        x = [0:0.01:2.5];
        xlim([0 2.5]); ylim([0 2.5]);        
    end
end

%% compare split nths.

ntrials = 500;
fxpred = @(x,slope,crit)((x>crit).*(crit+(x-crit).*slope)+(x<=crit).*x);
lik1 = @(y,x,slope,crit)(sum(min(0.5,(y-fxpred(x,slope,crit)).^2)));
lik = @(y,x,slope,crit)(lik1(y,x,slope,crit) + 2.*(slope-1)^2+3.*(crit-1)^2);

nfs = [50];
for q = [1:length(nfs)];
    clear ss5 sc5 ss5c sc5c;
    nf = nfs(q);
    for f = [1:nf]
        idx = ((f-1)*ntrials/nf)+[1:ntrials/nf];
        idxc = f:nf:ntrials;
        for i = [1:length(z)]
            % this is split sequentially
            fmfx = @(params)(lik(log10(z{i}(idx,aidx)), log10(z{i}(idx,nidx)), params(1), params(2)));
            params = fminsearch(fmfx, [1 1]);
            ss5{f}(i) = params(1);
            sc5{f}(i) = params(2);
            % this is split by modulus
            fmfx = @(params)(lik(log10(z{i}(idxc,aidx)), log10(z{i}(idxc,nidx)), params(1), params(2)));
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
imagesc(cssc{1}, [0.5, 1]), colormap gray

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
% ylim([0.4, 1])
% xlim([0 1]);

% normalized
figure();
for q = [usenfs];
    nf = nfs(q);
    errorbar([1:nf-1]./(nf), mcs{q}./mean(mcsc{q}), scs{q}./mean(mcsc{q}), 'r.-');
    hold on;
    errorbar([1:nf-1]./(nf), mcsc{q}./mean(mcsc{q}), scsc{q}./mean(mcsc{q}), 'k.-');
end
% ylim([0.4, 1.05])
% xlim([0 1]);

%% random walk calculations
clear deltaS vk v1
S = cell2mat(ss5');
subsets = {[1:10] [11:20], [21:30], [31:40], [41:50]};

for k = [1:length(subsets)]
    useblocks = subsets{k};
    S = cell2mat(ss5');
    useblocks
    S = S(useblocks,:);
    v1{k} = mean(diff(S).^2);
    % v1se = std(diff(S).^2)./sqrt(length(useblocks)-1);
    for i = [1:length(z)]
        [s1, s2] = ndgrid(S(:,i), S(:,i));
        delta = s1-s2;
        for fi = [1:(length(useblocks)-1)]
            vk{k}(fi,i) = mean(diag(delta.^2,fi));
        end
    end
end
% plot var_k / (var_1 * k)
colors = {'r', 'b', 'm', 'g', 'k'};
figure();
for k = [1:length(subsets)]
    varnorm = vk{k}./(repmat(v1{k}, [length(useblocks)-1 1]).*repmat([1:(length(useblocks)-1)]', [1 length(v1{k})]));
    errorbar([1:(length(useblocks)-1)], mean(varnorm,2), std(varnorm,[],2)./sqrt(length(v1{k})), colors{k})
    hold on;
end
xlabel('k','FontSize',16)
ylabel('var_k / (var_1 * k)','FontSize',16);
ylim([0 1])
%%
S = cell2mat(ss5');

figure();
errorbar([1:50], mean(S(:,grp1),2), std(S(:,grp1),[],2)./sqrt(length(grp1)), 'b-')
hold on
errorbar([1:50], mean(S(:,grp2),2), std(S(:,grp2),[],2)./sqrt(length(grp2)), 'r-')
axis tight
ys = ylim();
for i = [10.5:10:40.5]
    plot([i i], ys, 'k-')
end
axis tight;