clear all
close all

fpath = 'data/';
files = dir(sprintf('%s*.csv',fpath));

nsubs = 0;

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
end


%% bilinear model.

% likelihood of some data.
lik1 = @(y,x,slope,crit)(sum(min(0.5,(y-((x>crit).*(crit+(x-crit).*slope)+(x<=crit).*x)).^2)));
% add prior (this prior is based on mean/variance across subjects, and it
% keeps results from going nuts due to outliers when there isn't a lot of
% data.  This is a crude way of doing the full hierarchical model
lik = @(y,x,slope,crit)(lik1(y,x,slope,crit) + 2.*(slope-1)^2+3.*(crit-1)^2);

% fir each subject slope and critical value.
for i = [1:length(z)]
    fmfx = @(params)(lik(log10(z{i}(:,4)), log10(z{i}(:,3)), params(1), params(2)));
    params = fminsearch(fmfx, [1 1]);
    ss(i) = params(1);
    sc(i) = max(0, params(2));
end



%% plot data and bilinear model fit.

close all;
figure();

for i = [1:length(z)]
    subplot(4,6,i);
    idxuse = 1:300; % (sometimes I use part of the trials to illustrated split half differences)
    % this draws the log grid.
    for j = [1:10, 20:10:100 200 300]
        plot([0 2.5], [log10(j) log10(j)], 'k-', 'Color', [0.5 0.5 0.5]);
        hold on;
        plot([log10(j) log10(j)], [0 2.5], 'k-', 'Color', [0.5 0.5 0.5]);
    end
    % this draws the identity line.
    plot([0 2.5], [0 2.5], 'k-', 'LineWidth', 2);
    % this draws all the data points.
    plot(min(2.5, log10(z{i}(idxuse,3)+rand(length(idxuse),1).*0.5-0.25)), min(2.5,log10(z{i}(idxuse,4)+rand(length(idxuse),1).*0.5-0.25)),'r.');
    hold on;
    % get the subject's slope and critical point
    s = ss(i);
    c = sc(i);
    x = [0:0.01:2.5];
    % plot the best bilinear fit
    plot(x, (x>c).*(c + (x-c).*s)+(x<=c).*x, 'g-', 'Color', [0 0.5 0], 'LineWidth', 2);
    % fit axes to be nice.
    xlim([0 2.5]); ylim([0 2.5]); 
end



%% autocorrelation analysis
nlags = 51; 
% for each subject.
for i = [1:length(z)]
    % actual stimulus numbers
    stims = log10(z{i}(:,3));
    % responses (this is the average of two responses on one trial --
    % doesn't matter, can do with individual responses -- just a feature of
    % the first experiment we ran).
    resps = nanmean(log10(z{i}(:,[4 5])+1),2);
    % error of response.
    err = resps-stims;

    % permuted error vector for null hypothesis.  not really necessary.
    errperm = err(randperm(length(err)));%randsample(err, size(err));
    
    % error autocorrelation
    [xcf] = autocorr(err,nlags);
    ac_err(:,i) = xcf;
    % randomized error autocorrelation
    [xcf] = autocorr(errperm,nlags);
    ac_errperm(:,i) = xcf;
    % stimulus autocorrelation
    [xcf] = autocorr(stims,nlags);
    ac_stim(:,i) = xcf;
    % response autocorrelation
    xcf = autocorr(resps,nlags);
    ac_resp(:,i) = xcf;
end

% plot autocorrelations with error bars.
figure();
errorbar([1:nlags], mean(ac_err(2:end,:),2), std(ac_err(2:end,:),[],2)./sqrt(length(z)), 'r.-', 'LineWidth', 1.5, 'MarkerSize', 30)
hold on;
errorbar([1:nlags], mean(ac_stim(2:end,:),2), std(ac_stim(2:end,:),[],2)./sqrt(length(z)), 'b.-', 'LineWidth', 1.5, 'MarkerSize', 30)
errorbar([1:nlags], mean(ac_resp(2:end,:),2), std(ac_resp(2:end,:),[],2)./sqrt(length(z)), 'g.-', 'Color', [0 0.5 0], 'LineWidth', 1.5, 'MarkerSize', 30)
hold on;
plot([0 nlags], [0 0], 'k-');
xlim([0 nlags]);
ylim([-0.025 0.1]);

%% compare split nths.  
% split into n modular or blocked subsets.  
% do model fits for the subsets.  
% estimate cross-subset correlation

nfs = [2, 3, 5, 10, 15, 20, 30];
for q = [1:length(nfs)];
    clear ss5 sc5 ss5c sc5c;
    nf = nfs(q);
    % f split of nf total splits.
    for f = [1:nf]
        % blocked split indices
        idx = ((f-1)*300/nf)+[1:300/nf];
        % modular split indices.
        idxc = f:nf:300;
        for i = [1:length(z)]
            % blocked model fits for this subset.
            fmfx = @(params)(lik(log10(z{i}(idx,4)), log10(z{i}(idx,3)), params(1), params(2)));
            params = fminsearch(fmfx, [1 1]);
            ss5{f}(i) = params(1);
            sc5{f}(i) = params(2);
            % modular model fits for this subset.
            fmfx = @(params)(lik(log10(z{i}(idxc,4)), log10(z{i}(idxc,3)), params(1), params(2)));
            params = fminsearch(fmfx, [1 1]);
            ss5c{f}(i) = params(1);
            sc5c{f}(i) = params(2);
        end
    end
    
    % calculate cross-subset correlation.
    for f1 = [1:nf]
        for f2 = [1:nf]
            css{q}(f1,f2) = corr(ss5{f1}', ss5{f2}');
            csc{q}(f1,f2) = corr(sc5{f1}', sc5{f2}');
            
            cssc{q}(f1,f2) = corr(ss5c{f1}', ss5c{f2}');
            cscc{q}(f1,f2) = corr(sc5c{f1}', sc5c{f2}');
        end
    end
    
    % calculate average correlations for subsets at different distances
    % (different off-diagonal lines).
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

%% first: show cross-subset correlation matrices

figure();
% change variable to look at different ones.
imagesc(cssc{7}, [0.5, 1]), colormap gray 


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

%% plot normalized.
figure();
for q = [usenfs];
nf = nfs(q);
errorbar([1:nf-1]./(nf), mcs{q}./mean(mcsc{q}), scs{q}./mean(mcsc{q}), 'r.-');
hold on;
errorbar([1:nf-1]./(nf), mcsc{q}./mean(mcsc{q}), scsc{q}./mean(mcsc{q}), 'k.-');
end
ylim([0.4, 1.05])
xlim([0 1]);


%% compare split nths.  
% split into n modular or blocked subsets.  
% do model fits for the subsets.  
% estimate cross-subset correlation

    clear ss5 sc5 ss5c sc5c;
    k = 10;
    for f = [1:291]
        % blocked split indices
        idx = f:(f+k);
        for i = [1:length(z)]
            % blocked model fits for this subset.
            fmfx = @(params)(lik(log10(z{i}(idx,4)), log10(z{i}(idx,3)), params(1), params(2)));
            params = fminsearch(fmfx, [1 1]);
            ss5(f,i) = params(1);
            sc5(f,i) = params(2);
        end
    end
