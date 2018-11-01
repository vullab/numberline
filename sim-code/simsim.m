clear all
close all
F = 50;
n(1) = 25;
n(2) = 75;
n(3) = 50;

sx = 0.05;
sn = 1;
sa = 0.2;

% gibbs

ss = [1:400];
as = 10.^(-0.5:0.01:0.5);

[S A] = ndgrid(ss, as);

colors = {'r', 'b', 'k'}

    figure();
for i = [1:3]
    lik1 = log10(normpdf(S.^A, n(i), sn));
    lik2 = log10(normpdf(log10(S), log10(F), sx));
    prior = log10(normpdf(log10(A), 0, sa));
    post = lik1+lik2+prior;
    post = post - max(post(:));

    postp = 10.^post;
    postp = postp./sum(postp(:));

%     surf(S, A, postp, 'EdgeColor','none','LineStyle','none')
%     view(0,-90)

    postA = repmat(sum(postp, 1), [length(ss), 1]);

    % predictions

    for x = [1:F];
        spdf = normpdf(log10(S), log10(x), sx);
        spdf = spdf./sum(spdf(:));
        jpdf = spdf.*postA;
        jpdf = jpdf./sum(jpdf(:));

        mux(x) = sum(sum(S.^A.*jpdf, 1), 2);
        sdx(x) = sqrt(sum(sum(jpdf.*(S.^A).^2,1),2)-mux(x).^2);


    end

    errorbar([1:F], mux, sdx, colors{i});
    hold on;
    
end
