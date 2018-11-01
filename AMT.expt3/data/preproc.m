clear all
close all

% 1 workerId;
% 2 assignmentId;
% 3 hitId;
% 4 curtime;
% 5 OSName;
% 6 browserName;
% 7 country;
% 8 IP;
% 9 trialn;
% 10    shown;
% 11    response;
% 12    feedback;
% 13    responseTime;
% 14    fbcondition;
% 15    phase;
% 16    score

fp = fopen('numberline1.txt')
fgetl(fp)
p = textscan(fp, '%s %s %s %f %s %s %s %s %d %d %d %d %d %d %d %d ', 'delimiter', ';');

uwid = unique(p{1})

for i = [1:length(uwid)]
    sidx{i} = [];
    for j = [1:length(p{1})]
        if(strcmp(uwid{i}, p{1}{j}) & j <1680)
            sidx{i}  = [sidx{i} j];
        end
    end
end

for i = [1:length(sidx)]
    z{i} = [double(p{9}(sidx{i})) double(p{10}(sidx{i})) double(p{11}(sidx{i})) double(p{12}(sidx{i}))];
end

close all;
figure()
for i = 1:length(z);%[1:length(z)];
    subplot(4,8,i);
% figure();
    idxuse = [1:length(z{i})]
    for j = [1:10, 20:10:100 200:100:1000]
        plot([0 3], [log10(j) log10(j)], 'k-', 'Color', [0.5 0.5 0.5]);
        hold on;
        plot([log10(j) log10(j)], [0 3], 'k-', 'Color', [0.5 0.5 0.5]);
    end
    plot([0 3], [0 3], 'k-', 'LineWidth', 2);
    plot(min(2.5, log10(z{i}(idxuse,2)+rand(length(idxuse),1).*0.5-0.25)), min(2.5,log10(z{i}(idxuse,3)+rand(length(idxuse),1).*0.5-0.25)),'r.');
    hold on;
    x = [0:0.01:2.5];
    % plot([0 2.5], [0 2.5].*slope(i), 'b-', 'LineWidth', 2);
%     plot(x, (x>sc(i)).*(sc(i) + (x-sc(i)).*ss(i))+(x<=sc(i)).*x, 'g-', 'Color', [0 0.5 0], 'LineWidth', 2);
    xlim([0 3]); ylim([0 3]);
    
end
