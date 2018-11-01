clear all
close all

x = [1:1:350];

END = [75, 350, 750];

GUESS = {};


for j = [1:length(END)]
    for i = [1:length(x)]
        
        s = 10.^(randn(1000,1).*0.1+log10(x(i)));
        E = END(j);
%         E = END(j) + randn(1000,1).*30;
%         Q = 10.^(log10(max(x)) + randn(1000,1).*0.001);
        Q = 350;
        
%         GUESSES = s.*E./Q;
        GUESSES = 10.^(log10(s).*log10(E)./log10(Q));

        mu(i,j) = mean(GUESSES);
        sd(i,j) = std(GUESSES);
    end
end

figure();
subplot(3,1,1);
plot(x, mu)
xlabel('x')
ylabel('mean N')
subplot(3,1,2);
plot(x, sd)
xlabel('x')
ylabel('sd N')
subplot(3,1,3);
plot(x, sd./mu)
xlabel('x')
ylabel('sd N / mean N')
