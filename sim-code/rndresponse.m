function n = rndresponse(sz, x, ma, ss, sa)
    s = 10.^(randn(sz).*ss+log10(x));
    a = 10.^(randn(sz).*sa+0);
    n = floor(s.^a+0.5);

