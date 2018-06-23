set.seed(1)
n = 100
x = seq(-10,10,length=n)

fnrm = function(x, mu, sigma)
{
    ps = 2.0 * sigma ** 2.0
    e = exp((-(x - mu) ** 2.0) / ps)
    C = sqrt(2.0 * ps * acos(-1.0))
    e / C
}

# PDF function
f = function (x)
{
  rez <- 0
  rez <- rez + fnrm(x, 0, 1) 
  rez <- rez + 0.25*fnrm(x, -7.5, 0.5) 
  rez <- rez + 0.4*fnrm(x, 7.5, 0.5) 
  rez
}

plot(x,f(x),type="l")
rug(x)
title(main="pdf")