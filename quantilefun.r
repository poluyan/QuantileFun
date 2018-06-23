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

y <- f(x)
plot(x,y,type="l")
rug(x)
title(main="pdf")

y <- y/sum(y)

cdf <- cumsum(y)

plot(x,cdf,type="l")
title(main="cdf")

cdf <- c(0, cdf)

n = 1000 # number of sample
random = NULL
sample = NULL
for(i in 1:n)
{
  rnd <- runif(1, 0, 1)

  # linear interpolation
  index <- which(cdf == min(cdf[cdf>rnd])) - 1
  x0 <- x[index]
  y0 <- cdf[index]
  x1 <- x[index + 1]
  y1 <- cdf[index + 1]
  fx <- x0 + (rnd - y0) * (x1 - x0) / (y1 - y0)

  random = c(random,  rnd)
  sample = c(sample,  fx)
}