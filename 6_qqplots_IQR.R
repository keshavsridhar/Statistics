# Q1 data:
ppr=c(88,76,84,64,60,64,60,64,68,74,
      68,68,72,76,72,52,72,64,60,56,
      72,88,80,76,64,72,60,76,88,72,
      64,60,60,72,92,80,72,64,68)
# Q1a
plot(ecdf(ppr), main="ECDF of pulse rates of Peruvian Indians", xlab="pulse rates")
# Q1b
meanhat = sum(ppr)/length(ppr)
varhat = mean(ppr^2) - mean(ppr)^2
var(ppr)
sd(ppr)
#var(ppr)
# Q1c
median(ppr)
quantile(ppr,0.5)
iqr1 = quantile(ppr, 0.75) - quantile(ppr, 0.25)
quantile(ppr,c(0.25,0.75),type=2)
IQR(ppr)
# Q1d
IQR(ppr)/sqrt(varhat)
iqrsd = function(x)
{
  x.mean = mean(x)
  x.var = mean(x^2) - mean(x)^2
  q = as.vector(quantile(x,probs=c(0.25,0.75)))
  x.iqr = q[2] - q[1]
  return(x.iqr/sqrt(x.var))
}
iqrsd(ppr)
# Q1e
boxplot(ppr, main="Boxplot of the pulse rates of Peruvian Indians", ylab="Pulse rates")
# Q1f
qqnorm(ppr)
# Q1g
plot(density(ppr))
# Q1h
#hist(ppr)
# The iqr/sd is close to 1.35(normal). So I think the normal distribution is a good approximation
# for this sample even though there is some skewness towards the upper end of the distribution.

# Q2 data:
xbar = c(0.246,0.327,0.423,0.425,0.434,
         0.530,0.583,0.613,0.641,1.054,
         1.098,1.158,1.163,1.439,1.464,
         2.063,2.105,2.106,4.363,7.517)
# Q2a
plot(ecdf(xbar))
# Q2b
xbar.mean = mean(xbar)
xbar.var = mean(xbar^2) - xbar.mean^2
median(xbar)
quantile(xbar,0.5)
xbar.iqr=quantile(xbar,0.75) - quantile(xbar,0.25)
IQR(xbar)
# Q2c
xbar.iqr/sqrt(xbar.var)
# I don't think it was drawn from a normal distribution as the iqr/sd is not close to 1.35
# Q2d
qqnorm(xbar)
# The distribution doesnt seem normal as the line is curvy at the end.
# Q2e
ybar = log(xbar)
IQR(ybar)/sqrt(var(ybar))
qqnorm(ybar)
plot(density(ybar))
# This looks more like a normal distribution as the iqr/sd is close to 1.35 and
# the qqnorm resembles a straight line. So the logged data resembles an approximately normal distribution.

# Q3e
1 - pnorm(0.5, mean=0, sd=sqrt(16.2/100))

# Q5 data:
household_sizes = rep(1:7, c(27, 34, 16, 13, 6, 3, 1))
# Q5a
mean(household_sizes)
# Q5b
sd(household_sizes)
hs.var = mean(household_sizes^2) - mean(household_sizes)^2
hs.sd = sqrt(hs.var)
# Q5c
hs.err = hs.sd/sqrt(length(household_sizes))
# Q5d
pnorm(0.5,0,hs.err) - pnorm(-0.5,0,hs.err)
