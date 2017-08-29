# Q3
s=c(0.693, 0.662, 0.690, 0.606, 0.570,0.749 ,0.672 ,0.628 ,0.609 ,0.844 ,0.654 ,0.615 ,0.668 ,0.601 ,0.576 ,0.670 ,0.606 ,0.611 ,0.553 ,0.933)
s1=s-0.618034
qqnorm(s)
qqnorm(log(s))
deltahat = mean(s) - 0.618034
tstat = deltahat/(sd(s)/sqrt(length(s)))
2 * (1 - pt(abs(tstat), df=length(s)-1))

deltahat1 = mean(s1)
tstat1 = deltahat1/(sd(s1)/sqrt(length(s1)))
2 * (1 - pt(abs(tstat1), df=length(s1)-1))

# Q4
n = 61
tstat = (6.5-0)/(12/sqrt(61))
1 - pt(abs(tstat), df=n-1)
q = qt(0.975, df=n-1)
6.5 - q*12/sqrt(61)
6.5 + q*12/sqrt(61)


# Q6
crabcounts = read.table("crab-count.txt", header=TRUE)
View(crabcounts)
