#Q4 ch 5.6 ex 7
#a P(X < 0)
pnorm(0,mean=-5,sd=10)
#b P(X > 5)
1 - pnorm(5,-5,10)
#c P(-3 < X < 7)
pnorm(7,-5,10) - pnorm(-3,-5,10)
#d P(|X + 5| < 10)
#-15 < X < 5
pnorm(5,-5,10) - pnorm(-15,-5,10)
#e P(|X-3|>2)
# = 1 - P(|X-3|<2)
1-(pnorm(5,-5,10) - pnorm(1,-5,10))

#Q6
#a P(X1 > 72)
#= 1 - P(X1 <= 72)
#Men:
1 - pnorm(72,69.2,2.9)
#Women:
1 - pnorm(72,63.8,2.9)
#c P((X1 + X)/2 > 72)
#= 1 - P(Y <= 144)
sqrt(16.82)
1 - pnorm(144,133,sqrt(16.82))
(1 - pnorm(72,69.2,2.9))* (1 - pnorm(72,63.8,2.9))

#d
#E[X1-X2]
69.2-63.8
#Var[X1-X2]
2.9^2 + 2.9^2

#e
#P(X1 - X2 < 0)
#P(D<0)
pnorm(0,5.4,sqrt(16.82))
pnorm(0,5.4,4.1012)

#Men:
x=seq(50,90,0.1)
f=dnorm(x,69.2,2.9)
plot(x,f,type ="l")
abline(v=69.2)
#Women:
y=seq(50,90,0.1)
g=dnorm(y,63.8,2.9)
plot(y,g,type="l")
abline(v=63.8)
