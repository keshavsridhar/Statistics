# Q3 data:
sister.heights = c(69,64,65,63,65,62,65,64,66,59,62)
brother.heights = c(71,68,66,67,70,71,70,73,72,65,66)
# Q3a:
1 - pnorm(70,mean(brother.heights),sd(brother.heights))
# Q3b:
# taking bros as ys and sis as xs
r=cor(brother.heights,sister.heights)
slope = r*sd(brother.heights)/sd(sister.heights)
intercept = mean(brother.heights) - slope*mean(sister.heights)
carol.bro.height = intercept + 61*slope
carol.bro.height
# Q3c:
carol.bro.height
sd.error = sd(brother.heights)*sqrt(1-r^2)
1 - pnorm(70,carol.bro.height,sd.error)

# Q4a:
m1 = mean(brother.heights)
m2 = mean(sister.heights)
s1 = sd(brother.heights)
s2 = sd(sister.heights)
z1 = (brother.heights - m1)/s1
z2 = (sister.heights - m2)/s2
z = sum(z1*z2)/(length(sister.heights)-1)
r = cor(brother.heights,sister.heights)
r^2
# Q4b:
alpha = 0.05
se = (s1/s2)*sqrt((1-r^2)/9)
t = slope/se
2*(1 - pt(abs(t), df = 9))
# Q4c:
slope
slope + qt(0.95, df = length(sister.heights)-2)*se
slope - qt(0.95, df = length(sister.heights)-2)*se
# Q4d:
n = (2 * qnorm(0.975)*s1*sqrt(1-r^2)/(s2*0.1))^2 + 2

# Q5a:
m1 = 75
s1 = 10
m2 = 64
s2 = 12
r = 0.5
n = 33
slope = r*s2/s1
intercept = m2 - slope*m1
second.test.score = intercept + 80*slope
# Q5b:
slope2 = r*s1/s2
intercept2 = m1 - slope2*m2
first.test.score = intercept2 + 76*slope2

# Q6 data:
set.seed(2000017447)
x = rnorm(500)
y = 2*x + rnorm(500)
# Q6a:
mean(x)
mean(y)
sd(x)
sd(y)
r=cor(x,y)
# Q6b:
slope = cor(x,y)*sd(y)/sd(x)
intercept = mean(y) - slope*mean(x)
y1 = intercept + slope*x
intercept
slope
# Q6c:
# P(y>3|x=1)
# = 1-P(y<=3|x=1)
yi = intercept + slope*1
yi
1 - pnorm(3,yi,sd(y)*sqrt(1-r^2))
