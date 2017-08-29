getwd()
# Q1
typea=c(233, 291, 312, 250, 246, 197, 268, 224, 239, 239,254 ,276 ,234 ,181 ,248 ,252 ,202 ,218 ,212 ,325)
typeb=c(344, 185, 263, 246, 224, 212, 188, 250, 148, 169,226, 175, 242, 252, 153, 183, 137, 202, 194, 213)
qqnorm(typea)
qqnorm(typeb)
t.test(typea, typeb, alt="greater")
mu = mean(typea) - mean(typeb)
sd.err = sqrt((sd(typea)^2)/length(typea) + (sd(typeb)^2)/length(typeb))
mu - qt(0.95, df=35.413)*sd.err
mu + qt(0.95, df=35.413)*sd.err
# Q2 data:
normal = c(4.1, 6.3, 7.8, 8.5, 8.9, 10.4, 11.5, 12.0, 13.8, 17.6, 24.3, 37.2)
diabetic = c(11.5, 12.1, 16.1, 17.8, 24.0, 28.8, 33.9, 40.7, 51.3, 56.2, 61.7, 69.2)
# Q2 1:
mean(normal)
mean(diabetic)
plot(normal)
qqnorm(normal)
qqnorm(diabetic)
plot(ecdf(normal))
plot(ecdf(diabetic))
plot(density(normal))
abline(v=mean(normal), col="blue")
plot(density(diabetic))
abline(v=mean(diabetic), col="green")
# Q2 2:
lognorm = log(normal)
logdiab = log(diabetic)
plot(density(lognorm))
abline(v=mean(lognorm), col="blue")
plot(density(logdiab))
abline(v=mean(logdiab), col="green")
sqnorm = sqrt(normal)
sqdiab = sqrt(diabetic)
plot(density(sqnorm))
abline(v=mean(sqnorm), col="blue")
plot(density(sqdiab))
abline(v=mean(sqdiab), col="green")
# Q2 3:
qqnorm(lognorm)
qqnorm(logdiab)
qqnorm(sqnorm)
qqnorm(sqdiab)
# Q2 4:
delta.hat = mean(lognorm) - mean(logdiab)
s1 = sd(lognorm)
n1 = length(lognorm)
s2 = sd(logdiab)
n2 = length(logdiab)
sd.error = sqrt((s1^2)/n1 + (s2^2)/n2)
t.val = delta.hat/sd.error
nu = (((s1^2)/n1 + (s2^2)/n2)^2)/(((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1)))
p.val = pt(t.val, df=nu)
delta.hat - qt(0.975,df=21.9)*sd.error
delta.hat + qt(0.975,df=21.9)*sd.error
t.test(lognorm, logdiab, alt="less")

# Q3 data:
movies.1956 = c(74,114,114,87,92,55,67,118,79,83,79,92,99,87)
movies.1996 = c(70,98,90,95,88,108,110,96,91,88,120,96,90,90)
plot(density(movies.1956))
plot(density(movies.1996))
plot(density(log(movies.1956)))
plot(density(log(movies.1996)))
qqnorm(movies.1956)
qqnorm(movies.1996)
qqnorm(log(movies.1956))
qqnorm(log(movies.1996))
qqnorm(sqrt(movies.1956))
qqnorm(sqrt(movies.1996))
t.test(movies.1956, movies.1996, alt="less")
delta.hat = mean(movies.1956) - mean(movies.1996)
sd.error = sqrt((sd(movies.1956)^2/length(movies.1956))+(sd(movies.1996)^2/length(movies.1996)))
delta.hat - qt(0.975,df=22.395)*sd.error
delta.hat + qt(0.975,df=22.395)*sd.error
t.test(movies.1956, movies.1996, alt="less")$conf.int
wilcox.test(movies.1956, movies.1996, conf.int=TRUE)
