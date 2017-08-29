# Q1 data:
# different types of rat feeds
mu1=83.5
mu2=92.3
mu3=88.6
mu4=99.4
s1=16.9
s2=14.6
s3=14.2
s4=14.1
grand.mean = 35*(83.5+92.3+88.6+99.4)/140
ssb=35*(83.5-grand.mean)^2+35*(92.3-grand.mean)^2+35*(88.6-grand.mean)^2+35*(99.4-grand.mean)^2
ssb.ms = ssb/3
ssw = 34*(s1^2 + s2^2 + s3^2 + s4^2)
ssw.ms = ssw/136
Fstat = ssb.ms/ssw.ms
P.value = 1 - pf(Fstat, df1=3, df2= 136)

# Q2 data:
# hemoglobin levels for 3 types of sickle cell disease
ss = c(7.2, 7.7, 8.0, 8.1, 8.3, 8.4, 8.4, 8.5, 8.6, 8.7, 9.1, 9.1, 9.1, 9.8, 10.1, 10.3)
st = c(8.1, 9.2, 10.0, 10.4, 10.6, 10.9, 11.1, 11.9, 12.0, 12.1)
sc = c(10.7, 11.3, 11.5, 11.6, 11.7, 11.8, 12.0, 12.1, 12.3, 12.6, 12.6, 13.3, 13.3, 13.8, 13.9)
# Q2 1:
sd(ss)
sd(st)
sd(sc)
par(mfrow=c(1,1))
boxplot(ss,st,sc,names=c("ss","st","sc"))
par(mfrow=c(1,3))
qqnorm(ss, main="ss")
qqnorm(st, main="st")
qqnorm(sc, main="sc")
# Q2 2:
alpha = 0.05
grand.mean = (length(ss)*mean(ss) + length(st)*mean(st) + length(sc)*mean(sc))/(length(ss)
                                                                                +length(st)
                                                                                +length(sc))
ssb = length(ss)*(mean(ss)-grand.mean)^2 + length(st)*(mean(st)-grand.mean)^2 +
  length(sc)*(mean(sc)-grand.mean)^2
ssb.ms = ssb/2
ssw = (length(ss)-1)*(sd(ss)^2)+(length(st)-1)*(sd(st)^2)+(length(sc)-1)*(sd(sc)^2)
ssw.ms = ssw/(41-3)
Fstat = ssb.ms/ssw.ms
P.value = 1 - pf(Fstat, df1=2,df2=38)

# Q3 data:
# survival days of 5 different cancer types after ascorbate treatment
x1=c(124,42,25,45,412,51,1112,46,103,876,146,340,396)
x2=c(81,461,20,450,246,166,63,64,155,859,151,166,37,223,138,72,245)
x3=c(248,377,189,1843,180,537,519,455,406,365,942,776,372,163,101,20,283)
x4=c(1234,89,201,356,2970,456)
x5=c(1235,24,1581,1166,40,727,3808,791,1804,3460,719)
y1<-log(x1)
y2<-log(x2)
y3<-log(x3)
y4<-log(x4)
y5<-log(x5)
par(mfrow=c(1,1))
boxplot(x1,x2,x3,x4,x5, main="boxplot of survival times(days) after ascorbate treatment",
        names=c("Stomach","Bronchus","Colon","Ovary","Breast"))
boxplot(y1,y2,y3,y4,y5, main="boxplot of log survival times(days) after ascorbate treatment",
        names=c("Stomach","Bronchus","Colon","Ovary","Breast"))
sd(x1)
sd(x2)
sd(x3)
sd(x4)
sd(x5)

sd(y1)
sd(y2)
sd(y3)
sd(y4)
sd(y5)

par(mfrow=c(2,3))
qqnorm(x1, main="Stomach")
qqnorm(x2, main="Bronchus")
qqnorm(x3, main="Colon")
qqnorm(x4, main="Ovary")
qqnorm(x5, main="Breast")

par(mfrow=c(2,3))
qqnorm(y1, main="logStomach")
qqnorm(y2, main="logBronchus")
qqnorm(y3, main="logColon")
qqnorm(y4, main="logOvary")
qqnorm(y5, main="logBreast")

n1=length(y1)
n2=length(y2)
n3=length(y3)
n4=length(y4)
n5=length(y5)
N=n1+n2+n3+n4+n5
grand.mean = (n1*(mean(y1))+n2*(mean(y2))+n3*(mean(y3))+n4*(mean(y4))+n5*(mean(y5)))/N
ssb = n1*(mean(y1)-grand.mean)^2+n2*(mean(y2)-grand.mean)^2+n3*(mean(y3)-grand.mean)^2+
  n4*(mean(y4)-grand.mean)^2+n5*(mean(y5)-grand.mean)^2
ssb.ms = ssb/4
ssw = (n1-1)*var(y1) + (n2-1)*var(y2) + (n3-1)*var(y3) + (n4-1)*var(y4) + (n5-1)*var(y5)
ssw.ms = ssw/(N-5)
Fstat = ssb.ms/ssw.ms
P.value = 1 - pf(Fstat, df1=4, df2=N-5)
