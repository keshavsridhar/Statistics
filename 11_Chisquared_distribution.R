# Q1:
observed = c(29,19,18,25,17,10,15,11)
sum(observed)
expected = rep(144/8,8)
sum(expected)
pcs_val = sum((observed-expected)^2/expected)
pcs_val
g2 = 2*sum(observed*log(observed/expected))
g2
1 - pchisq(pcs_val, df=8-1)
1 - pchisq(g2, df=8-1)

# Q2:
observed = c(30,93,159,184,195,171,92,45,31)
expected=rep(NA,9)
expected[1] = pbinom(1,16,0.29)*1000
expected[2:8] = dbinom(2:8,16,0.29)*1000
expected[9] = (1 - pbinom(8,16,0.29))*1000
data.frame(observed,expected)
g2 = 2*sum(observed*log(observed/expected))
g2
x2 = sum((observed-expected)^2/expected)
x2
1-pchisq(g2,df=9-1)
1-pchisq(x2,df=9-1)

# Q3:
observed = c(107,55,39,22,13,18,13,23,15)
newcombf = function(x)
{
  y = log10((1+x)/x)
  return(y)
}
sum(observed)
expected = newcombf(c(1:9))*305
data.frame(observed, expected)
g2 = 2 * sum(observed*log(observed/expected))
g2
x2 = sum((observed-expected)^2/expected)
x2
1-pchisq(g2,df=9-1)
1-pchisq(x2,df=9-1)

# Q4:
observed = c(173,125,150,73)
N=sum(observed)
p.m = 298/521
p.f = 1 - p.m
p.3 = 323/521
p.35 = 1 - p.3
expected=c(N*p.m*p.3, N*p.m*p.35, N*p.f*p.3, N*p.f*p.35)
data.frame(observed, expected)
g2 = 2*sum(observed*log(observed/expected))
g2
x2 = sum((observed-expected)^2/expected)
x2
1-pchisq(g2,df=4-1-2)
1-pchisq(x2,df=4-1-2)

# Q5:
observed=c(53,3057,110,4621,27,606)
N = sum(observed)
p.lowang = 3110/8474
p.modang = 4731/8474
p.hiang = 1 - (p.lowang + p.modang)
p.hd = 190/8474
p.nhd = 1 - p.hd
expected = c(N*p.lowang*p.hd,N*p.lowang*p.nhd,N*p.modang*p.hd,N*p.modang*p.nhd,
             N*p.hiang*p.hd,N*p.hiang*p.nhd)
data.frame(observed,expected)
g2 = 2 * sum(observed*log(observed/expected))
g2
x2 = sum((observed-expected)^2/expected)
x2
1-pchisq(g2,df=6-1-3)
1-pchisq(x2,df=6-1-3)

# Q6:
EPL201516 = read.csv("http://www.football-data.co.uk/mmz4281/1516/E0.csv")
# Q6a: home goals:
homegoals=EPL201516$FTHG
table(homegoals)
ave=sum(homegoals)/length(homegoals)
observed=data.frame(table(homegoals))
observed=observed$Freq
obs=rep(NA,6)
obs[1:5]=observed[1:5]
obs[6]=observed[6]+observed[7]
expected=rep(NA,6)
expected[1:5]=dpois(0:4,ave)*380
expected[6]=(1-ppois(4,ave))*380
data.frame(obs,expected)
g2 = 2*sum(obs*log(obs/expected))
g2
x2 = sum((obs-expected)^2/expected)
x2
1-pchisq(g2,df=6-1-1)
1-pchisq(x2,df=6-1-1)
# Q6b: away goals:
awaygoals=EPL201516$FTAG
sum(awaygoals)
length(awaygoals)
avg=sum(awaygoals)/length(awaygoals)
obs=data.frame(table(awaygoals))
observed=obs$Freq
fourormore=sum(observed[5:7])
observed=observed[0:4]
observed[5]=fourormore
expected=rep(NA,5)
expected[1:4]=dpois(0:3,avg)*380
expected[5]=(1-ppois(3,avg))*380
data.frame(observed,expected)
g2 = 2*sum(observed*log(observed/expected))
g2
x2 = sum((expected-observed)^2/expected)
x2
1-pchisq(g2,df=5-1-1)
1-pchisq(x2,df=5-1-1)
# Q6c:
total=EPL201516$FTAG+EPL201516$FTHG
sum(total)
length(total)
avg=sum(total)/length(total)
obs=data.frame(table(total))
observed=obs$Freq
eormore=sum(observed[8:10])
observed=observed[1:7]
observed[8]=eormore
expected=rep(NA,8)
expected[1:7] = dpois(0:6,avg)*380
expected[8]= (1 - ppois(6,avg))*380
data.frame(observed,expected)
g2 = 2* sum(observed*log(observed/expected))
g2
x2 = sum((observed-expected)^2/expected)
x2
1 - pchisq(g2,df=8-1-1)
1 - pchisq(x2,df=8-1-1)
