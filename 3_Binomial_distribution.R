#50 total voters, 10 voters had fox news as their news source.(19% of total american voters
#had fox news as their news source)
#a
dbinom(10,50,0.19)
pbinom(10,50,0.19) - pbinom(9,50,0.19)
#b
pbinom(5,25,0.40)
#Suppose we select a simple random sample of 25 American voters. What is the probability that at least 5 had Fox News as their main news source?
1 - pbinom(4,25,0.19)

dbinom(7,12,0.5)
#Trosset ch4.5,Ex-10
1 - pbinom(7,12,0.4)
#Trosset ch4.5,Ex-14
#b
1 - pbinom(7,25,0.2)
#c > P(X>=1) = 1- P(X<=0)
q = 1 - pbinom(7,25,0.2)
1 - pbinom(0,20,q)

#6 marbles 1 white rest non white. Draw 100 times with replacement.
#What is the probability of drawing the white marble atleast once?
#P(Drawing white marble atleast once) = P(X>=1) = 1 - P(X<1) = 1 - P(X<=0)
1 - pbinom(0,100,0.16)

#####################
c = read.csv("train_orig.csv")
cd = data.frame(c)
plot(cd$GrLivArea,log(cd$SalePrice), xlab="GrLivArea", ylab="LogSales", main="Area vs Saleprice")
hist(cd$MoSold, xlab="Month#", ylab="Freq#", main="SeasonalSales",col="lightblue")
plot(cd$GrLivArea[cd$SaleCondition=="Partial"],log(cd$SalePrice[cd$SaleCondition=="Partial"]))
summary(cd)
#####################
