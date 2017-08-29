# Q1 data:
ANES = read.csv("anes_pilot_2016.csv")
summary(ANES)
View(ANES)
# Q1a
boxplot(ANES$fttrump,ANES$fthrc,ANES$ftsanders, outline=TRUE)
help(boxplot)
# Q1b
trump = ANES$fttrump[ANES$fttrump<=100]
hrc = ANES$fthrc[ANES$fthrc<=100]
sanders = ANES$ftsanders[ANES$ftsanders<=100]
mean(trump)
sd(trump)
mean(hrc)
sd(hrc)
mean(sanders)
sd(sanders)
# Q1c
# i
alpha=0.01
mean(trump)
mean(trump) + qnorm(1-alpha/2)*sd(trump)/sqrt(length(trump))
mean(trump) - qnorm(1-alpha/2)*sd(trump)/sqrt(length(trump))
# ii
mean(hrc)
mean(hrc) + qnorm(1-alpha/2)*sd(hrc)/sqrt(length(hrc))
mean(hrc) - qnorm(1-alpha/2)*sd(hrc)/sqrt(length(hrc))
# iii
mean(sanders)
mean(sanders) + qnorm(1-alpha/2)*sd(sanders)/sqrt(length(sanders))
mean(sanders) - qnorm(1-alpha/2)*sd(sanders)/sqrt(length(sanders))

# Q2 data:
# 61% out of 1025 US adults supported same-sex marriage
# Q2a:
alpha = 0.05
n=1025
y = 1310:1410
plot(y,dbinom(y,1549,0.88), type="h")
xbar = 0.61
sd_error = sqrt(xbar*(1-xbar)/n)
xbar - qnorm(0.975)*sd_error
xbar + qnorm(0.975)*sd_error

# Q2b:
L = 0.02
# L = 2*qnorm(0.975)*sqrt(xbar*(1-xbar)/n)
n1 = ((2*qnorm(0.975)/L)^2)*(xbar*(1-xbar))


# Q3:
# Q3b:
help(pbinom)
#c
1 - pbinom(13,20,0.5)
#d
1 - pbinom(19,20,0.5)

# Q4a:
pbinom(15,100,1/6)
# Q4b:
pbinom(59,1000,32/663)
# Q4c:
# Home team:
2*(1 - pbinom(1149,2215,0.5))
# Away team:
2*(pbinom(1065,2215,0.5))
# Q4d:
1 - pbinom(237,720,1/4)
