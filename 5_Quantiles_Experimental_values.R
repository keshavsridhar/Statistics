library(rio)
# Q3 a)
qnorm(0.9,mean=63.8,sd=2.9)
# Q3 b)  IQR = q3 - q1
qnorm(0.75,mean=69.2,sd=2.9) - qnorm(0.25,mean=69.2,sd=2.9)
# Q3 c)
qnorm(0.8,mean=69.2,sd=2.9)
qnorm(0.9,69.2,2.9)
pnorm(69.2+3.7165,69.2,2.9) - pnorm(69.2-3.7165,69.2,2.9)
# Q5
pennsylvania = read.table("pennsylvania.txt", header=TRUE)
plot(pennsylvania$Obama/1000,pennsylvania$Clinton/1000,
     xlab="Obama votes(thousands)",
     ylab="Clinton votes(thousands)",
     main="Keshav's graph of the Pennsylvania vote data")
abline(0,1,col="Red")
# Q6
IUsalaries=rio::import("IU_Salary_List-2014-2015.xls")
Names=IUsalaries$Name
Salaries=IUsalaries$Salary
Plan=factor(IUsalaries$Plan)
luen.salary=Salaries[Names=="Luen, B"]
academics=which(Plan=="AC1")
academic.salaries=Salaries[academics]
#Number of academic employees:
n=length(academic.salaries)
#Number of academics who earn more than Dr. Luen
amtl=sum(academic.salaries>luen.salary)
#Percentge of academics who earn more than Dr. Luen:
amtl/n
