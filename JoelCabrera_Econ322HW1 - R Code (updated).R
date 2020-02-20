# Joel Cabrera
# Econometrics 322 - Homework 1
# Professor Agan
# April 8, 2019

# Part 1: Determinants of Weight
#1 
njdata<-read.csv('NJHealth1000.csv', head=T, sep=",")
njdata$agesq=njdata$age*njdata$age
age2 <- njdata$agesq
njdata$sex.f<-factor(njdata$sex)
sex.f <- njdata$sex.f
options(scipen = 999)

#Check
njdata$sex.f
njdata$agesq

#2
njreg <-lm(weight ~ age + agesq + sex.f + middleincome + highincome + married + height, data=njdata)
summary(njreg)
#install.packages("sandwich")
#install.packages("lmtest")
#library(lmtest)
#library(sandwich)
coeftest(njreg, vcov=vcovHC(njreg))

# Checking w/ plot
install.packages("ggplot2")  #install a new package ggplot
library("ggplot2") #load the ggplot2 package
ggplot(njdata,(aes(x=age,y=weight)))+ geom_point() +  #simple scatter plot
  scale_y_continuous(labels=comma)+                 # dont use sci notation for wage
  scale_x_continuous(breaks=seq(20,100,10))    #make axis easier to read

#3
#Joint Hypothesis Testing
njreg <-lm(weight ~ age + agesq + sex.f + middleincome + highincome + married + height, data=njdata)
summary(njreg)
coeftest(njreg, vcov=vcovHC(njreg))
linearHypothesis(njreg,c("age", "agesq"))
#linearHypothesis(njreg,c("sex.f = 1"))
#linearHypothesis(njreg,c("sex.f = 2"))
#linearHypothesis(njreg,c("sex.f = 1", "sex.f = 2"))
t.test(njdata$weight~sex.f, paired=F, var.eq=T) #reject

confint(njreg) #married -> (-10.05, -1.04)

#njreftest <-lm(weight ~ married, data=njdata)
#summary(njreftest)
confint(njreftest) #Q: What makes thiss different from confint(njreg)?

linearHypothesis(njreg,c("middleincome", "highincome"))
linearHypothesis(njreg,c("middleincome"))
linearHypothesis(njreg,c("highincome"))
linearHypothesis(njreg,c("middleincome=highincome"))

#coef(njreg)
#t.test(njdata$weight~njdata$middleincome, paired=F, var.eq=T)  
#t.test(njdata$weight~njdata$highincome, paired=F, var.eq=T) #reject
#t.test(njdata$weight~njdata$married, paired=F, var.eq=T) #married -> (-5.66, 4.32) ##Different 95% CI, why?
linearHypothesis(njreg,c("height"))

#4
njreg2 <-lm(weight ~ age + agesq + sex.f + middleincome + highincome + married + height + race + educ, data=njdata)
summary(njreg2)
coeftest(njreg2, vcov=vcovHC(njreg2))
#linearHypothesis(njreg2,c("race"="weight"))
#linearHypothesis(njreg2,c("weight"="race"))
#linearHypothesis(njreg2,c("race","weight"))
#linearHypothesis(njreg2,c("weight","race"))
linearHypothesis(njreg2,c("race=0"))
#t.test(njdata$weight~njdata$race, paired=F, var.eq=T) #ignore
linearHypothesis(njreg2,c("educ=0"))
#t.test(njdata$weight~njdata$educ, paired=F, var.eq=T) #ignore
confint(njreg2)


#5 (same as above, but omitting height)
njreg3 <-lm(weight ~ age + agesq + sex.f + middleincome + highincome + married + race + educ, data=njdata)
summary(njreg3)
coeftest(njreg3, vcov=vcovHC(njreg3))
coef(njreg3)

#Part 2: Own Adventure
#1
#IV = income is less than $15,000 (lowincome)
#DV = Aver. # of alcoholic drinks someone has (avgDrinksPerDay)
regown <-lm(avgDrinksPerDay ~ lowincome, data=njdata)
summary(regown)
coeftest(regown, vcov=vcovHC(regown))
confint(regown)

#2
regown2 <-lm(avgDrinksPerDay ~ lowincome + married, data=njdata)
summary(regown2)
coeftest(regown2, vcov=vcovHC(regown2))
confint(regown2)
#linearHypothesis(njreg2,c("lowincome", "married"))
