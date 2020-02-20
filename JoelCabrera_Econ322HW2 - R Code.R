# Joel Cabrera
# Econ 322 Homework 2
# Professor Agan
# May 2, 2019

# Reading Datasets
data1 <-read.csv('votingdata.csv', head=T, sep=",")
data2 <-read.csv('Suicide.csv', head=T, sep=",")

# Part 1: Presidential Voting
#1
summary(data1$family_income)
data1$high_income <- ifelse(data1$family_income == 4 | data1$family_income == 5, 1, 0)
data1$high_income
# Attempted alternative way, ignore; will use for future reference
#data1$family_income <- NA
#data1$family_income[data1$family_income == "4" | data1$family_income == "5" ] <- "1"
#data1$family_income[data1$family_income == "1" | data1$family_income == "2" | data1$family_income == "3"] <- "0"
#data1$family_income

#2
myprobit1 <- glm(rvote ~ high_income, family = binomial(link = "probit"), data = data1)
summary(myprobit1)
#install.packages("mfx")
library(mfx)
probitmfx(rvote ~ high_income, data=data1)

#3
myprobit2 <- glm(rvote ~ high_income + age + female + black + white, family = binomial(link = "probit"), data = data1)
summary(myprobit2)
probitmfx(rvote ~ high_income + age + female + black + white, data=data1)

#4
myprobit3 <- glm(rvote ~ high_income + age + female + black + white + college, family = binomial(link = "probit"), data = data1)
summary(myprobit3)
probitmfx(rvote ~ high_income + age + female + black + white + college, data=data1)

# Part 2: Deteriminants of the State Suicide Rate
#1
summary(data2)
reg1<-lm(adultsuicrate ~ unemp + rinc + pctrural + college + unins, data=data2)
summary(reg1)

#2
#install.packages("plm") 
library(plm)
reg2 <- plm(adultsuicrate ~ unemp + rinc + pctrural + college + unins, data=data2, index=c("state", "year"), effect="twoway", model="within")
summary(reg2)

#3
reg3 <- plm(adultsuicrate ~ unemp + rinc + pctrural + college + unins + mormon + sobapt + catholic + protestant, data=data2, index=c("state", "year"), effect="twoway", model="within")
summary(reg3)
library("car")
linearHypothesis(reg3, c("mormon=0", "sobapt=0", "catholic=0", "protestant=0"), test="F")
#summary(reg3)$fstatistic

#4
reg4 <- plm(adultsuicrate ~ unemp + rinc + pctrural + college + unins + mormon + sobapt + catholic + protestant + blackper, data=data2, index=c("state", "year"), effect="twoway", model="within")
summary(reg4)

#5 (Extra Credit)
reg3 <- plm(adultsuicrate ~ unemp + rinc + pctrural + college + unins + mormon + sobapt + catholic + protestant, data=data2, index=c("state", "year"), effect="twoway", model="within")
summary(reg3)
true <- coeftest(reg3, vcov=vcovHC(reg3,type="HC0",cluster="group")) #clustered standard errors
summary(true)