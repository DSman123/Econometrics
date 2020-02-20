# Joel Cabrera
# 171001652
# Section 01
# Econ 322 - Homework 0
data<-read.csv('birthweight.csv', head=T, sep=",")
data2 <- data[data$smoke!="9",]
data2
mean(data2$bwt[data2$smoke=="0"])
mean(data2$bwt[data2$smoke=="1"])
mean(data$weight[data$feed=="casein"])
sd(data$bwt[data$smoke=="0"])/sqrt(length(which(data$smoke=="0")))
sd(data$bwt[data$smoke=="1"])/sqrt(length(which(data$smoke=="1")))
t.test(data2$bwt~data2$smoke, paired=F, var.eq=T)  
dim(data2)
names(data2)
summary(data2)
levels(data2$smoke)
test.stat1 <- abs(mean(data2$bwt[data2$smoke=="0"]) - mean(data2$bwt[data2$smoke=="1"]))
abs( diff( with(data2, tapply(bwt, smoke, mean)) ) )
test.stat1      
123.28-1.96*0.71
123.28+1.96*(0.71)
113.69-1.96*(0.92)
113.69+1.96*(0.92)