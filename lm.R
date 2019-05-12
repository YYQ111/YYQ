setwd("C:/Users/YYQ/Desktop")
library(MASS)
pork<-read.csv("data.csv",skip=0, header = F)
pork<-pork[complete.cases(pork[,2:6]),]#check data set

reg<-glm(as.matrix(pork[,4])~as.matrix(pork[,1:3]))
summary(reg)
reg_step<-stepAIC(reg, direction="both")
summary(reg_step)




pconomy.pr<-princomp(~as.matrix(pork[,1:3]))
variance_explained <- function(plist) {
  rotation <- as.data.frame(plist$rotation)
  variance <- plist$sdev ^ 2  
  data.frame(
    Component = factor(1:ncol(rotation), levels = 1:ncol(rotation)),
    Variance = variance / sum(variance),
    CumulativeVariance = ( cumsum(variance) / sum(variance) )
  )
}

variance_explained(pconomy.pr)


p<-1000000
j<-0#store best i
m<-10000

for (i in 1:55){
  reg<-lm(as.matrix(pork[1:(110-2*i+1),4])~as.matrix(pork[i:(110-i),1:3]))
  m<-sum(abs(predict(reg)-pork[i:(110-i),4]))
  if (m<p){
    j<-i
    p<-m
  }
}
j

p<-1000000
j<-0#store best i
m<-10000

#test first variable
for (i in 1:24){
  reg<-lm(as.matrix(pork[1:(110-2*i+1),4])~as.matrix(pork[i:(110-i),2:3])+as.matrix(pork[1:(110-2*i+1),1]))
  m<-sum(abs(predict(reg)-pork[i:(110-i),4]))
  if (m<p){
    j<-i
    p<-m
  }
}
j

#test second variable
p<-1000000
j<-0#store best i
m<-10000
for (i in 1:24){
  reg<-lm(as.matrix(pork[1:(110-2*i+1),4])~as.matrix(pork[i:(110-i),c(1,3)])+as.matrix(pork[1:(110-2*i+1),2]))
  m<-sum(abs(predict(reg)-pork[i:(110-i),4]))
  if (m<p){
    j<-i
    p<-m
  }
}
j

#test third variable
p<-1000000
j<-0#store best i
m<-10000
for (i in 1:24){
  reg<-lm(as.matrix(pork[1:(110-2*i+1),4])~as.matrix(pork[i:(110-i),c(1,2)])+as.matrix(pork[1:(110-2*i+1),3]))
  m<-sum(abs(predict(reg)-pork[i:(110-i),4]))
  if (m<p){
    j<-i
    p<-m
  }
}
j


