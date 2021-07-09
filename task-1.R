rm(list=ls())

#Task 1
#Data Science & Business Analytics Internship
#In this task, we have to perform Simple linear Regression analysis and predict the percentage of students on

#packages
install.package(ggplot2)
library(ggplot2)


#reading csv datafile
data<-read.csv("C:\\Users\\Taiyaba\\Desktop\\TSF\\Book1.csv")
attach(data)

#Getting view of dataset
View(data)

#Summary of data
summary(data)

#displaying the internal structure of dataset
str(data)

#correlation
corr=cor(data)
corr

#command for linear regression model
model<-lm(Scores~Hours)
model
summary(model)
anova(model)

coeff = model$coef
est.Scores = coeff[1]+coeff[2]*Hours

#Regression equation
eq=paste0("Scores= ",round(coeff[2],4),"*Hours+",round(coeff[1],4))
print(eq)

#method-1
#when Hours is 9.25
Hours=9.25
est.Scores1 = coeff[1]+coeff[2]*Hours
est.Scores1

#method-2
#prediction when Hours = 9.25
tset<-data.frame(Hours=c(9.25))
Scores_pred= predict(model,newdata=tset,interval='confidence')
Scores_pred

ds<-data.frame(data,est.Scores)
print(ds)

#visualizing data
p1<-ggplot(data=data,aes(x=Hours,y=Scores))+geom_point(alpha=0.5,color="blue")+ggtitle(eq)
p1<-p1+geom_abline(intercept=2.484,slope=9.776,color="red")
#p1<-p1+geom_line(aes(y=est.Scores,color="trendline"))
p1<-p1+theme_bw()
p1<-p1+labs(colour="Trendline")
p1<-p1+theme(legend.position="right")
p1



