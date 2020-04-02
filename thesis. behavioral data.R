#thesis/ analysis of behvioral data
getwd()
setwd("/Users/Rabia/Desktop/IPAP analysis")
bdata<-read.csv("thesisbehavioraldata.csv",header=T)
str(bdata)
names(bdata)
summary(bdata)


#age of participants:
install.packages("psych")
library(psych)
describeBy(bdata$age.in.days)

#age: 
mean(bdata$age.in.days)
range(bdata$age.in.days)

#answer: > mean(beh.data$age.in.days)
[1] 340.4
> range(beh.data$age.in.days)
[1] 272 429

#descriptives:
length(bdata$ID)
table(bdata$age.in.days)
table(bdata$Gender)
table(bdata$Condition)
table(bdata$objects)


#mean and sd of latency to touch based on objects and conditions. 
aggregate(latency.to.touch~objects*Condition,mean,data=bdata)
aggregate(latency.to.touch~objects*Condition,sd,data=bdata)

#barplot: (looks messy and is not giving any clear picture)
summaryplot<-table(bdata$latency.to.touch,bdata$objects)
summaryplot

barplot(summaryplot)
barplot(summaryplot, main="latency to touch based on object categories per condition", xlab="object categories", ylab= "latency to touch", col=c("lightblue", "darkblue"), beside=TRUE)  
legend("topright", legend=c("Social.Information", "No.SocialInformation"), title="condition", fill=c("lightblue", "darkblue")) 

# plots (looks very messy)
plot(bdata$ID,bdata$latency.to.touch)

#ggplot:
pgrid <- ggplot(bdata, aes(trial.number, latency.to.touch)) 
pgrid + geom_point(aes(colour = Condition)) + geom_smooth(aes(colour = Condition), method = "lm", se = F) + facet_wrap(~objects, ncol = 8) + labs(x = "trial number", y = "latency to touch")
imageDirectory<-paste(Sys.getenv("IPAP analysis"),"/Users/Rabia/Desktop",sep="/")
imageFile <- paste(imageDirectory,"ggplot.png",sep="/")
ggsave(file = imageFile)

#histograms:
hist(bdata$latency.to.touch)

#bargraphs: 
install.packages("sciplot")
library(sciplot)
bargraph.CI(x.factor=objects,group=Condition,response=latency.to.touch,data=bdata,xlab="objects",ylab="latency.to.touch",main="barplotCI",col=c("lightblue","darkblue"))
legend("topright", legend=c("Social.Information", "No.SocialInformation"), title="condition", fill=c("lightblue", "darkblue")) 

#subsets: 
trial1<-subset(bdata,bdata$trial.number==1)
trial1
hist(trial1$latency.to.touch)
bargraph.CI(x.factor=objects,group=Condition,response=latency.to.touch,data=trial1,xlab="objects",ylab="latency.to.touch",main="barplotCI",col=c("pink","red"))
legend("topright", legend=c("Social.Information", "No.SocialInformation"), title="condition", fill=c("pink", "red")) 
#latency to touch is higher for social info condition as compared to no social info condition. 
pgrid <- ggplot(trial1, aes(ID, latency.to.touch)) 
pgrid + geom_point(aes(colour = Condition)) + geom_smooth(aes(colour = Condition), method = "lm", se = F) + facet_wrap(~objects, ncol = 8) + labs(x = "participants", y = "latency to touch")

block1<-subset(bdata,bdata$trial.number==c(1,2,3,4))
block1
hist(block1$latency.to.touch)
bargraph.CI(x.factor=objects,group=Condition,response=latency.to.touch,data=block1,xlab="objects",ylab="latency.to.touch",main="barplotCI",col=c("grey","red"))
legend("topright", legend=c("Social.Information", "No.SocialInformation"), title="condition", fill=c("grey", "red")) 
#latency to touch is higher for no social info compared to social info condition but compared to trial 1 its lower for block 1. 

block2<-subset(bdata,bdata$trial.number==c(5,6,7,8))
block2
hist(block2$latency.to.touch)
bargraph.CI(x.factor=objects,group=Condition,response=latency.to.touch,data=block2,xlab="objects",ylab="latency.to.touch",main="barplotCI",col=c("blue","red"))
legend("topright", legend=c("Social.Information", "No.SocialInformation"), title="condition", fill=c("blue", "red")) 
#latency to touch is lower as compared to block 1. 


#Preparations before running glmm:
#Before running the model, make sure you defined all of your factors as factors!
bdata$ID<-as.factor(bdata$ID)
bdata$objects<-as.factor(bdata$objects)
bdata$Condition<-as.factor(bdata$Condition)
bdata$Gender<-as.factor(bdata$Gender)
bdata$trial.number<-as.vector(bdata$trial.number)
bdata$latency.to.touch<-as.vector(bdata$latency.to.touch)
str(bdata)

#packages needed:  
install.packages("lme4")
install.packages("Matrix")
install.packages("nlme")
install.packages("ez")
install.packages("ggplot2")
install.packages ("multcomp")
install.packages("nlme")
install.packages("pastecs")
install.packages("reshape")
install.packages("WRS", repos="http://R-Forge. R-project.org") 
library(lme4)
library(nlme)
library(Matrix)
library(ez)
library(ggplot2)
library(multcomp)
library(nlme)
library(pastecs)
library(reshape)
library(WRS) 

contr=lmerControl(optimizer="bobyqa",  optCtrl=list(maxfun=1000000))

#You will then need to add this control to all of your models using lmer( ~ , …, control=contr)
#This code tells R to run more iterations in order to provide a maximum likelihood estimation of the parameters.

RandomFull<-lmer(latency.to.touch~Condition*objects+trial.number+Gender+(1|ID)+(0+trial.number|ID),data=bdata,control=contr)
summary(RandomFull)
drop1(RandomFull,test="Chisq")

#main effect of conditions: 
RandomRed<-lmer(latency.to.touch~Condition+objects+trial.number+Gender+(1|ID)+(0+trial.number|ID),data=bdata,control=contr)
summary(RandomRed)
drop1(RandomRed,test="Chisq")
###significant results for trial.number only ###
 
#assumptions of GLMM: 
#since our outcome variable(latency to touch) is a continuous variable so we will use Gaussian model and the assumptions of normality and homogenity of residuals applies. 

hist(bdata$latency.to.touch)

#inspect the data: 
#how many data points per level of factor: 
table(bdata$objects)
table(bdata$trial.number)
#how many observations per level of random effect (participant):
table(bdata$ID)
#check levels of fixed effect per level of random effect:
table(bdata$ID,bdata$trial.number)

#random slope: 
library(lme4)
RandomSlope=lmer(latency.to.touch~objects+Condition+trial.number+(1|ID)+(0+trial.number|ID),data=bdata)

#multicollinearity: 
install.packages("carData")
library(car)
vif(RandomSlope)
#all values are around 1 so no issue of multicollinearity 

#normality of residuals:
hist(residuals(RandomSlope))  #slightly skewed but looks ok

qqnorm(residuals(RandomSlope))
qqline(residuals(RandomSlope))

#homogeneity of variances: 
plot(x=fitted(RandomSlope),y=residuals(RandomSlope),pch=19)
or 
plot(RandomSlope) 

#influential cases: 
source("/Users/Rabia/Desktop/glmm_stability.r")
m.stab=glmm.model.stab(model.res=RandomSlope)
m.stab$summary[,-1] 
#result shows that model is stable enough since there is not much difference between the values of the original coefficients and the estimated minimum and maximum range of the coefficients after excluding each factor of the random effect (ID)


