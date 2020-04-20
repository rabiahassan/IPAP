#thesis/ analysis of behvioral data
getwd()
setwd("/Users/Rabia/Desktop/IPAP")
bdata<-read.csv("behaviordata.csv",header=T)
str(bdata)
names(bdata)
summary(bdata)

#age of participants:
install.packages("psych")
library(psych)
describeBy(bdata$age.in.days)
install.packages("pastecs")
library(pastecs)
stat.desc(bdata$age.in.days)


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
table(bdata$Gender,bdata$ID)
table(bdata$Condition)
table(bdata$objects)
table(bdata$object.category)


#mean and sd of latency to touch based on objects and conditions. 
aggregate(latency~object.category*Condition,mean,data=bdata)
aggregate(latency~object.category*Condition,sd,data=bdata)


#barplot: (looks messy and is not giving any clear picture)
summaryplot<-table(bdata$latency,bdata$object.category)
summaryplot

barplot(summaryplot)
barplot(summaryplot, main="latency to touch based on object categories per condition", xlab="object categories", ylab= "latency to touch", col=c("black", "grey"), beside=TRUE)  
legend("topright", legend=c("Social.Information", "No.SocialInformation"), title="condition", fill=c("black", "grey")) 

# plots (looks very messy)
plot(bdata$ID,bdata$latency)

#ggplot:
library(ggplot2)
pgrid <- ggplot(bdata, aes(trial.number, latency)) 
pgrid + geom_point(aes(colour = Condition)) + geom_smooth(aes(colour = Condition), method = "lm", se = F) + facet_wrap(~object.category, ncol = 4) + labs(x = "trial number", y = "latency to touch")
imageDirectory<-paste(Sys.getenv("IPAP analysis"),"/Users/Rabia/Desktop",sep="/")
imageFile <- paste(imageDirectory,"ggplot.png",sep="/")
ggsave(file = imageFile)

#histograms:
hist(bdata$latency)

#bargraphs: 
install.packages("sciplot")
library(sciplot)
bargraph.CI(x.factor=object.category,group=Condition,response=latency,data=bdata,ylim=c(0,12),xlab="Object Categories ",ylab="latency to touch (in secs)",main="Latency",col=c("darkblue","grey"),err.width=0.01)
legend("topright", legend=c("No.Social.Information", "Social.Information"), title="Condition", fill=c("darkblue", "grey")) 

#subsets: 
trial1<-subset(bdata,bdata$trial.number==1)
trial1
hist(trial1$latency)
bargraph.CI(x.factor=object.category,group=Condition,response=latency,data=trial1,ylim=c(0,20),xlab="Object Categories",ylab="latency.to.touch (in secs)",main="Latency Trial 1",col=c("darkblue","grey"),err.width=0.01)
legend("topright", legend=c("No.Social.Information", "Social.Information"), title="Condition", fill=c("darkblue", "lightgrey")) 
#latency to touch is higher for social info condition as compared to no social info condition. 
pgrid <- ggplot(trial1, aes(trial.number, latency)) 
pgrid + geom_point(aes(colour = Condition)) + geom_smooth(aes(colour = Condition), method = "lm", se = F) + facet_wrap(~object.category, ncol = 4) + labs(x = "trial.number", y = "latency to touch")

block1<-subset(bdata,bdata$trial.number==c(1,2,3,4))
block1
hist(block1$latency)
bargraph.CI(x.factor=object.category,group=Condition,response=latency,data=block1,ylim=c(0,20),xlab="Object Categories",ylab="latency.to.touch (in secs)",main="Latency Block 1",col=c("darkblue","grey"),err.width=0.01)
legend("topright", legend=c("No.Social.Information", "Social.Information"), title="Condition", fill=c("darkblue", "grey")) 
#latency to touch is higher for no social info compared to social info condition but compared to trial 1 its lower for block 1. 

block2<-subset(bdata,bdata$trial.number==c(5,6,7,8))
block2
hist(block2$latency)
bargraph.CI(x.factor=object.category,group=Condition,response=latency,data=block2,ylim=c(0,20),xlab="Object Categories",ylab="latency.to.touch (in secs)",main="Latency block 2",col=c("darkblue","grey"),err.width=0.01)
legend("topright", legend=c("No.Social.Information", "Social.Information"), title="condition", fill=c("darkblue", "grey")) 
#latency to touch is lower as compared to block 1. 


#Preparations before running glmm:
#Before running the model, make sure you defined all of your factors as factors!
bdata$ID<-as.factor(bdata$ID)
bdata$object.category<-as.factor(bdata$object.category)
bdata$Condition<-as.factor(bdata$Condition)
bdata$Gender<-as.factor(bdata$Gender)
bdata$trial.number<-as.vector(bdata$trial.number)
bdata$latency<-as.vector(bdata$latency)
bdata$z.latency<-as.vector(scale(bdata$latency)) #dont z transform the latency
str(bdata)
#log transformation of latency : 
bdata$log.latency=log(bdata$latency)

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

RandomFull<-lmer(log.latency~Condition*object.category+trial.number+Gender+(1|ID)+(0+trial.number|ID),data=bdata,control=contr)
summary(RandomFull)
drop1(RandomFull,test="Chisq")
#significant result for trial number 


#main effect of conditions: 
RandomRed<-lmer(log.latency~Condition+object.category+trial.number+Gender+(1|ID)+(0+trial.number|ID),data=bdata,control=contr)
summary(RandomRed)
drop1(RandomRed,test="Chisq")
###significant results for object.category and trial number ###
#main effect of objects
RandomRed1<-lmer(log.latency~object.category+Condition+trial.number+Gender+(1|ID)+(0+trial.number|ID),data=bdata,control=contr)
 summary(RandomRed1)
drop1(RandomRed1,test="Chisq")
###significant results for object.category and trial number ###


#wilcoxon sign ranked test for checking the number of no touch trials per object category:
bdata1<-aggregate(bdata$latency,by=list(bdata$ID,bdata$object.category),FUN=max,na.rm=TRUE)
colnames(bdata1)<-c("ID","object.category","latency.to.touch")
bdata1
install.packages("exactRankTests")
library(exactRankTests)
install.packages("coin")
library(coin)


wilcox.exact(bdata1[bdata1$object.category=="Plants",]$latency,bdata1[bdata1$object.category=="Novel.artifacts",]$latency) 

wilcox.exact(bdata1[bdata1$object.category=="Plants",]$latency,bdata1[bdata1$object.category=="Natural.objects",]$latency) 


#assumptions of GLMM: 
#since our outcome variable(latency to touch) is a continuous variable so we will use Gaussian model and the assumptions of normality and homogenity of residuals applies. 

hist(bdata$latency)

#inspect the data: 
#how many data points per level of factor: 
table(bdata$object.category)
table(bdata$trial.number)
#how many observations per level of random effect (participant):
table(bdata$ID)
#check levels of fixed effect per level of random effect:
table(bdata$ID,bdata$trial.number)
table(bdata$ID,bdata$object.category)
#enough data for every fixed factor and every subject. 

#random slope: 
library(lme4)
FullModel=lmer(latency~Condition+object.category+trial.number+Gender+(1|ID)+(0+trial.number|ID),data=bdata)

#multicollinearity: 
install.packages("carData")
install.packages("car")
library(car)
vif(FullModel)
#all values are around 1 so no issue of multicollinearity 

#normality of residuals:
hist(residuals(RandomFull))  #slightly skewed but looks ok

qqnorm(residuals(FullModel)) #doesnt look great / skewness of dv , higher and lower ends model doesnt fit properly. 
qqline(residuals(FullModel))

#homogeneity of variances: 
plot(x=fitted(RandomFull),y=residuals(RandomFull),pch=19) #terrible 
or 
plot(RandomFull) 

#influential cases: 
source("/Users/rabiahassan/Desktop/glmm_stability.r")
m.stab=glmm.model.stab(model.res=FullModel)
m.stab$summary[,-1] 
#result shows that model is stable enough since there is not much difference between the values of the original coefficients and the estimated minimum and maximum range of the coefficients after excluding each factor of the random effect (ID)


