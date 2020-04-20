getwd()
setwd("/Users/rabiahassan/Desktop/IPAP")
qpdata<-read.csv("plantsQ.csv",header=T)
str(qpdata)
names(qpdata)
summary(qpdata)

latency.Plants=rep(NA,nrow(qpdata))
TD.Plants=rep(NA,nrow(qodata))
qpdata=cbind(qpdata,latency.Plants,TD.Plants)

qpdata$latency.Plants<-qpdata$latency.plant1+qpdata$latency.plant2/2
qpdata$TD.Plants<-qpdata$td.plant1+qpdata$td.plant2/2
qpdata

Experience.outdoorplants=rep(NA,nrow(qpdata))
Experience.indoorplants=rep(NA,nrow(qpdata))
eating.from.plants=rep(NA,nrow(qpdata))
prevent.from.eating.plants=rep(NA,nrow(qpdata))
qpdata=cbind(qpdata,Experience.outdoorplants,Experience.indoorplants,eating.from.plants,prevent.from.eating.plants)

qpdata$Experience.indoorplants<-qpdata$X2a+qpdata$X2b+qpdata$X3a+qpdata$X3b+qpdata$X5a+qpdata$X5b

qpdata$Experience.outdoorplants<-qpdata$X1.1a+qpdata$X1.1b+qpdata$X1.2a+qpdata$X1.2b+qpdata$X1.5a+qpdata$X1.5b

qpdata$eating.from.plants<-qpdata$X4a+qpdata$X4b+qpdata$X1.4a+qpdata$X1.4b

qpdata$prevent.from.eating.plants<-qpdata$X6a+qpdata$X6b+qpdata$X1.6a+qpdata$X1.6b

qpdata

#correlation between latency and experience with indoor plants, outdoor plants, eating and preventing from eating from plants: 

cor.test(qpdata$Experience.indoorplants,qpdata$latency.Plants,use="pairwise",method="pearson")

cor.test(qpdata$Experience.outdoorplants,qpdata$latency.Plants,use="pairwise",method="pearson")

cor.test(qpdata$eating.from.plants,qpdata$latency.Plants,use="pairwise",method="pearson")

cor.test(qpdata$prevent.from.eating.plants,qpdata$latency.Plants,use="pairwise",method="pearson")

#correlation between TRIAL DURATION and experience with indoor plants, outdoor plants, eating and preventing from eating from plants: 


cor.test(qpdata$Experience.indoorplants,qpdata$TD.Plants,use="pairwise",method="pearson")

cor.test(qpdata$Experience.outdoorplants,qpdata$TD.Plants,use="pairwise",method="pearson")

cor.test(qpdata$eating.from.plants,qpdata$TD.Plants,use="pairwise",method="pearson")

cor.test(qpdata$prevent.from.eating.plants,qpdata$TD.Plants,use="pairwise",method="pearson")