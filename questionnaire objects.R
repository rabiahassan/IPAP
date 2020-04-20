
getwd()
setwd("/Users/rabiahassan/Desktop/IPAP")
qodata<-read.csv("objectsQ.csv",header=T)
str(qodata)
names(qodata)
summary(qodata)

#correlation between children's interaction with these objects and latency

cor.test(qodata$X2a,qodata$latency.shoe, method=c("spearman"))

cor.test(qodata$X2b,qodata$latency.stationeryholder, method=c("spearman"))

cor.test(qodata$X2c,qodata$latency.geode, method=c("spearman"))

cor.test(qodata$X2d,qodata$latency.seashell, method=c("spearman"))

#correlation between parents's interactions with these objects and latency:

cor.test(qodata$X3a,qodata$latency.shoe, method=c("spearman"))

cor.test(qodata$X3b,qodata$latency.stationeryholder, method=c("spearman"))

cor.test(qodata$X3c,qodata$latency.geode, method=c("spearman"))

cor.test(qodata$X3d,qodata$latency.seashell, method=c("spearman"))


#correlation between stoping children from touching these objects and latency
cor.test(qodata$X4a,qodata$latency.shoe, method=c("pearson"))

cor.test(qodata$X4b,qodata$latency.stationeryholder, method=c("pearson"))

cor.test(qodata$X4c,qodata$latency.geode, method=c("pearson"))

cor.test(qodata$X4d,qodata$latency.seashell, method=c("pearson"))

#combining objects into main categries
latency.Familiarobjects=rep(NA,nrow(qodata))
latency.Naturalobjects=rep(NA,nrow(qodata))
qodata=cbind(qodata,latency.Familiarobjects,latency.Naturalobjects)

qodata$latency.Familiarobjects<-qodata$latency.shoe+qodata$latency.stationeryholder/2
qodata$latency.Naturalobjects<-qodata$latency.geode+qodata$latency.seashell/2

qodata

X1FO=rep(NA,nrow(qodata))
X1NO=rep(NA,nrow(qodata))
X2FO=rep(NA,nrow(qodata))
X2NO=rep(NA,nrow(qodata))
X3FO=rep(NA,nrow(qodata))
X3NO=rep(NA,nrow(qodata))
X4FO=rep(NA,nrow(qodata))
X4NO=rep(NA,nrow(qodata))
qodata=cbind(qodata,X1FO,X1NO,X2FO,X2NO,X3FO,X3NO,X4FO,X4NO)

qodata$X2FO<-qodata$X2a+qodata$X2b/2
qodata$X3FO<-qodata$X3a+qodata$X3b/2
qodata$X4FO<-qodata$X4a+qodata$X4b/2

qodata$X2NO<-qodata$X2c+qodata$X2d/2
qodata$X3NO<-qodata$X3c+qodata$X3d/2
qodata$X4NO<-qodata$X4c+qodata$X4d/2

qodata

#correlation between children's interaction with familiar objects, parents's intereaction with familiar objects , parents stoping children from touching familiar objects and latency
cor.test(qodata$X2FO,qodata$latency.Familiarobjects,use="pairwise",method="pearson")
cor.test(qodata$X3FO,qodata$latency.Familiarobjects,use="pairwise",method="pearson")
cor.test(qodata$X4FO,qodata$latency.Familiarobjects,use="pairwise",method="pearson")

#correlation between children's interaction with natural objects, parents's intereaction with natural objects , parents stoping children from touching natural objects and latency
cor.test(qodata$X2NO,qodata$latency.Naturalobjects,use="pairwise",method="pearson")
cor.test(qodata$X3NO,qodata$latency.Naturalobjects,use="pairwise",method="pearson")
cor.test(qodata$X4NO,qodata$latency.Naturalobjects,use="pairwise",method="pearson")

#trial duration:
TD.Familiarobjects=rep(NA,nrow(qodata))
TD.Naturalobjects=rep(NA,nrow(qodata))
qodata=cbind(qodata,TD.Familiarobjects,TD.Naturalobjects)
qodata

#correlation between children's interaction with familiar objects, parents's intereaction with familiar objects , parents stoping children from touching familiar objects and trial duration
cor.test(qodata$X2FO,qodata$TD.Familiarobjects,use="pairwise",method="spearman")
cor.test(qodata$X3FO,qodata$TD.Familiarobjects,use="pairwise",method="pearson")
cor.test(qodata$X4FO,qodata$TD.Familiarobjects,use="pairwise",method="pearson")

#correlation between children's interaction with natural objects, parents's intereaction with natural objects , parents stoping children from touching natural objects and latency
cor.test(qodata$X2NO,qodata$TD.Naturalobjects,use="pairwise",method="pearson")
cor.test(qodata$X3NO,qodata$TD.Naturalobjects,use="pairwise",method="pearson")
cor.test(qodata$X4NO,qodata$TD.Naturalobjects,use="pairwise",method="pearson")

#output/results example: Output 6.5 shows the output for a Spearman correlation on the variables Creativity and Position. The output is very similar to that of the Pearson correlation (except that confidence intervals are not produced – if you want one see the section on bootstrapping): the correla- tion coefficient between the two variables is fairly large (−.373), and the significance value of this coefficient is very small (p < .001). The significance value for this correlation coefficient is less than .05; therefore, it can be concluded that there is a significant relationship between creativity scores and how well someone did in the World’s Biggest Liar competition. Note that the relationship is negative: as creativity increased, position decreased. Remember that a low number means that you did well in the competition (a low number such as 1 means you came first, and a high number like 4 means you came fourth). Therefore, our hypothesis is supported: as creativity increased, so did success in the competition. 


