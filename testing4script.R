#testing4
#data
data<-read.csv("formabilityrandom.csv",FALSE,",")
str(data)
#Min-Max Normalization
hist(data$V2)
data$V1<-(data$V1-min(data$V1))/(max(data$V1)-min(data$V1))
data$V2<-(data$V2-min(data$V2))/(max(data$V2)-min(data$V2))
data$V3<-(data$V3-min(data$V3))/(max(data$V3)-min(data$V3))
hist(data$V2)
hist(data$V3)
summary(data)
str(data)

set.seed(222)
ind<-sample(2,nrow(data),replace=TRUE,prob=c(0.7,0.3))
training<-data[ind==1,]
testing<-data[ind==2,]
#neural network
library(neuralnet)
set.seed(333)
n<-neuralnet(V1~V2+V3,data=training,hidden=5,err.fct="ce",linear.output=FALSE, lifesign = 'full', rep=5, algorithm="rprop+",stepmax=100000)
plot(n)
#prediction
output<-compute(n,training[,-1])
#str(output)
#head(output$net.result)

p1<-output$net.result
pred1<-ifelse(p1>0.5,1,0)

summary(data$V1)
tab1<-table(pred1,training$V1)
tab1
accuracy<-sum(diag(tab1))/sum(tab1)
accuracy
#

output<-compute(n,testing[,-1],rep=4)
#str(output)
#head(output$net.result)

p1<-output$net.result
pred1<-ifelse(p1>0.5,1,0)

summary(data$V1)

tab1<-table(pred1,testing$V1)
tab1
accuracy<-sum(diag(tab1))/sum(tab1)
accuracy
#