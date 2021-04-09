install.packages("DataExplorer")
install.packages("tidyverse")
install.packages("mice")
install.packages("caTools")
library(knitr)
library(tidyverse)
library(ggplot2)
library(mice)
library(lattice)
library(reshape2)
library(DataExplorer)
library(dplyr)
library(e1071)
library(caTools)
library(caret)

data= read.csv('give path to your dataset folder')

head(data)
View(data)
str(data)
summary(data)
sum(is.na(data))
colnames(data)
defaultpayment<- data$default.payment.next.month

introduce(data)
count(data, vars = EDUCATION)
# or we can use following to get the count
table(data$EDUCATION)
summary(data$EDUCATION)
summary(data$MARRIAGE)
table(data$MARRIAGE)
data$EDUCATION[data$EDUCATION==0] <- 4
data$EDUCATION[data$EDUCATION==5] <- 4
data$EDUCATION[data$EDUCATION==6] <- 4
data$MARRIAGE[data$MARRIAGE==0] <- 3


plot_correlation(na.omit(data),maxcat=5L)
plot_correlation(data)
attach(data)
skewness(EDUCATION)
skewness(PAY_0)
skewness(PAY_2)
skewness(PAY_3)
skewness(PAY_4)
plot_histogram(data)
new_data<- select(data,-one_of('BILL_AMT1','BILL_AMT2','BILL_AMT3','BILL_AMT4','BILL_AMT5',
                    'BILL_AMT6','ID','AGE')) 
head(new_data)
dim(new_data)
colnames(new_data)
new_data[,1:16]<-scale(new_data[,1:16])

#splitting data: method 1
d1=sort(sample(nrow(new_data),nrow(new_data)*.7))

#splitting data: method 2
d=sample.split(new_data$default.payment.next.month,SplitRatio = 0.7)
dim(d)
t=new_data[d,]
View(t)
dim(t)
train_data=new_data[d1,]
View(train_data)
dim(train_data)

test_data=new_data[-d1,]
dim(test_data)
View(test_data)

log.model <- glm(default.payment.next.month ~.,data=train_data,family=binomial(link = "logit"))

summary(log.model)
anova(log.model,test="Chisq")

View(new_data)
prediction= predict(log.model,test_data,type = "response")
head(prediction,10)

prediction1= ifelse(prediction>0.5,1,0)
head(prediction1,10)

table(test_data$default.payment.next.month)
table(prediction1)
actual=new_data

accuracy=table(prediction1,test_data[,17])
accurate=sum(diag(accuracy))/sum(accuracy)# accuracy of the model is 81.71%

