##THE SPARKS FOUNDATION##
#GRIPMAY21#
#TASK1-predicting scores using supervised learning#
#author:Rajendar reddy s#


#load required libraries & packages


library(caTools)

library(ggplot2)

library(dplyr)

#load given data set from remote source

given_data<-read.csv("https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv")
View(given_data)
summary(given_data)

#plot graph of hours vs scores

plot(given_data$Hours,given_data$Scores,xlab = "no. of hours studied",ylab = "percentagae of score")

#given_data shows positive linear  relation---> check correlation

cor(given_data$Hours,given_data$Scores)

#cor.value = 0.97.. (very  strong correlation)


#split the given_data into "train" and "test"

set.seed(2)
split<-sample.split(given_data$Hours,SplitRatio = 0.7)
train<-subset(given_data,split=="TRUE")
test<-subset(given_data,split=="FALSE")
train
test

#build LINEAR REGRESSION MODEL using train data set

model<-lm(Scores~Hours,data=train)
model
summary(model)

plot(given_data$Hours,given_data$Scores, xlab = 'hours studies', ylab = 'percentage score')
abline(model,col='red')


#prediction

predict_scores<-predict(model,test)
predict_scores

#COMPARISON
df<-cbind(Actual=test$Scores,prediction=predict_scores)
DT::datatable(df)
#finding final score for 9.25 hours#
final_score<-data.frame(Hours=c(9.25))
predict_final_score<-predict(lm(Scores~Hours,data = test),newdata =final_score)
predict_final_score 



