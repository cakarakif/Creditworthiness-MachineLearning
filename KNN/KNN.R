library(class) # Contains the "knn" function
library(plyr)
library(ggplot2)
library(ISLR)
library(caret)

setwd('C:\\Users\\akifc\\Desktop')

data <- read.csv("german_credit.csv")

##########################-Cross-Validation

#Create 10 equally size folds
folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)

##########################-Find Optimum k Value

testIndexes <- which(folds==1,arr.ind=TRUE)
testData <- data[testIndexes, ]
trainData <- data[-testIndexes, ]
#First try to determine the right K-value  

acc <- numeric() #holding variable  


highest_accuracy <- -99  
highest_k_accuracy <- -1   

for(i in 1:70){  
  predict <- knn(train=trainData, test=testData, cl=trainData$Creditability, k=i)  
  acc <- c(acc, mean(predict==testData$Creditability))  
  if(highest_accuracy < acc[i]){ 
    #To find the value which has the highest accuracy.  
    highest_accuracy <- acc[i]  
    highest_k_accuracy <- i  
  }  
}  

#Plot error rates for k=1 to 30  
plot(1-acc, type="l", ylab="Error Rate",  xlab="K", main="Error Rate for Data with varying K")  

#highest accuracy  
highest_accuracy  

#k value has highest accuracy  
highest_k_accuracy  

##############################################-Apply knn Algorithm || Cross Validation

acc <- numeric() #reset acc list

#Perform 10 fold cross validation
for(i in 1:(10/2)){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- data[testIndexes, ]
  trainData <- data[-testIndexes, ]
  
  predict <- knn(train=trainData, test=testData, cl=trainData$Creditability, k=highest_k_accuracy)  
  acc <- c(acc, mean(predict==testData$Creditability))
}

mean(acc)
summary(predict)
##############################################-Ploting
# First use Convex hull to determine boundary points of each cluster
plot.df1 = data.frame(x = plot.df$Duration.of.Credit..month., 
                      y = plot.df$Credit.Amount, 
                      predicted = plot.df$predicted)

find_hull = function(df) df[chull(df$x, df$y), ]
boundary = ddply(plot.df1, .variables = "Creditability", .fun = find_hull)

ggplot(plot.df, aes(Duration.of.Credit..month., Credit.Amount, color = predicted, fill = predicted)) + 
  geom_point(size = 5) + 
  geom_polygon(data = boundary, aes(x,y), alpha = 0.5)

