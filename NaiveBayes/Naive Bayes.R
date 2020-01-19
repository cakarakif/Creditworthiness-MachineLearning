# Libraries
library(naivebayes)
library(ggplot2)
library(caret)
library(psych)
library(e1071)

mydata <- read.csv("german_credit.csv")

# Data(Train) # Data(Test)
set.seed(7267166)
trainIndex=createDataPartition(mydata$Creditability, p=0.7)$Resample1
train_sal=mydata[trainIndex, ]
test_sal=mydata[-trainIndex, ]

View(train_sal)
train_sal$Creditability <- as.factor(train_sal$Creditability)
class(train_sal)

View(test_sal)
test_sal$Creditability <- as.factor(test_sal$Creditability)
class(test_sal)



#Naive Bayes
data=naiveBayes(Creditability~Duration.of.Credit..month.+Credit.Amount, data=train)


#Density Plot 

ggplot(data=train_sal,aes(x = train_sal$Duration.of.Credit..month., fill = train_sal$Creditability)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggplot(data=train_sal,aes(x = train_sal$Credit.Amount, fill = train_sal$Creditability)) +
  geom_density(alpha = 0.9, color = 'Violet')


#Visualization 
# Plot and ggplot 
ggplot(data=train_sal,aes(x=train_sal$Duration.of.Credit..month., y = train_sal$Credit.Amount, fill = train_sal$Creditability)) +
  geom_point(size = 1) + 
  geom_polygon(data = train_sal, aes(train_sal$Duration.of.Credit..month.,train_sal$Credit.Amount), alpha = 0.9)