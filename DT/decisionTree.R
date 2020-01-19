library(ggplot2) # Data visualization
library(rpart)
library(rpart.plot)
library(rpart)
library(randomForest)

data <- read.csv("german_credit.csv")


################## Set random seed. Don't remove this line.
set.seed(1)

################## Shuffle the dataset; build train and test
n <- nrow(data)
shuffled <- data[sample(n),]
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]

##################Fill in the model that has been learned.
tree <- rpart(Creditability ~ ., data=train, method = "class")


################# cross validation 
################## Set random seed
set.seed(2)

################## Initialize the accs vector
accs <- rep(0,10)
################## dividing  data in 10 times (10 different train and test data) and --
for (i in 1:10) {
  # These indices indicate the interval of the test set
  indices <- (((i-1) * round((1/10)*nrow(shuffled))) + 1):((i*round((1/10) * nrow(shuffled))))
  
  # Exclude them from the train set
  train <- shuffled[-indices,]
  
  # Include them in the test set
  test <- shuffled[indices,]
  
  # A model is learned using each training set
  tree <- rpart(Creditability ~ ., train, method = "class")
  
  # Make a prediction on the test set using tree
  pred <- predict(tree,test,type='class')
  
  # Assign the confusion matrix to conf
  conf <- table(test$Creditability,pred)
  
  # Assign the accuracy of this model to the ith index in accs
  accs[i] <- sum(diag(conf))/sum(conf)
}

################## Print out the mean of accs
mean(accs)
summary(tree)

################### visualization
rpart.plot(tree, extra = 106)