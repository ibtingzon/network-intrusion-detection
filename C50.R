#load library
library(C50)

#load data files
trainingData <- read.csv("C:/Users/issa/Dropbox/CS 176/PA2/kddcup_data_10_percent.csv", header=FALSE)
testingData <- read.csv("C:/Users/issa/Dropbox/CS 176/PA2/kddcup_testData_unlabeled_10_percent.csv", header=FALSE)
names <- read.csv("C:/Users/issa/Dropbox/CS 176/PA2/kddcup_names.csv", sep=":", header=FALSE)

#Split data into training set and testing set 80:20
testingSet <- trainingData[395218:494021,]
trainingSet <- trainingData[1:395217,]

#assign column names to data files
colnames(trainingSet) <- names[,1]
colnames(trainingSet)[42] <- "attack_type"
colnames(testingSet) <- names[,1]
colnames(testingSet)[42] <- "attack_type"
colnames(testingData) <- names[,1]

set.seed(1) #makes sure results are reproducible

#build tree model using C5.0 algorithm
treeModel <- C5.0(attack_type ~ ., data = trainingSet) 
#Equivalent to C5.0(x = trainingSet[,-42], y = trainingSet$attack_type)

pred <- predict(treeModel, testingSet)

#boosting aids to increase accuracy of the tree model by adding weak learners 
#such that new learners pick up the slack of old learners
#Reference: http://connor-johnson.com/2014/08/29/decision-trees-in-r-using-the-c50-package/
boostTreeModel <- C5.0(attack_type ~ ., data = trainingSet, trials = 5)
pred <- predict(boostTreeModel, testingSet)
sum(pred == testingSet$attack_type) / length(pred)
summary(boostTreeModel)

sink(file = "boost-decision-tree-model-results")
summary(boostTreeModel)
sink(NULL)

#rule generation
rules <- C5.0(attack_type ~ ., data = trainingSet, rules = TRUE)
summary(rules)

#prediction of attack types on test data
levels(testingData$service) <- trainingSet$service # set factor levels of testingData such that |levels(testingData$service)| = |levels(trainingSet$service)|
boostTreeModelPred <- predict(boostTreeModel, testingData, type = "class")
treeModelPred <- predict(treeModel, testingData, type = "class")
summary(treeModelPred)
summary(boostTreeModelPred)

sink(file = "pred_tree_results")
summary(boostTreeModelPred)
sink(NULL)
