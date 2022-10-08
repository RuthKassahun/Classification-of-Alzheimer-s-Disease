
setwd("C:/Users/LENOVO/Documents/Second_Assignment") #Set working directory to the specified file path

ADCTLTrain <-read.csv("C:/Users/LENOVO/Documents/Second_Assignment/ADCTLtrain.csv")

library(caret)
library(MASS)
library(ggplot2)

view(ADCTLTrain)
summary(ADCTLTrain)

names(ADCTLTrain)

correlation_matrx <- cor(ADCTLTrain[,2:431])
correlation_matrx

dim(ADCTLTrain)
#pairs(ADCTLTrain)

train_data <-data.frame(ADCTLTrain[2:431])
train <-data.frame(ADCTLTrain[2:431]) 
dim(train)

str(ADCTLTrain) # to see the stracture of the data
train[1:429] <- scale(train[1:429])

#find mean of each predictor variable
apply(train[1:429], 2, mean)

#find standard deviation of each predictor variable
apply(train[1:429], 2, sd) 

#fit LDA model
#model <- lda(Label~., data=train)


# Libraries
library(Boruta)
library(mlbench)
library(caret)
library(randomForest)


str(ADCTLTrain)

# Feature Selection
set.seed(111)
boruta <- Boruta(Label ~ ., data = Sonar, doTrace = 2, maxRuns = 500)
print(boruta)
plot(boruta, las = 2, cex.axis = 0.7)
plotImpHistory(boruta)
