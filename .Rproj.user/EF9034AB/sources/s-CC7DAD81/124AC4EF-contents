rm(list=ls())
setwd("C:/Users/LENOVO/Documents/Second_Assignment") #Set working directory to the specified file path

ADCTLTrain <-read.csv("C:/Users/LENOVO/Documents/Second_Assignment/ADCTLtrain.csv")

#Libraries
library(caret)
library(MASS)
library(ggplot2)
library(Boruta)
library(mlbench)
library(caret)
library(randomForest)
library(pROC)
library(ROCR)
library(mltools)


str(ADCTLTrain)

ADCTL <-data.frame(ADCTLTrain[2:431])

str(ADCTL)
data("ADCTL")

# Feature Selection
set.seed(111)

class(ADCTL$Label)  
ADCTL$Label <- as.factor(ADCTL$Label)

boruta <- Boruta(Label ~ ., data = ADCTL, doTrace = 2, maxRuns = 500)
print(boruta)

# Tentative Fix
bor <- TentativeRoughFix(boruta)
print(bor)
attStats(boruta)

# Data Partition
set.seed(222)
ind <- sample(2, nrow(ADCTL), replace = T, prob = c(0.6, 0.4))
train <- ADCTL[ind==1,]
test <- ADCTL[ind==2,]