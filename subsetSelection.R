rm(list=ls())

setwd("C:/Users/LENOVO/Documents/Second_Assignment") #Set working directory to the specified file path

ADCTLTrain <-read.csv("C:/Users/LENOVO/Documents/Second_Assignment/ADCTLtrain.csv")

ADCTL <-data.frame(ADCTLTrain[2:431])
View(ADCTL)
names(ADCTL)
dim(ADCTL)

ADCTL <- na.omit(ADCTL)

sum(is.na(ADCTL))

#


library(leaps)
#subsetSelected <- regsubsets(Label~., data=ADCTL)
#subsetSelected <- regsubsets(Label~., data=ADCTL, nbest = 5, method="exhaustive", nvmax = 60)

