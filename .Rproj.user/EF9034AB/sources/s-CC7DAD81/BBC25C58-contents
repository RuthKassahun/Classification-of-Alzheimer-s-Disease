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
library(dplyr)

str(ADCTLTrain)

ADCTL <-data.frame(ADCTLTrain[2:431])

str(ADCTL)

class(ADCTL)
ADCTL_numeric <- as.numeric(ADCTL)

ADCTL_num <- mutate_all(ADCTL, function(x) as.numeric(x))

ADCTL_PCA <-prcomp(ADCTL_num[], center = TRUE,scale = FALSE)

all(is.finite(ADCTL_num))
