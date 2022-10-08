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
plot(boruta, las=2, cex.axis = 0.7)

attStats(boruta)$normHits

# Tentative Fix
bor <- TentativeRoughFix(boruta)
print(bor)
attStats(boruta)



# Data Partition

#ind <- sample(2, nrow(ADCTL), replace = T, prob = c(0.6, 0.4))
#train <- ADCTL[ind==1,]
#test <- ADCTL[ind==2,]

# Data partition
set.seed(222)
partition<-createDataPartition(ADCTL$Label, p =0.7, list= FALSE) 
train<-ADCTL[partition,] 
test<-ADCTL[-partition,] 

#Get none rejected variables
getNonRejectedFormula(boruta)



names(boruta)


ADCTL %% select (Frontal_Med_Orb_R , OFCpost_L , Hippocampus_L , Hippocampus_R , 
                   ParaHippocampal_L , ParaHippocampal_R , Amygdala_L , Amygdala_R , 
                   Occipital_Mid_R , Fusiform_L , Fusiform_R , Angular_R , Caudate_L , 
                   Caudate_R , Thalamus_L , Thalamus_R , Temporal_Pole_Sup_L , 
                   Temporal_Pole_Sup_R , Temporal_Mid_L , Temporal_Mid_R , Temporal_Pole_Mid_L , 
                   Temporal_Pole_Mid_R , Temporal_Inf_L , Temporal_Inf_R , ATP5EP2 , 
                   ATP5I , ATP6V1E1 , C1orf63 , C5orf41 , CEP350 , CETN2 , COX17 , 
                   CXorf26 , DCUN1D1 , DICER1 , DNAJC7 , GIMAP7 , GNL2 , HIST1H2AC , 
                   HSPE1 , ING3 , LAMP2 , LCOR , LDHB , LOC134997 , LOC285900 , 
                   LOC388720 , LOC401206 , LOC440055 , LOC646200 , LOC653658 , 
                   LOC654121 , LOC731365 , LPP , MRPL51 , MRPS21 , NACA , NDUFA1 , 
                   NDUFS5 , PSMA3 , RPA3 , RPL32 , RPL36AL , RPS25 , RPS27A , 
                   SHFM1 , SOD1 , SSBP1 , TCEAL4 , UFC1 , UQCRH , ZMAT2 , Label)

br <- attStats(boruta)
br$normHits
df <- row.names(subset(br,normHits > 0.70))
df

boruta_dataset <- ADCTL[,c(df,"Label")]

################################################################################



################################################################################

# Random Forest Model
set.seed(333)
rf430 <- randomForest(Label~., data = train)



# Prediction & Confusion Matrix - Test
#p <- predict(rf430, test, type="prob", probability =TRUE)
p <- predict(rf430, test,type="response")
#pos <- p[,2]
conf_matrix<-confusionMatrix(p,as.factor(test$Label))
conf_matrix

#change to factor
#class(test$Label) 
#test$Label <- as.numeric(levels(test$Label))[test$Label]

#calculate AUC
library(pROC)
library(ROCR)
library("gplots")
library("mltools")

#roc_object <- roc(test$Label, p)

#rf_p_train <- predict(rf430,test, type="prob", probability =TRUE)[,2]
rf_p_train <- predict(rf430,newdata=test) 
rf_pr_train <- predict(rf430, newdata = test, type="prob")

#confusionMatrix(data = rf_p_train, reference = test$Label, positive = "AD")
#conf_matrix_again

auc(test$Label,rf_pr_train[,"AD"])

mcc_result <- mcc(preds=rf_p_train, actuals=test$Label)
mcc_result

################################################################################

############### Linear Discriminant Analysis (LDA)
library(MASS)
set.seed(333)
lda430 <- lda(Label~., data = train)
plot(lda430)

lda_all <- train(Label~., data = train, method="lda")
lda_all

lda_p_test <- predict(lda_all,newdata=test)
lda_p_test

names(lda_p_test)

lda.class <- lda_p_test$class
#table (lda.class, test)

lda_pr_train <- predict(lda_all, newdata = test, type="prob")


auc(test$Label,lda_pr_train[,"AD"])

mcc_result <- mcc(preds=lda_p_test, actuals=test$Label)
mcc_result

################################################################################
#LDA with boruta
set.seed(123)
boruta_formula <- as.formula("Label ~ OFCpost_L , Hippocampus_L , Hippocampus_R , ParaHippocampal_L , 
                 ParaHippocampal_R , Amygdala_L , Amygdala_R , Fusiform_L , 
                 Fusiform_R , Angular_R , Caudate_L , Caudate_R , Thalamus_L , 
                 Thalamus_R , Temporal_Pole_Sup_L , Temporal_Pole_Sup_R , 
                 Temporal_Mid_L , Temporal_Mid_R , Temporal_Pole_Mid_L , Temporal_Pole_Mid_R , 
                 Temporal_Inf_L , Temporal_Inf_R , ATP5EP2 , ATP5I , ATP6V1E1 , 
                 C1orf63 , C5orf41 , CBX3 , CEP350 , CETN2 , COX17 , CXorf26 , 
                 DCUN1D1 , DICER1 , DNAJC7 , GIMAP7 , GNL2 , HIST1H2AC , HSPE1 , 
                 ING3 , LAMP2 , LCOR , LDHB , LOC134997 , LOC285900 , LOC388621 , 
                 LOC388720 , LOC401206 , LOC646200 , LOC653658 , LOC654121 , 
                 LOC731365 , LPP , MRPL51 , MRPS21 , NACA , NDUFA1 , NDUFS5 , 
                 PSMA3 , RPA3 , RPL32 , RPL36AL , RPS25 , RPS27A , SHFM1 , 
                 SOD1 , SSBP1 , TCEAL4 , UFC1 , UQCRH , ZMAT2")
boruta_formula

ctrl_bootstrap <- trainControl(method = "boot632", number = "50")

lda_bor_boot <- train(Label ~ OFCpost_L + Hippocampus_L + Hippocampus_R + ParaHippocampal_L + 
                        ParaHippocampal_R + Amygdala_L + Amygdala_R + Fusiform_L + 
                        Fusiform_R + Angular_R + Caudate_L + Caudate_R + Thalamus_L + 
                        Thalamus_R + Temporal_Pole_Sup_L + Temporal_Pole_Sup_R + 
                        Temporal_Mid_L + Temporal_Mid_R + Temporal_Pole_Mid_L + Temporal_Pole_Mid_R + 
                        Temporal_Inf_L + Temporal_Inf_R + ATP5EP2 + ATP5I + ATP6V1E1 + 
                        C1orf63 + C5orf41 + CBX3 + CEP350 + CETN2 + COX17 + CXorf26 + 
                        DCUN1D1 + DICER1 + DNAJC7 + GIMAP7 + GNL2 + HIST1H2AC + HSPE1 + 
                        ING3 + LAMP2 + LCOR + LDHB + LOC134997 + LOC285900 + LOC388621 + 
                        LOC388720 + LOC401206 + LOC646200 + LOC653658 + LOC654121 + 
                        LOC731365 + LPP + MRPL51 + MRPS21 + NACA + NDUFA1 + NDUFS5 + 
                        PSMA3 + RPA3 + RPL32 + RPL36AL + RPS25 + RPS27A + SHFM1 + 
                        SOD1 + SSBP1 + TCEAL4 + UFC1 + UQCRH + ZMAT2, data = train, trControl=ctrl_bootstrap, method="lda")
plot(lda_bor)



lda_p_test_bor <- predict(lda_bor,newdata=test)
lda_p_test_bor

names(lda_p_test_bor)


lda_pr_train_bor <- predict(lda_bor, newdata = test, type="prob")

auc(test$Label,lda_pr_train_bor[,"AD"])

mcc_result <- mcc(preds=lda_p_test_bor, actuals=test$Label)
mcc_result

conf_matrix<-confusionMatrix(p,as.factor(test$Label))
conf_matrix


################################################################################
############################Native Bayers################################################
library(caret)
set.seed(444)


class(train) 
train <- data.frame(train)

str(train)
summary(train)

with(train,table(Label))

levels(train$Label)
train_control<- trainControl(method="boot", number=100)

library (e1071)
bayer_all <- naiveBayes(Label~., data = train,trControl=train_control)

bayer_p_test <- predict(bayer_all,newdata=test)
bayer_p_test

bayer_pr_train <- predict(bayer_all, newdata = test, )

auc(test$Label,bayer_pr_train[,"AD"])

mcc_result <- mcc(preds=bayer_p_test, actuals=test$Label)
mcc_result

################################################################################
#KNN

set.seed (555)
tuned<-expand.grid(k=seq(1,100, by=2)) 
train_label <- train(Label~., data=train, method='knn',trControl=trainControl(method= 'repeatedcv', number=10, repeats=5),tuneGrid=tuned)
train_label 

knn_p_test <- predict(train_label,newdata=test)
knn_p_test

Knn_pr_train <- predict(train_label, newdata = test, type="prob")


auc(test$Label,Knn_pr_train[,"AD"])

mcc_result <- mcc(preds=knn_p_test, actuals=test$Label)
mcc_result
############################################################
#Cross validation
library(glmnet)

#10 fold cross validation is used
ctrl<-trainControl(method = "cv", number=10)

#formula_all <- train$Label~.

TuneGrid <- expand.grid(alpha =seq(0,1,.5),
                        lambda =seq(0,.5, by=0.1))
ctrll<-trainControl(method = "cv",
                    number=5,
                    summaryFunction = twoClassSummary,
                    classProbs = TRUE)
lassofit <- train(Label~., data = train, method = "glmnet",
                  trControl = ctrll,
                  tuneGrid = TuneGrid,
                  family="binomial",
                  metric = "ROC")
lassofit

plot(lassofit)



pred_glmnet <- predict.train(object = lassofit, newdata = test)

predicted_glmnet <- predict.train(object = lassofit,newdata = test, type = "prob")

auc(test$Label,predicted_glmnet[,"AD"])

mcc_result <- mcc(preds=pred_glmnet, actuals=test$Label)
mcc_result

lasImp <-varImp(lassofit, scale=FALSE)
lasImp

#Extracting lasso important variables

df_las<- row.names(subset(lasImp$importance, Overall > 1))
df_las

lasson_dataset <- ADCTL[,c(df_las,"Label")]


plot(lasImp)

tuned <-lassofit$bestTune
plot(tuned)

varImpPlot(lassofit )
varImpPlot(lasImp,type=2)

bestlam <- lassofit$lambda
bestlam
lasso.coef <- predict(lassofit, type = "coefficients" )

##################################################################
#Lassso with svm
library(e1071)
library(caret)
TuneGrid <- expand.grid(C = seq(0,1,length =20))
ctrll<-trainControl(method = "cv",
                    number=5,
                    summaryFunction = twoClassSummary,
                    classProbs = TRUE)
lasso_svm <- train(boruta_formula, data = train, 
                  method="svmLinear",
                  trControl = ctrll,
                  tuneGrid = TuneGrid,
                  preProcess=c("center","scale"))
                  #family="binomial",
                  #kernel = "linear"

lasso_svm

plot(lasso_svm)

lasso_svm$bestTune

pred_lasso_svm <- predict(object = lasso_svm, newdata = test)

class(pred_lasso_svm)
#changed<-as.numeric(pred_lasso_svm)

predicted_lasso_svm_prob <- predict(lasso_svm, newdata = test, type = "prob")

auc(test$Label,predicted_lasso_svm_prob[,"AD"])

mcc_result <- mcc(preds=predicted_lasso_svm_prob, actuals=test$Label)
mcc_result




################################################################
# Decision tree

TuneGrid_dt <- expand.grid(maxdepth = 2:10)
dt_fit <- train(Label~., data = train, method = "rpart2",
                  trControl = ctrll,
                  tuneGrid = TuneGrid_dt,
                  metric = "ROC")

dt_fit
plot(dt_fit$finalModel)
text(dt_fit$finalModel)

pred_dt <- predict.train(object = dt_fit, newdata = test)

predicted_dt <- predict.train(object = dt_fit, newdata = test, type = "prob")

auc(test$Label,predicted_dt[,"AD"])

mcc_result <- mcc(preds=pred_dt, actuals=test$Label)
mcc_result
###################################################################
#Boosting (generalized boosting method)
library(gbm)
require(gbm) 

TuneGrid_boos <- expand.grid(interaction.depth = seq(1,3, by= 1),
                             n.trees = seq(100,900, by = 300),
                             shrinkage= c(0.01,0.1),
                             n.minobsinnode = 10)
ctrl_boos <- trainControl(method = "cv",
                          number =5,
                          summaryFunction = twoClassSummary,
                          classProbs = TRUE)

gbmTune <- train(Label~., data = train, method = "gbm",
                trControl = ctrl_boos,
                tuneGrid = TuneGrid_boos,
                metric = "ROC",
                verbose = FALSE)
gbmTune

pred_gbm <- predict.train(object = gbmTune, newdata = test)

predicted_gbm <- predict.train(object = gbmTune, newdata = test, type = "prob")

auc(test$Label,predicted_gbm[,"AD"])

mcc_result <- mcc(preds=pred_gbm, actuals=test$Label)
mcc_result

gbmImp <-varImp(gbmTune, scale = FALSE)
gbmImp
plot(gbmImp)
###############################################################################
#Random Forest
set.seed(158)
TuneGrid_rf <- expand.grid(mtry= c(1,5,20))

rfTune <- train(boruta_formula, data = train, method = "rf",
                 trControl = ctrl_boos,
                 tuneGrid = TuneGrid_rf,
                 metric = "ROC",
                 preProcess=c("center","scale"),
                 verbose = FALSE)

pred_rff <- predict.train(object = rfTune, newdata = test)
predicted_rff<- predict.train(object = rfTune, newdata = test, type = "prob")

auc(test$Label,predicted_rff[,"AD"])

mcc_result <- mcc(preds=pred_rff, actuals=test$Label)
mcc_result


################################################################################
#GLM with bootstrap
set.seed(457)
ctrl_glm <- trainControl(method = "boot",
                         number = 50,
                         classProbs = TRUE,
                         summaryFunction = twoClassSummary)
glm_boot<-caret::train(boruta_formula,
                trControl = ctrl_glm,
                data = train,
                method = "lda",
                family = "bionomial",
                preProcess=c("center","scale"),
                metrics = "ROC")

glm_boot

################################################################################
library(tidyverse)
library(caret)
library(glmnet)

# Dumy code categorical predictor variables
x <- model.matrix(Label~., train)[,-1]
# Convert the outcome (class) to a numerical variable
y <- ifelse(train$Label == "pos", 1, 0)

has_NA = apply(is.na(train), 1, any) #= 1 if any column in that row is NA
X <- x[!has_NA,]
Y<- y[!has_NA,]

glmnet(x, y, family = "binomial", alpha = 1, lambda = NULL)
