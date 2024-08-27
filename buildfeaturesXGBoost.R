rm(list=ls())
library(data.table)
library(caret)
library(glmnet)
library(Metrics)
library(plotmo)
library(lubridate)
library(xgboost)
set.seed(7)

train<-fread("./project/volume/data/raw/Stat_380_train.csv")
test<-fread("./project/volume/data/raw/Stat_380_test.csv")
submit<-fread("./project/volume/data/raw/Example_Sub.csv")
covar<-fread("./project/volume/data/raw/covar_data.csv")

sample_id <- test$sample_id
test$ic50_Omicron <- 0


#drop sample_id
train <- subset(train, select = -c(sample_id))
test <- subset(test, select = -c(sample_id))

train_y<-train$ic50_Omicron

#replace NA values
missing_index <- which(is.na(train), arr.ind = TRUE)
train[missing_index] <- 1

missing_index <- which(is.na(test), arr.ind = TRUE)
test[missing_index] <- 1

#dummies
dummies <- dummyVars(ic50_Omicron ~ ., data = train)
saveRDS(dummies, "./project/volume/models/dummies")
train <- predict(dummies, newdata = train)
test <- predict(dummies, newdata = test)


train <- data.table(train)
test <- data.table(test)
train <- as.matrix(train)
test <- as.matrix(test)





############################
# cross validation #
############################

train <- subset(train, select = -c(dose_3mRNA1272))

param <- list(  objective           = "reg:linear",
                eval_metric         = "rmse",
                alpha               = 1,
                lambda              = 0.1
)


############################
# fit model to all data  #
############################

XGBm<-xgboost(data = train, label = train_y, params = param, nrounds = 100, verbose = 0, early_stopping_rounds = 1000)
pred<-predict(XGBm, newdata = test)


############################
# submission  #
############################

test$ic50_Omicron <- pred
submit <- data.table(sample_id)
submit$ic50_Omicron <- test$ic50_Omicron
fwrite(submit, "./project/volume/data/processed/submit1.csv")
