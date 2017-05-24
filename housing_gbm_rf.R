housing.train<-read.csv(file = "train.csv")
housing.test<-read.csv(file = "test.csv")
housing.test$SalePrice<-NA
housing.full<-rbind(housing.train,housing.test)
housing.full$MSZoning[is.na(housing.full$MSZoning)]<-'RL'
housing.full$LotFrontage[is.na(housing.full$LotFrontage)] <- mean(housing.full$LotFrontage[!is.na(housing.full$LotFrontage)])
housing.full$Alley <- as.character(housing.full$Alley)
housing.full$Alley[is.na(housing.full$Alley)] <- "No"
housing.full$Utilities[is.na(housing.full$Utilities)]<-'AllPub'
housing.full$Exterior1st[is.na(housing.full$Exterior1st)]<-'VinylSd'
housing.full$Exterior2nd[is.na(housing.full$Exterior2nd)]<-'VinylSd'
housing.full$MasVnrType[is.na(housing.full$MasVnrType)]<-'None'
housing.full$MasVnrArea[is.na(housing.full$MasVnrArea)] <- mean(housing.full$MasVnrArea[!is.na(housing.full$MasVnrArea)])
housing.full$BsmtQual <- as.character(housing.full$BsmtQual)
housing.full$BsmtQual[is.na(housing.full$BsmtQual)] <- "No"
housing.full$BsmtCond <- as.character(housing.full$BsmtCond)
housing.full$BsmtCond[is.na(housing.full$BsmtCond)] <- "No"
housing.full$BsmtExposure <- as.character(housing.full$BsmtExposure)
housing.full$BsmtExposure[is.na(housing.full$BsmtExposure)] <- "No"
housing.full$BsmtFinType1 <- as.character(housing.full$BsmtFinType1)
housing.full$BsmtFinType1[is.na(housing.full$BsmtFinType1)] <- "No"
housing.full$BsmtFinType2 <- as.character(housing.full$BsmtFinType2)
housing.full$BsmtFinType2[is.na(housing.full$BsmtFinType2)] <- "No"
housing.full$BsmtFinSF1[is.na(housing.full$BsmtFinSF1)] <- 0
housing.full$BsmtFinSF2[is.na(housing.full$BsmtFinSF2)] <- 0
housing.full$BsmtUnfSF[is.na(housing.full$BsmtUnfSF)] <- 0
housing.full$TotalBsmtSF[is.na(housing.full$TotalBsmtSF)] <- 0
housing.full$Electrical[is.na(housing.full$Electrical)]<-'SBrkr'
housing.full$BsmtFullBath[is.na(housing.full$BsmtFullBath)] <- 0
housing.full$BsmtHalfBath[is.na(housing.full$BsmtHalfBath)] <- 0
housing.full$KitchenQual[is.na(housing.full$KitchenQual)] <- 'TA'
housing.full$Functional[is.na(housing.full$Functional)] <- 'Typ'
housing.full$FireplaceQu <- as.character(housing.full$FireplaceQu)
housing.full$FireplaceQu[is.na(housing.full$FireplaceQu)] <- "No"
housing.full$GarageType <- as.character(housing.full$GarageType)
housing.full$GarageType[is.na(housing.full$GarageType)] <- "No"
housing.full$GarageYrBlt[is.na(housing.full$GarageYrBlt)] <- housing.full$YearBuilt[is.na(housing.full$GarageYrBlt)]
housing.full$GarageFinish <- as.character(housing.full$GarageFinish)
housing.full$GarageFinish[is.na(housing.full$GarageFinish)] <- "No"
housing.full$GarageCars[is.na(housing.full$GarageCars)] <- 0
housing.full$GarageArea[is.na(housing.full$GarageArea)] <- 0
housing.full$GarageQual <- as.character(housing.full$GarageQual)
housing.full$GarageQual[is.na(housing.full$GarageQual)] <- "No"
housing.full$GarageCond <- as.character(housing.full$GarageCond)
housing.full$GarageCond[is.na(housing.full$GarageCond)] <- "No"
housing.full$PoolQC <- as.character(housing.full$PoolQC)
housing.full$PoolQC[is.na(housing.full$PoolQC)] <- "No"
housing.full$Fence <- as.character(housing.full$Fence)
housing.full$Fence[is.na(housing.full$Fence)] <- "No"
housing.full$MiscFeature <- as.character(housing.full$MiscFeature)
housing.full$MiscFeature[is.na(housing.full$MiscFeature)] <- "No"
housing.full$SaleType[is.na(housing.full$SaleType)] <- "WD"

housing.full$Alley <- as.factor(housing.full$Alley)
housing.full$BsmtQual <- as.factor(housing.full$BsmtQual)
housing.full$BsmtCond <- as.factor(housing.full$BsmtCond)
housing.full$BsmtExposure <- as.factor(housing.full$BsmtExposure)
housing.full$BsmtFinType1 <- as.factor(housing.full$BsmtFinType1)
housing.full$BsmtFinType2 <- as.factor(housing.full$BsmtFinType2)
housing.full$FireplaceQu <- as.factor(housing.full$FireplaceQu)
housing.full$GarageType <- as.factor(housing.full$GarageType)
housing.full$GarageFinish <- as.factor(housing.full$GarageFinish)
housing.full$GarageQual <- as.factor(housing.full$GarageQual)
housing.full$GarageCond <- as.factor(housing.full$GarageCond)
housing.full$PoolQC <- as.factor(housing.full$PoolQC)
housing.full$Fence <- as.factor(housing.full$Fence)
housing.full$MiscFeature <- as.factor(housing.full$MiscFeature)

housing.train <- housing.full[1:1460,]
housing.test <- housing.full[1461:2919,]
housing.train<- housing.train[,-1]
housing.test<- housing.test[,-1]
install.packages("gbm")
library(gbm)
#housing.train$SalePrice = as.integer(housing.train$SalePrice)
model <- gbm(SalePrice ~., data = housing.train, distribution = "gaussian",
             shrinkage = 0.05,
             interaction.depth = 5,
             bag.fraction = 0.66,
             n.minobsinnode = 1,
             cv.folds = 100,
             keep.data = F,
             verbose = F,
             n.trees = 1500)
SalePrice <- predict(model,newdata = housing.test , n.trees = 1500)
id<- housing.test$Id
output.df<-as.data.frame(id)
output.df$SalePrice <- SalePrice
write.csv(output.df,file = "Solution2.csv",row.names = FALSE)
housing.train<- housing.train[,-Id]
housing.test<- housing.test[,-Id]

housing.model<- randomForest(SalePrice~.,data = housing.train)
SalePrice <- predict(housing.model,newdata = housing.test)
output.df$SalePrice <- SalePrice
write.csv(output.df,file = "Solution1.csv",row.names = FALSE)
# try 1
load.libraries <- c('data.table', 'testthat', 'gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071', 'dplyr')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)

cat_var <- names(housing.train)[which(sapply(housing.train, is.character))]
cat_car <- c(cat_var, 'BedroomAbvGr', 'HalfBath', ' KitchenAbvGr','BsmtFullBath', 'BsmtHalfBath', 'MSSubClass')
numeric_var <- names(housing.train)[which(sapply(housing.train, is.numeric))]

housing.train[,(cat_var) := lapply(.SD, as.factor), .SDcols = cat_var]

correlations <- cor(na.omit(train_cont[,-1, with = FALSE]))

# correlations
row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)

correlations<- correlations[row_indic ,row_indic ]
corrplot(correlations, method="square")








nums<- sapply(housing.train,is.numeric)
num <- housing.train[,nums]
corr <- cor(num[,-1])
install.packages("corrplot")
library(corrplot)
corrplot(corr)

data1<- lapply(housing.train[,!is.numeric()],as.numeric)






trainData<- as.matrix(housing.train, rownames.force=NA)
testData<- as.matrix(housing.test, rownames.force=NA)
install.packages("xgboost")
library(xgboost)
trainD <- xgb.DMatrix(data = trainData, label = trainData[,"SalePrice"])

newmod<-xgboost(data = trainData, 
                booster = "gbtree", 
                objective = "reg:linear", 
                max.depth = 5, 
                eta = 0.5, 
                nthread = 2, 
                nround = 2, ;l
                min_child_weight = 1, 
                subsample = 0.5, 
                colsample_bytree = 1, 
                num_parallel_tree = 1)