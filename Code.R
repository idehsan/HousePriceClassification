#Data Importing
library(readxl)
DMPHP <- read_excel("C:/Users/Ehsan Sharifi/Desktop/DMPHP.xlsx")

#PRE PROCESSING:
#Data Transformation: Visualizing the distribution of target variable
histinfo <- hist(DMPHP$SalePrice, xlab = "Sales Price",main = "Distribution of Sales Price",
                 col = "red", breaks = 30 )

#Data Transformation: categorizing Sale Price (Target Varibale)
SaleC <- factor(rep("high",1460), levels = c('low','medium','high','very high'))
for (i in which( DMPHP$SalePrice <= 120000 ) ) { SaleC[i] <- 'low' }
for (i in which( DMPHP$SalePrice > 120000 & DMPHP$SalePrice <= 180000 ) ) { SaleC[i] <- 'medium' }
for (i in which( DMPHP$SalePrice > 180000 & DMPHP$SalePrice <= 260000 ) ) { SaleC[i] <- 'high' }
for (i in which( DMPHP$SalePrice > 260000 ) ) { SaleC[i] <- 'very high' }

#Data Transformation: Choosing Categorical Columns
c_variables <- c('MSSubClass','MSZoning','Street','Alley','LotShape','LandContour',
                 'Utilities','LotConfig','LandSlope','Neighborhood','Condition1',
                 'Condition2','BldgType','HouseStyle','OverallQual','OverallCond',
                 'RoofStyle','RoofMatl','Exterior1st','Exterior2nd','MasVnrType',
                 'ExterQual','ExterCond','Foundation','BsmtQual','BsmtCond',
                 'BsmtExposure','BsmtFinType1','BsmtFinType2','Heating','HeatingQC',
                 'CentralAir','Electrical','KitchenQual','Functional','FireplaceQu',
                 'GarageType','GarageFinish','GarageQual','GarageCond','PavedDrive',
                 'PoolQC','Fence','MiscFeature')

categorical <- subset(DMPHP,select = c_variables)

#Data Transformation: Choosing Nominal Columns
n_variables <- c('LotFrontage','LotArea','MasVnrArea',
                 'BsmtFinSF1','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','1stFlrSF',
                 '2ndFlrSF','LowQualFinSF','GrLivArea','BsmtFullBath','BsmtHalfBath',
                 'FullBath','HalfBath','BedroomAbvGr','KitchenAbvGr','TotRmsAbvGrd',
                 'Fireplaces','GarageCars','GarageArea','WoodDeckSF','OpenPorchSF',
                 'EnclosedPorch','3SsnPorch','ScreenPorch','PoolArea','MiscVal')

nominal <- subset(DMPHP,select = n_variables)

#Data Transformation: Correcting type of columns
for (item in 1:44)  {
   categorical[,item] <- as.factor(as.character(unlist(categorical[,item])))
}

for (item in 1:28)  {
   nominal[,item] <- as.numeric(nominal[,item])
}


#Data Reduction:
useless_columns <- c("Id","MoSold","YrSold","SaleType","SaleCondition","YearBuilt",
                     "YearRemodAdd","GarageYrBlt")


useless_categorical_columns <- c('Street','Alley','Utilities','Condition2','GarageQual',
                                 'GarageCond','RoofMatl','Functional','Heating','PoolQC',
                                 'MiscFeature','PavedDrive','Electrical',
                                 'BsmtFinType1','BsmtFinType2')

#Data Reduction: Becuase of too missing Values(Zero)
length(which(nominal$PoolArea !=0))
length(which(nominal$LowQualFinSF != 0))
length(which(nominal$MiscVal !=0)) 

#Data Reduction: Becuase of Strong Correlation
cor(nominal$GarageCars,nominal$GarageArea)

#Data Reduction: Aggregation of Bathrooms
Bathroom <- nominal$BsmtFullBath + nominal$BsmtHalfBath + nominal$FullBath + nominal$HalfBath

#Data Reduction: Aggregation of Porchs
PorchSF <- nominal$OpenPorchSF + nominal$EnclosedPorch + nominal$`3SsnPorch` + nominal$ScreenPorch

useless_columns_nominal <- c('BsmtFinSF1','BsmtFinSF2','BsmtUnfSF')

#Data Integration
c_clean <- c('MSSubClass','MSZoning','LotShape','LandContour',
             'LotConfig','LandSlope','Neighborhood',
             'Condition1','BldgType','HouseStyle','OverallQual',
             'OverallCond','RoofStyle','Exterior1st',
             'Exterior2nd','MasVnrType','ExterQual','ExterCond',
             'Foundation','BsmtQual','BsmtCond','BsmtExposure',
             'HeatingQC','CentralAir','KitchenQual','FireplaceQu',
             'GarageType','GarageFinish','Fence')

n_clean <- c('LotFrontage','LotArea','MasVnrArea',
             'TotalBsmtSF','1stFlrSF','2ndFlrSF','GrLivArea',
             'BedroomAbvGr','KitchenAbvGr','TotRmsAbvGrd',
             'Fireplaces','GarageCars','WoodDeckSF')

categorical_clean <- subset(categorical,select = c_clean)
nominal_clean <- cbind(subset(nominal,select = n_clean),Bathroom,PorchSF) 

DB <- cbind(categorical_clean, nominal_clean, SaleC)


#Data Cleaning: Column Names Correction
colnames(nominal_clean) <- c('LotFrontage','LotArea','MasVnrArea','TotalBsmtSF','FirstFlrSF','SecondFlrSF',
                             'GrLivArea','BedroomAbvGr','KitchenAbvGr','TotRmsAbvGrd','Fireplaces','GarageCars',
                             'WoodDeckSF','Bathroom','PorchSF')

#Data Cleaning: Na Treatment
library(mice)
library(VIM)
md.pattern(nominal_clean)
impu <- mice(nominal_clean,m=5,maxit = 20,method = 'pmm')

nominal_clean <- complete(impu,1)
md.pattern(nominal_clean)


#Spliting Train and Test data
library(caTools)
set.seed(5)
splitpoint <- sample.split(DB, SplitRatio = 0.75 )
DB_train <- subset(DB, splitpoint == TRUE)
DB_test <- subset(DB, splitpoint == FALSE)


#Modeling:
#Decision Tree
library(rpart)
library(rpart.plot)
library(caret)
model_tree <- rpart(SaleC ~. , data = DB_train )
predict_tree <- predict(model_tree,DB_test,type = 'class')
conf_tree <- table( DB_test$SaleC , predict_tree )
acc_tree <- sum(diag(conf_tree))/sum(conf_tree)

#Naive Bayesian
library(e1071)
model_bayesian <- naiveBayes(SaleC~. , data = DB_train)
predict_bayesian <- predict(model_bayesian, DB_test)
conf_bayesian <- table( DB_test$SaleC , predict_bayesian )
acc_bayesian <- sum(diag(conf_bayesian))/sum(conf_bayesian)


#Proportional Odds Logistic Regression
library(VGAM)
library(MASS)
model_polr <- polr(SaleC ~ LotFrontage + LotArea + MasVnrArea + 
                           TotalBsmtSF + FirstFlrSF + SecondFlrSF + 
                           GrLivArea + BedroomAbvGr + KitchenAbvGr + 
                           TotRmsAbvGrd + Fireplaces + GarageCars + 
                           WoodDeckSF + Bathroom + PorchSF  + LotShape + 
                           LandContour + LotConfig + LandSlope + 
                           BldgType + ExterQual + Foundation + KitchenQual , 
                           data = DB_train)

predict_polr <- predict(model_polr, DB_test)
conf_polr <- table(DB_test$SaleC, predict_polr )
acc_polr <- sum(diag(conf_polr)) / sum(conf_polr)

#K Nearest Neighbor
DB_train_num <- DB_train
DB_test_num <- DB_test
Sa <- DB_train$SaleC
for (item in 1:45)  {
   DB_train_num[,item] <- as.numeric(DB_train[,item])
   DB_test_num[,item] <- as.numeric(DB_test[,item])
}
model_knn <- knn( train = DB_train_num, cl = Sa ,test = DB_test_num, k = 4 )
predict_knn <- model_knn
conf_knn <- table(DB_test$SaleC, predict_knn )
acc_knn <- sum(diag(conf_knn)) / sum(conf_knn)


#Final Result
Algorithm <- c('Decision Tree', 'Naiva Bayesian', 'Proportional Odds Logistic Regression', 'K Nearest Neighbour')
Accuracy <- format(round(c(acc_decision_tree,acc_bayesian,acc_polr,acc_knn)*100, 2), nsmall = 2) 
Final_Result <- data.frame( Algorithm, Accuracy )
Final_Result <- Final_Result[order(Final_Result$Accuracy,decreasing = TRUE),]
Final_Result 
