# Multiple Linear Regression

# Importing the dataset
train = read.csv('train_cle.csv')
test = read.csv('test_cle.csv')
train=train[,3:81]
test=test[,3:80]

# Encoding categorical data train(all categorical columns in a list)
for (column in c('MSZoning','Street','LotShape','LandContour','Utilities','LotConfig',
                 'LandSlope','Neighborhood','Condition1','Condition2','BldgType','HouseStyle',
                 'RoofStyle','RoofMatl','Exterior1st','Exterior2nd','MasVnrType','ExterQual','ExterCond',
                 'Foundation','BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinType2',
                 'Heating','HeatingQC','CentralAir','Electrical','KitchenQual','Functional','PoolQC','FireplaceQu',
                 'Fence','MiscFeature','SaleType','SaleCondition','GarageType','GarageFinish','GarageQual','GarageCond','PavedDrive')){
  categories <- unique(train[[column]]) 
  train[[column]] = factor(train[[column]],
                        levels = categories,
                        labels = c(categories))

  # Encoding categorical data test(all categorical columns in a list)
  categories <- unique(test[[column]]) 
  test[[column]] = factor(test[[column]],
                          levels = categories,
                          labels = c(categories))
}



#taking care on missing data(check if i missed some values)
colSums(is.na(train))


# install.packages('caTools')
library(caTools)
set.seed(123)


# Feature Scaling
for (column in c('MSSubClass','LotFrontage','LotArea','OverallQual','OverallCond','YearBuilt','YearRemodAdd','MasVnrArea',
                 'BsmtFinSF1','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','X1stFlrSF','X2ndFlrSF','LowQualFinSF','GrLivArea',
                 'GarageYrBlt','GarageArea','WoodDeckSF','OpenPorchSF','EnclosedPorch','X3SsnPorch','ScreenPorch','PoolArea',
                 'MiscVal','MoSold','YrSold')){
  train[column]=scale(train[column])
  test[column]=scale(test[column])
  
  
  
}
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = SalePrice ~ .,
               data = train)

#backwarde elimination

backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = SalePrice ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.1
backwardElimination(train, SL)
plot(regressor)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test)
plot(y_pred,test[,1])
y_pred
#?????????? ??????????, ???????? ???? ???? ?????????? ???????? ???????????????? ?????? ????????