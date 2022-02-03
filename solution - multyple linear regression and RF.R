# Random Forest Regression

# Importing the dataset
dataset = read.csv('train.csv')
x = read.csv('python_to_R.csv')
y= dataset$SalePrice
x_test = read.csv('python_R_test')
x[, 'SalePrice']<-y
# Splitting the dataset into the Training set and Test set
# # install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$Salary, SplitRatio = 2/3)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Random Forest Regression to the dataset
# install.packages('randomForest')
library(randomForest)
set.seed(1234)
regressor = lm(formula = SalePrice ~ .,
               data = x)
summary(regressor)

x <- x[,-93]  
regressor = lm(formula = SalePrice ~ .,
               data = x)
summary(regressor)

x <- x[,-63] 
regressor = lm(formula = SalePrice ~ .,
               data = x)
summary(regressor)

x <- x[,-58]
regressor = lm(formula = SalePrice ~ .,
               data = x)
summary(regressor)

x <- x[,-26]
regressor = lm(formula = SalePrice ~ .,
               data = x)
summary(regressor)

x <- x[,-1]
regressor = lm(formula = SalePrice ~ .,
               data = x)
summary(regressor)

cols.dont.want <- "X34"
x <- x[, ! names(x) %in% cols.dont.want, drop = F]
regressor = lm(formula = SalePrice ~ .,
               data = x)
summary(regressor)


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

SL = 0.001

backwardElimination(x, SL)
# Predicting a new result with Random Forest Regression
y_pred = predict(regressor, data.frame(x_test))

#write.csv(y_pred,"prediction_with_R_linearegression2.csv", row.names = FALSE)


library(randomForest)

regressor_rf = randomForest(x = x[-99],
                         y = x$SalePrice,
                         ntree = 600 )


print(regressor_rf)
y_pred_RF = predict(regressor_rf, x_test)
print(round(importance(regressor_rf), 2))
plot(x=y_pred_RF,y=y_pred,
xlab='pred_rf',
ylab='pred mlr')

write.csv(y_pred_RF,"prediction_with_R_RF4.csv", row.names = FALSE)
