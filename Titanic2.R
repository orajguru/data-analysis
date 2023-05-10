library(data.table)
library(lattice)
library(caret)
library(ggplot2)
library(mice)
library(GGally)
library(rattle)
library(ElemStatLearn)
#set.seed(1234) reason for getting different outputs
set.seed(123)
data = fread("/Users/Documents/titanic_ML.csv")
imputed_data = mice(data)
data = complete(imputed_data, 1)
data2 = data[, c("Age","Fare","Survived")]

data2$Survived = as.factor(data2$Survived)
data2$Age = scale(data2$Age)
data2$Fare = scale(data2$Fare)
training = createDataPartition(y = data2$Survived, p = 0.60, list = FALSE)
train_set = data2[training, ]
test_set = data2[-training, ]

#Bagging
model_fitting_bagging = train(data = train_set, Survived~., method = "treebag")
model_fitting_bagging$finalModel
summary(model_fitting_bagging)

confusionMatrix(predict(model_fitting_bagging, newdata = train_set), train_set$Survived)
confusionMatrix(predict(model_fitting_bagging, newdata = test_set), test_set$Survived)

#Random Forest
set.seed(123)
model_fitting_rf = train(data = train_set, Survived~., method = "rf", prox = TRUE)
model_fitting_rf$finalModel
summary(model_fitting_rf)

confusionMatrix(predict(model_fitting_rf, newdata = train_set), train_set$Survived)
confusionMatrix(predict(model_fitting_rf, newdata = test_set), test_set$Survived)


#Model Fitting with Boosting(Least susceptible to over fitting)
set.seed(123)
model_fitting_boosting = train(data = train_set, Survived~., method = "gbm", verbose = FALSE)
model_fitting_boosting$finalModel
summary(model_fitting_boosting)

confusionMatrix(predict(model_fitting_boosting, newdata = train_set), train_set$Survived)
confusionMatrix(predict(model_fitting_boosting, newdata = test_set), test_set$Survived)
set = train_set
X1 = seq(min(set[, "Age"]) - 1, max(set[, "Age"]) + 1, by = 1)
X2 = seq(min(set[, "Fare"]) - 1, max(set[, "Fare"]) + 1, by = 1)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'Fare')
y_grid = predict(model_fitting_boosting, newdata = grid_set, type = 'raw')

#Incomplete Part
plot(set[, c(1,2)],
     main = "Boosting (Training Set)",
     xlab = "Age", ylab = "Fare",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch ='.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch =21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

