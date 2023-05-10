set.seed(123)
library(ggplot2)
library(lattice)
library(caret)
library(ISLR)

data(Smarket)

training = createDataPartition(y = Smarket$Direction, p = 0.75, list = FALSE)
train_set = Smarket[training,]
test_set = Smarket[-training,]

prop.table(table(Smarket$Direction)) * 100
prop.table(table(train_set$Direction)) * 100
prop.table(table(test_set$Direction)) * 100

library(class)
Predicted_Direction = knn(train = train_set[, -9], 
                          cl = train_set[, 9],
                          test = test_set[, -9],
                          k = 5)

confusionMatrix(Predicted_Direction, test_set$Direction)


model_fitting = train(data = train_set,
                      Direction~.,
                      method = 'knn')

model_fitting

#Predict direction in test_set using our knn model
Predicted_Direction = predict(model_fitting, newdata = test_set)

#Accuracy of prediction in test_set
confusionMatrix(Predicted_Direction, test_set$Direction)


model_fitting_KNN = train(data = train_set, Direction~.,
                          method = 'knn', preProcess = c("center", "scale"),
                          tuneLength = 20)

model_fitting_KNN


plot(model_fitting_KNN)

#Predict direction in test_set using our knn model
Predicted_Direction = predict(model_fitting_KNN, newdata = test_set)

#Accuracy of prediction in test_set
confusionMatrix(Predicted_Direction, test_set$Direction)


knnfit = train(data = train_set, Direction~., method = "knn",
               trControl = trainControl(method = "repeatedcv", repeats = 3,
                                        classProbs = TRUE,
                                        summaryFunction = twoClassSummary),
               preProcess = c("center", "scale"),
               tuneLength = 20)

knnfit


plot(knnfit)


library(pROC)
knn_predict = predict(knnfit, newdata = test_set, type = "prob")

head(knn_predict, 10)

knn_roc = roc(test_set$Direction, knn_predict[, "Down"])

knn_roc

plot(knn_roc)


model_fitting_rf = train(data = test_set, Direction~., method = "rf", prox = TRUE)
model_fitting_rf$finalModel
summary(model_fitting_rf)

confusionMatrix(predict(model_fitting_rf, newdata = test_set), test_set$Direction)
