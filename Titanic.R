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

summary(data)
table(is.na(data))
summary(is.na(data))

data$Gender = factor(data$Gender, 
                     levels = c("male","female"),
                     labels = c(0,1))


imputed_data = mice(data)
data = complete(imputed_data, 1)
summary(is.na(data))

data2 = data[, c("Age","Fare","Survived")]

data2$Survived = as.factor(data2$Survived)

ggpairs(data2)


training = createDataPartition(y = data2$Survived, p = 0.75, list = FALSE)
train_set = data2[training, ]
test_set = data2[-training, ]

model_fitting = train(data = train_set, Survived~., method = "rpart")
model_fitting$finalModel

plot(model_fitting$finalModel)
text(model_fitting$finalModel)

fancyRpartPlot(model_fitting$finalModel)

confusionMatrix(predict(model_fitting, newdata = train_set), train_set$Survived)
confusionMatrix(predict(model_fitting, newdata = test_set), test_set$Survived)

set = train_set
X1 = seq(min(set[, "Age"]) - 1, max(set[, "Age"]) + 1, by = 1)
X2 = seq(min(set[, "Fare"]) - 1, max(set[, "Fare"]) + 1, by = 1)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'Fare')
y_grid = predict(model_fitting, newdata = grid_set, type = 'raw')


plot(set[, c(1,2)],
     main = "CART (Training Set)",
     xlab = "Age", ylab = "Fare",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch ='.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch =21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


#Feature Scaling
data2$Age = scale(data2$Age)
data2$Fare = scale(data2$Fare)

#After Feature Scaling
training = createDataPartition(y = data2$Survived, p = 0.75, list = FALSE)
train_set = data2[training, ]
test_set = data2[-training, ]
model_fitting = train(data = train_set, Survived~., method = "rpart")
model_fitting$finalModel

plot(model_fitting$finalModel)
text(model_fitting$finalModel)

fancyRpartPlot(model_fitting$finalModel)

confusionMatrix(predict(model_fitting, newdata = train_set), train_set$Survived)
confusionMatrix(predict(model_fitting, newdata = test_set), test_set$Survived)


# Apply above for
data$Survived = as.factor(data$Survived)
training = createDataPartition(y = data$Survived, p = 0.75, list = FALSE)
train_set = data[training, ]
test_set = data[-training, ]
model_fitting = train(data = train_set, Survived~., method = "rpart")
model_fitting$finalModel

plot(model_fitting$finalModel)
text(model_fitting$finalModel)

fancyRpartPlot(model_fitting$finalModel)

confusionMatrix(predict(model_fitting, newdata = train_set), train_set$Survived)
confusionMatrix(predict(model_fitting, newdata = test_set), test_set$Survived)


#Feature Scaling
data$Age = scale(data$Age)
data$Fare = scale(data$Fare)
training = createDataPartition(y = data$Survived, p = 0.75, list = FALSE)
train_set = data[training, ]
test_set = data[-training, ]
model_fitting = train(data = train_set, Survived~., method = "rpart")
model_fitting$finalModel

plot(model_fitting$finalModel)
text(model_fitting$finalModel)

fancyRpartPlot(model_fitting$finalModel)

confusionMatrix(predict(model_fitting, newdata = train_set), train_set$Survived)
confusionMatrix(predict(model_fitting, newdata = test_set), test_set$Survived)

#Application
imputed_data = mice(data)
data = complete(imputed_data, 1)
data2 = data[, c("Age","Fare","Survived")]

data2$Survived = as.factor(data2$Survived)
data2$Age = scale(data2$Age)
data2$Fare = scale(data2$Fare)
training = createDataPartition(y = data2$Survived, p = 0.60, list = FALSE)
train_set = data2[training, ]
test_set = data2[-training, ]
model_fitting = train(data = train_set, Survived~., method = "rpart")
model_fitting$finalModel

plot(model_fitting$finalModel)
text(model_fitting$finalModel)

fancyRpartPlot(model_fitting$finalModel)

confusionMatrix(predict(model_fitting, newdata = train_set), train_set$Survived)
confusionMatrix(predict(model_fitting, newdata = test_set), test_set$Survived)

set = train_set
X1 = seq(min(set[, "Age"]) - 1, max(set[, "Age"]) + 1, by = 0.3)
X2 = seq(min(set[, "Fare"]) - 1, max(set[, "Fare"]) + 1, by = 0.3)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age", "Fare")
y_grid = predict(model_fitting, newdata = grid_set, type = 'raw')


