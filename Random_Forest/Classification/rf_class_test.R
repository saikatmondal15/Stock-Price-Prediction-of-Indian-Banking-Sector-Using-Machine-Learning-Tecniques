library(randomForest)
train <- read.csv("/home/saikat/Documents/2020/Project/Dataset_ML_Cla.csv")
train <- train[0:2610,]
test <- read.csv("/home/saikat/Documents/2020/Project/Dataset_ML_Cla_test.csv")
test <- test[0:259,]
train$close_norm <- factor(train$close_norm)
test$close_norm <- factor(test$close_norm)                                 
set.seed(300)
attach(train)
#na.exclude(train)
myrf <- randomForest(close_norm~., data=train)
myrf
close_pred <- predict(myrf, test)
head(close_pred)
length(close_pred)
attach(test)
plot(test$close_norm~close_pred, xlab = "Predicted percentage change in Open value", ylab = "Actual percentage change in Open value", lwd = 2)
gg1=floor((as.numeric(as.character(close_pred))) + 0.5)
gg1
length(close_norm)
length(close_pred)
ttt <- table(close_norm,gg1)
ttt
length(gg1)
error <- (ttt[1,2]+ttt[2,1])/260
error
  