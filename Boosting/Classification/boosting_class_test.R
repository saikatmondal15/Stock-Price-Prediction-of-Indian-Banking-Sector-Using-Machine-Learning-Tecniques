library(adabag)
training <- read.csv("/home/saikat/Documents/2020/Project/Dataset_ML_Cla.csv")
training <- training[0:2610,]
summary(training)
test <- read.csv("/home/saikat/Documents/2020/Project/Dataset_ML_Cla_test.csv")
test <-test[0:259,]
summary(test)
set.seed(300)
training$close_norm <- as.factor(training$close_norm)
#training <- training[1:241,]
#test$close_norm <- as.factor(test$close_norm)
attach(training)
myadaboost <- boosting(close_norm~., data=training)
#test$close_norm <- as.factor(test$close_norm)
close_pred <- predict(myadaboost, test)
close_pred
close_pred <- as.numeric(close_pred$class)
close_pred
attach(test)
plot(test$close_norm~close_pred, xlab = "Predicted percenatge change in Close value", ylab = "actual percentage change in close value", lwd = 2)
gg1=floor(close_pred+0.5)
gg1
#predicted <- scan("predicted.txt")
#actual <- scan("actual.txt")

ttt <- table(close_norm,gg1)
ttt
length(gg1)
error <- (ttt[1,2]+ttt[2,1])/260
error


