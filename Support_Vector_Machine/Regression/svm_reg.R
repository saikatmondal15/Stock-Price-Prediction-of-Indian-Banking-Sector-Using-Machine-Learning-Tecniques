library(e1071)
train <- read.csv("/home/saikat/Documents/2020/Project/Dataset_ML_reg.csv")
train <- train[0:2610,]
head(train)
test <- read.csv("/home/saikat/Documents/2020/Project/Dataset_ML_reg.csv")
test <- test[0:2610,]
head(test)
attach(train)
train_reg <- svm(close_norm ~ day+day_week+high_norm+low_norm+month+open_norm+range_norm ,data = train, scale = TRUE)
train_reg
close_pred <- predict(train_reg, test)
close_pred
length(close_pred)
attach(test)
plot(test$close_norm, xlab="Time points", ylab="Percentage change in Close Price stock",lty=1, col = "blue", type = 'l', lwd = 2, ylim = c(-0.1, 0.1))
lines(close_pred, lty=2, col = "red", lwd=2)
legend("topleft", c("Actual Index","Predicted Index"), col=c("blue","red"), cex=0.8, lty=c(1,2), lwd=c(2,2), bty="n")

y <- (test$close_norm - close_pred)
plot(y, xlab = "Time points", ylab = "Residual values", lwd = 2)
  cor(test$close_norm , close_pred)
attach(test)
plot(test$close_norm~close_pred, xlab = "Predicted Index", ylab = "Actual Index", lwd = 2)
library(Metrics)
x <- rmse(test$close_norm , close_pred)
x
y <- mean(abs(test$close_norm))
y
z <- (x/y)*100
z

w <- test$close_norm * close_pred
m <- which (w < 0)
length(m)
mape(close_pred,test$close_norm )

