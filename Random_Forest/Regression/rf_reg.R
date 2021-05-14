library(randomForest)
test <- read.csv("/home/saikat/Documents/2020/Project/Dataset_ML_reg.csv")
test <- test[0:2610,]
head(test)
train <- read.csv("/home/saikat/Documents/2020/Project/Dataset_ML_reg.csv")
train <- train[0:2610,]
head(train)
set.seed(300)
attach(train)
myrf <- randomForest(close_norm~day+day_week+high_norm+low_norm+month+open_norm+range_norm+year, data = train)
myrf
close_pred <- predict(myrf, test)
close_pred

plot(test$close_norm, xlab="Time points", ylab="Percentage change in Close value of IT stock", lwd=2, lty=1, col="blue", type='l')
lines(close_pred, lty=2, col="red", lwd=2)
legend("topleft", c("Actual Index","Predicted Index"), col=c("blue","red"), cex=0.8, lty=c(1,2), lwd=c(2,2), bty="n")

r <- (test$close_norm - close_pred)
plot(r, xlab = "Time points", ylab = "Residual values", lwd = 2)
attach(tata_2014)
plot(test$close_norm~close_pred,  xlab = "Predicted Index", ylab = "Actual Index", lwd = 2)

cor(test$close_norm , close_pred)
library(Metrics)
x <- rmse(test$close_norm , close_pred)
x
y <- mean(abs(test$close_norm))
y
z <- (x/y)*100
z

w <- test$close_norm*close_pred

m <- which (w < 0)
length(m)
mape(close_pred,test$close_norm )
