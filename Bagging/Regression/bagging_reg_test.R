library(ipred)
test<- read.csv("/home/saikat/Documents/2020/Project/Dataset_ML_reg_test.csv")
test <-test[0:259,]
summary(test)
head(test)
training <- read.csv("/home/saikat/Documents/2020/Project/Dataset_ML_reg.csv")
test <-test[0:259,]
head(training)

set.seed(300)
mybag <- bagging(close_norm~day+day_week+month+year+open_norm+low_norm+high_norm, data=training, nbag=200)
close_pred <- predict(mybag, test)
close_pred
summary(close_pred)
plot(test$close_norm, xlab="Time points", ylab="Percentage change in Close value",lty=1, col = "blue", type = 'l', lwd = 2, ylim = c(-0.1, 0.1))
lines(close_pred, lty=2, col = "red", lwd=2)
legend("topleft", c("Actual Index","Predicted Index"), col=c("blue","red"), cex=0.8, lty=c(1,2), lwd=c(2,2), bty="n")

y <- (test$close_norm - close_pred)
plot(y, xlab = "Time points", ylab = "Residual values", lwd = 2)
cor(test$close_norm, close_pred)
plot(test$close_norm~close_pred, xlab = "Predicted Index", ylab = "Actual Index", lwd = 2)

library(Metrics)
#mape(close_pred,test$close_norm)
x <- rmse(test$close_norm, close_pred)
x
y <- mean(abs(test$close_norm))
y
z <- (x/y)*100
z
w <- test$close_norm* close_pred
m <- which( w <0 )
m
length(m)

