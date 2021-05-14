test <- read.csv("/home/saikat/Documents/2020/Project/Dataset_ML_reg.csv")
test <- test[0:2610,]
test
train <- read.csv("/home/saikat/Documents/2020/Project/Dataset_ML_reg.csv")
train <- train[0:2610,]
train
str(train)
str(test)
normalize <- function(x){return((x-min(x))/(max(x)-min(x)))
}
train_norm <- as.data.frame(lapply(train, normalize))
test_norm <- as.data.frame(lapply(test, normalize))
summary(train_norm$close_norm)
summary(test_norm$close_norm)
naprint(train_norm)
naprint(test_norm)
library(neuralnet)
head(test_norm)
head
#test_norm$year = 0
#train_norm$year = 0
train_model <- neuralnet(close_norm ~day+day_week+high_norm+low_norm+month+open_norm+year,hidden = 2,data = test_norm)
plot(train_model)
model_results <- compute(train_model, test_norm)
predicted_close_norm <- model_results$net.result
predicted_close <- predicted_close_norm*(max(test$close_norm) - min(test$close_norm)) + min(test$close_norm)
predicted_close

plot(test$close_norm, xlab="Time points", ylab="Percentage change in Close value",lty=1, col = "blue", type = 'l', lwd = 2)
lines(predicted_close, lty=2, col = "red", lwd=2)
legend("topleft", c("Actual Index","Predicted Index"), col=c("blue","red"), cex=1, lty=c(1,2), lwd=c(2,2), bty="n")

cor(predicted_close, test$close_norm)
plot(test$close_norm~predicted_close, xlab = "Predicted Index", ylab = "Actual Index", lwd = 2)

library(Metrics)
 x<- rmse(predicted_close, test$close_norm)
 x
 y <- mean(abs(test$close_norm))
 y
 (x/y)*100
 

 w = predicted_close* test$close_norm
 l <- which (w < 0)
 length(l)
mape(predicted_close, test$close_norm) 

