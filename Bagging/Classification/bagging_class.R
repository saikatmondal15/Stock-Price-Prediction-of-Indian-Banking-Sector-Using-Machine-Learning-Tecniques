library(ipred)
training <- read.csv("/home/saikat/Documents/2020/Project/Dataset_ML_Cla.csv")
training <- training[0:2610,]
summary(training)
tail(training)
test <- training

set.seed(300)
training$close_norm <- as.factor(training$close_norm)
summary(training)
mybag <- bagging(close_norm~.,data=training, nbag=25)
close_pred <- predict(mybag, test)
summary(close_pred)
close_pred
#attach(test)
close_pred <- as.integer(as.character(close_pred))
close_pred
plot(test$close_norm~close_pred, xlab = "Predicted percenatge change in Close value", ylab = "actual percentage change in close value", lwd = 2)
gg1=floor(close_pred + 0.5) 
gg1
length(close_norm)
length(close_pred)
ttt <- table(test$close_norm,gg1)
ttt
length(gg1)
error <- (ttt[1,2]+ttt[2,1])/260
error
# for identifying the wronly predicted records
x <- (gg1 - close_norm)
n <-which(x== 1 | x== -1)
length(n)
