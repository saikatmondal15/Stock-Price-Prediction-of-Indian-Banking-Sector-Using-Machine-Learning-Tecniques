library(ipred)
training <- read.csv("/home/saikat/Documents/2020/Project/Dataset_ML_Cla.csv")
training <- training[0:2610,]
summary(training)
tail(training)
test <- read.csv("/home/saikat/Documents/2020/Project/Dataset_ML_Cla_test.csv")
test <- test[0:259,]

training$close_norm <- as.factor(training$close_norm)
set.seed(300)
mybag <- bagging(close_norm~day+ day_week+month+year+open_norm+low_norm+high_norm+range_norm,data=training, nbag=25)
#test$close_norm <- as.factor(test$close_norm)
close_pred <- predict(mybag, test)
close_pred
attach(test)
close_pred <- as.integer(as.character(close_pred))
close_pred
plot(test$close_norm~close_pred, xlab = "Predicted percenatge change in Close value", ylab = "actual percentage change in close value", lwd = 2)
#close_pred <- as.numeric(close_pred)
gg1=floor(close_pred+0.5)
gg1
length(close_norm)
length(close_pred)
ttt <- table(test$close_norm,gg1)
ttt
length(gg1)
error <- (ttt[1,2]+ttt[2,1])/2610
  error
# for identifying the wronly predicted records
x <- (gg1 - close_norm)
n <-which(x== 1 | x== -1)
length(n)
