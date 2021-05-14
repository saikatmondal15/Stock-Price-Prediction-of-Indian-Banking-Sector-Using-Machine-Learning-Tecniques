test <- read.csv("/home/saikat/Documents/2020/Project/Dataset_ML_Cla.csv")
test <- test[0:2610,]
head(test)
train <- test
head(train)
str(train)
str(test)
normalize <- function(x){
  
  if((max(x)-min(x)) != 0){
    return((x-min(x))/(max(x)-min(x)))
  }
  else{
    return(0)
  }
    
}
train_norm <- as.data.frame(lapply(train, normalize))
test_norm <- as.data.frame(lapply(test, normalize))
head(train_norm)
#test_norm$year = 0
summary(train_norm$close_norm)
summary(test_norm$close_norm)
library(neuralnet)
head(test_norm)
train_model <- neuralnet(close_norm~day+day_week+high_norm+low_norm+month+open_norm+range_norm+year, data = train_norm,hidden = 1, linear.output=FALSE)
plot(train_model)
model_results <- compute(train_model, test_norm[c(1:4,6:9)])
predicted_perc <- model_results$net.result
predicted_perc 
plot(test$close_norm~predicted_perc, xlab = "Predicted percentage change in Open value", ylab = "Actual percentage change in Open value", lwd = 2)
gg <- floor(predicted_perc + 0.5)
gg
ttt <- table(test_norm$close_norm, gg)
ttt
plot(test_norm$close_norm~predicted_perc)
error <- 100*(ttt[1,2] +ttt[2,1])/2610
error

z <- abs(test_norm$close_norm - gg)
which(z > 0)

