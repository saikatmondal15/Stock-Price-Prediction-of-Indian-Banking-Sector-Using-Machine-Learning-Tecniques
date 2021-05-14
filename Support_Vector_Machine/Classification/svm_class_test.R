library(kernlab)
train <- read.csv("/home/saikat/Documents/2020/Project/Dataset_ML_Cla.csv")
train <- train[0:2610,]
test <- read.csv("/home/saikat/Documents/2020/Project/Dataset_ML_Cla_test.csv")
test <- test[0:259,]
train$close_norm <- factor(train$close_norm)
length(test$close_norm)
head(train)
head(test)
attach(train)
close_classifier <- ksvm(close_norm~day+day_week+high_norm+low_norm+month+open_norm+range_norm+year, data= train, kernel="vanilladot")
close_classifier
close_pred <- predict(close_classifier, test)
close_pred
plot(test$close_norm~close_pred, xlab = "Predicted percentage change in Close value", ylab = "Actual percentage change in Close value", lwd = 2)
attach(test)
ttt <- table(close_pred, test$close_norm)
ttt
error <- (ttt[1,2]+ttt[2,1])/260
error
#actual <- scan("actual_class_close_perc_2013_2014.txt")
#predicted <- scan("predicted_class_close_perc_2013_2014.txt")
z <- (test$close_norm - as.numeric(close_pred))
which(z==1 | z==-1)
which(z==1)
which(z==-1)
    
