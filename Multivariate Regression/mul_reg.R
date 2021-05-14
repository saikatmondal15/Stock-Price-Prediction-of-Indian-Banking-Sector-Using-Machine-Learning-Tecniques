##############################################
#   Backward Removal Procedure               #
##############################################

train <- read.csv("/home/saikat/Documents/2020/Project/Dataset_ML_reg.csv")
train <- train[0:2610,]
head(train)
library(faraway)
close_pred <- lm(close_norm ~. ,data = train)
vif(close_pred)
# Multicollinear variables are: None. We keep all. 
close_pred <- lm(close_norm~., data=train)
formula(close_pred)
drop1(close_pred,test='F')
# month has the lowest AIC(-25705). Hence it is removed.
close_pred <- lm(close_norm~day+day_week+year+open_norm+low_norm+high_norm+range_norm, data=train)
formula(close_pred)
drop1(close_pred, test='F')
# day has the lowest AIC (-25706) and hence removed.
close_pred <- lm(close_norm~day_week+year+open_norm+low_norm+high_norm+range_norm, data=train)
drop1(close_pred, test='F')
# day_week has the lowest AIC (-25708) and hence it is removed.
close_pred <- lm(close_norm~year+open_norm+low_norm+high_norm+range_norm, data=train)
formula(close_pred)
drop1(close_pred, test='F')
# year has the lowest AIC (-25710) and it is not significant, hence it is dropped.
close_pred <- lm(close_norm~open_norm+low_norm+high_norm+range_norm, data=train)
formula(close_pred)
drop1(close_pred, test='F')
# No further predictors can be dropped, since both are significant.
summary(close_pred)
close_pred$residuals
plot(close_pred$residuals, xlab = "Time points", ylab = "Residual values", lwd =2, xlim = c(0, 800))

######################################################
#         Forward Addition Procedure                 #
######################################################

train <- read.csv("/home/saikat/Documents/2020/Project/Dataset_ML_reg.csv")
train <- train[0:2610,]
head(train)
#no multicollinearity exists.
close_pred<- lm(close_norm~1, data=train)
summary(close_pred)
add1(close_pred, scope=train)
# low_norm has the lowest AIC (-23486) and hence it is included in the model.
close_pred<- lm(close_norm~low_norm, data=train)
add1(close_pred, scope=train)
# high_norm has the lowest AIC (-24026) and hence it is added in the predictor list.
close_pred <- lm(close_norm~high_norm + low_norm, data=train)
summary(close_pred)
add1(close_pred, scope=train)
# open_norm has the lowest AIC(-25686) and hence it is included in the model.
close_pred <- lm(close_norm~high_norm + low_norm+open_norm, data=train)
summary(close_pred)
add1(close_pred, scope=train)
# range_norm has the lowest AIC(-25710) and hence it is included in the model.
close_pred <- lm(close_norm~high_norm + low_norm+open_norm + range_norm, data=train)
summary(close_pred)
x <- predict(close_pred)  
y <- train$close_norm
###########################################################################################
# Computation of correlation coefficient between predicted and actual values of close_norm #
###########################################################################################

plot(train$close_norm, xlab="Time points", ylab="Percentage change in Close value",lty=1, col = "blue", type = 'l', lwd = 2)
lines(x, lty=2, col = "red", lwd=2)
legend("topleft", c("Actual Index","Predicted Index"), col=c("blue","red"), cex=0.8, lty=c(1,2), lwd=c(2,2), bty="n")

# Computation of RMSE value #
cor(x, y)
cor.test(x, y)
plot(y, xlab="Time points", ylab="Percentage change in Open value",lty=1, col = "blue", type = 'l', lwd = 2)
lines(x, lty=2, col = "red", lwd=2)
legend("topleft", c("Actual Index","Predicted Index"), col=c("blue","red"), cex=0.8, lty=c(1,2), lwd=c(2,2), bty="n")

library(Metrics)
a <- rmse(x, y)   # 0.2591309
a
b <- mean(abs(y))
b
(a/b)*100       # 32.40208

c <- x*y
l <- which(c<0)
length(l)
