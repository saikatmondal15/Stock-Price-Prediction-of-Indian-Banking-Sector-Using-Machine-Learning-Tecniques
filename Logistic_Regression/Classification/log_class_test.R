training <- read.csv("/home/saikat/Documents/2020/Project/Dataset_ML_Cla.csv")
training <- training[0:2610,]
training_logit <- glm(close_norm~., family=binomial, data= training)
test <- read.csv("/home/saikat/Documents/2020/Project/Dataset_ML_Cla_test.csv")
test <- test[0:259,]
close_pred<- predict(training_logit,newdata=test,type="response")
close_pred
attach(test)
plot(test$close_norm~close_pred)
gg1=floor(close_pred+0.5)
ttt=table(close_norm,gg1)
ttt
error=(ttt[1,2]+ttt[2,1])/260
error

# Computation of Lift
data.frame(test$close_norm, close_pred)
bb <- cbind(close_pred, test$close_norm)
bb[1:10,]
bb1 <- bb[order(close_pred, decreasing = TRUE),]
xbar <- mean(test$close_norm)
axis <- 240
ax <- 240
ay <- 240
axis[1] = 1
ax[1] = xbar
ay[1] <- bb1[1, 2]
for( i in 2:240){
  axis[i] = i
  ax[i] = xbar*i
  ay[i] = ay[i-1]+ bb1[i,2]
}
aaa <- cbind(bb1[,1], bb1[,2], ay, ax)
plot(axis, ay, xlab="Close Value", ylab="Gain")
points(axis,ax,type="l")

