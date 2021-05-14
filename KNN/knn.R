# KNN Classification #
train <- read.csv("/home/saikat/Documents/2020/Project/Dataset_ML_Cla.csv")
train <- train[0:2610,]
str(train)
table(train$close_norm)
normalize <- function(x) {return ((x-min(x))/(max(x)-min(x)))}
train_n <- as.data.frame(lapply(train, normalize))
library(class)
train_train <- train_n[,-5]
train_test <- train_n[,-5]
train_train_labels <- train[,5]
train_test_labels <- train[,5]
train_test_pred_1 <- knn(train=train_train, test = train_test, cl= train_train_labels, k=1)
train_test_pred_3 <- knn(train=train_train, test = train_test, cl= train_train_labels, k=3)
train_test_pred_5 <- knn(train=train_train, test = train_test, cl= train_train_labels, k=5)
train_test_pred_7 <- knn(train=train_train, test = train_test, cl= train_train_labels, k=7)
train_test_pred_9 <- knn(train=train_train, test = train_test, cl= train_train_labels, k=9)

nearest1 <- knn(train=train_train, test=train_test, cl=train_train_labels, k=1)
nearest1

nearest3 <- knn(train=train_train, test=train_test, cl=train_train_labels, k=3)
nearest3

nearest5 <- knn(train=train_train, test=train_test, cl=train_train_labels, k=5)
nearest5

nearest7 <- knn(train=train_train, test=train_test, cl=train_train_labels, k=7)
nearest7

nearest9 <- knn(train=train_train, test=train_test, cl=train_train_labels, k=9)
nearest9

error1 <- 100*sum(train_test_labels==nearest1)/2610
error1

error3 <- 100*sum(train_test_labels==nearest3)/2610
error3

error5 <- 100*sum(train_test_labels==nearest5)/2610
error5

error7 <- 100*sum(train_test_labels==nearest7)/2610
error7

error9 <- 100*sum(train_test_labels==nearest9)/745
error9


#install.packages("gmodels")
library(gmodels)
CrossTable(x=train_test_labels, y = train_test_pred_1, prop.chisq=FALSE)
CrossTable(x=train_test_labels, y = train_test_pred_3, prop.chisq=FALSE)
CrossTable(x=train_test_labels, y = train_test_pred_5, prop.chisq=FALSE)
CrossTable(x=train_test_labels, y = train_test_pred_7, prop.chisq=FALSE)
CrossTable(x=train_test_labels, y = train_test_pred_9, prop.chisq=FALSE)

