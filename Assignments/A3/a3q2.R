library(caret)
college <- read.csv("~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A3/College.csv")
college <- college[,-1]
inTrain  <- createDataPartition(college[,"Apps"], p = .6, list = FALSE)
Train    <- college[inTrain,]
CV_Test  <- college[-inTrain,]
# OLS
GCV <- function(resp,design) {
        errors <- rep(0,length(resp))
        data <- data.frame(resp=resp,design=design)
        for (i in 1:length(resp)){
                
                mod <- lm(resp ~.,data=data)
                newd <- data.frame(design=design)
                fhati <- predict(mod,newdata=newd)[i]
                denom <- 1 - (1 / length(resp))*sum(lm.influence(mod)$hat)
                errors[i] <- ((resp[i] - fhati)/denom)^2
        }
        return(mean(errors))
}
#
ols.mod <- lm(Apps ~ ., data=Train)
ols.test <- predict(object = ols.mod,CV_Test)
ols.test.error <- mean((ols.test -CV_Test[,"Apps"])^2)
GCV(college$Apps,college[,-2])
# Ridge regression
ctrl <- trainControl(method = "repeatedcv", repeats = 10)
ridge.mod <- train(Apps ~ ., data = Train,method = 'ridge', 
                   trControl = ctrl,
                   metric = "RMSE")
ridge.test <- predict(object = ridge.mod,CV_Test)
ridge.test.error <- mean((ridge.test -CV_Test[,"Apps"])^2)
# Lasso regression: test error and num of nonzero coeffs
lasso.cv<-cv.lars(as.matrix(Train[,-c(1,2)]),Train$Apps,K=10,index=seq(0,1, length=100),
                   type="lasso",mode="fraction",trace=TRUE,max.steps=80)
ideal_l1_ratio <- lasso.cv$index[which.max(lasso.cv$cv - lasso.cv$cv.error <= min(lasso.cv$cv))]
min(lasso.cv$cv)
lasso.mod <- lars(as.matrix(Train[,-c(1,2)]),Train$Apps,type="lasso",trace=TRUE,max.steps=80)
scaled_coefs <- scale(lasso.mod$beta, FALSE, 1 / lasso.mod$normx)
l1 <- apply(X = scaled_coefs, MARGIN = 1, FUN = function(x) sum(abs(x)))
coef(lasso.mod)[which.max(l1 / tail(l1, 1) > ideal_l1_ratio),]
CV_Test.lass <- as.matrix(CV_Test[,-c(1,2)])
lasso.test <- CV_Test.lass%*%coef(lasso.mod)[which.max(l1 / tail(l1, 1) > ideal_l1_ratio),]
lasso.test.error <- mean((lasso.test -CV_Test[,"Apps"])^2)
lasso.test.error
sqrt(lasso.test.error)
# ctrl <- trainControl(method = "repeatedcv", repeats = 10)
# lasso.mod <- train(Apps ~ ., data = Train,method = 'lasso', 
#                    trControl = ctrl,
#                    metric = "RMSE")
# lasso.test <- predict(object = lasso.mod,CV_Test)
# lasso.test.error <- mean((lasso.test -CV_Test[,"Apps"])^2)
# Apaptive lasso: test error and non-zeros
library(parcor)
Y <- Train$Apps
levels(Train[,"Private"]) <- c("Yes","No",1,0)
Train[is.na(Train$Private),"Private"] <- 0
Train[Train[,"Private"]== "Yes","Private"] <- 1
Train[Train[,"Private"]!= "Yes","Private"] <- 0
X <- as.matrix(Train[,-c(1,2)])
adalass.mod <- adalasso(X,Y,k=10)
#
levels(CV_Test[,"Private"]) <- c("Yes","No",1,0)
CV_Test[is.na(CV_Test$Private),"Private"] <- 0
CV_Test[CV_Test[,"Private"]== "Yes","Private"] <- 1
CV_Test[CV_Test[,"Private"]!= "Yes","Private"] <- 0
CV_Test.ada <- as.matrix(CV_Test[,-c(1,2)])
adalasso.test <- CV_Test.ada%*%adalass.mod$coefficients.adalasso
adalasso.test.error <- mean((adalasso.test -CV_Test[,"Apps"])^2)
adalasso.test.error
sqrt(adalasso.test.error)
# Elastic net: test error and number of non zeros
enet.cv <- glmnet::cv.glmnet(X, Y)
enet.coef <- as.vector(coef(enet.cv, s = "lambda.1se"))
enet.test <- CV_Test.ada%*%enet.coef[-1]
enet.test.error <- mean((adalasso.test -CV_Test[,"Apps"])^2)
enet.test.error
sqrt(enet.test.error)
> enet.test.error
[1] 2153528
> sqrt(enet.test.error)
[1] 1467.49
# ctrl <- trainControl(method = "repeatedcv", repeats = 10)
# enet.mod <- train(Apps ~ ., data = Train,method = 'enet', 
#                    trControl = ctrl,
#                    metric = "RMSE")
# enet.test <- predict(object = enet.mod,CV_Test)
# enet.test.error <- mean((enet.test -CV_Test[,"Apps"])^2)
# Comment on results: how accurate, differences among the test erorrs