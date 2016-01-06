library(leaps) ; library(caret)
set.seed(1)
college <- read.csv(file = "~/Dropbox//Academic notes//5 A//STAT 444//Assignments//A4//College.csv")
college <- college[,-1]
inTrain  <- sample(length(college$Apps), length(college$Apps) *.6)
Train    <- college[inTrain,]
CV_Test  <- college[-inTrain,]
# Forward stepwise selection
outstate.mod <- regsubsets(Outstate ~ ., data = Train, nvmax = 17, method = "forward")
summary(outstate.mod)
par(mfrow = c(1, 3))
min.bic <- min(summary(outstate.mod)$bic)
#Minimizing BIC
predictors <- coef(outstate.mod, id = which(min.bic == summary(outstate.mod)$bic))
#
names(predictors)
# ii GAM
library(gam)
opt.outstate.mod <- gam(Outstate ~ Private + s(Room.Board) + s(PhD) + s(perc.alumni) + s(Expend) + s(Grad.Rate) +
                   s(Personal), data=Train)
par(mfrow = c(3, 3))
plot(opt.outstate.mod, se = T, col = "blue")
#explain findings

## iii test set evaluation
outstate.test.pred <- predict(opt.outstate.mod, CV_Test)
outstate.test.error <- mean((CV_Test$Outstate - outstate.test.pred)^2)
outstate.test.error
RMSE <- sqrt(outstate.test.error)
RMSE
tss <- mean((CV_Test$Outstate - mean(CV_Test$Outstate))^2)
rsq <- 1 - outstate.test.error / tss
rsq
# explain results

# get r sq?

## iv non linearrelation with response
summary(opt.outstate.mod)
# Room Board and Expend based on anova