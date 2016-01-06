library(MASS) ; 
###################################################
#                                              ####
#  Model Assessment Tools taught in Stat 444   ####
#  to assess models for A2                     ####
#                                              ####
###################################################

#LOOCV : leave one out cross validation
LCV <- function(resp,design) {
        errors <- rep(0,length(resp))
        for (i in 1:length(resp)){
                if(is.vector(design)){
                        datai <- data.frame(resp=resp[-i],design=design[-i])
                } else {
                        datai <- data.frame(resp=resp[-i],design=design[-i,])        
                }
                mod <- lm(resp ~.,data=datai)
                newd <- data.frame(design=design)
                fhati <- predict(mod,newdata=newd)[i]
                errors[i] <- (resp[i] - fhati)^2
        }
        return(mean(errors))
}

# Generalized cross validation (lm)
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
################################################
par(mfrow=c(1,2))
library(MASS)
summary(Boston) ; attach(Boston) 

# best subset selection , forward selection, backward elimination, 
# ridge regression, lasso
library(leaps)
# Best Subset selection
Design <- Boston[,-1]
Response <- data.frame(crim=Boston[,1])
crime.subsets <- leaps(x=Boston[,-1],y=Boston$crim,nbest=1,method="Cp")
plot(crime.subsets$size,crime.subsets$Cp,pch=23,bg='red',cex=1.5)
crime.subsets.Cp <- crime.subsets$which[which((crime.subsets$Cp == min(crime.subsets$Cp))),]
crime.subsets.Cp
crime.subsets
# Analyze the subsets

# Backward elimination next
back.elim.mod <- lm(crim~.,data=Boston)
summary(back.elim.mod)
# In iteration 1, the least of the statistically insignificant coefficient was age. So it was removed from the model.
back.elim.mod <- lm(crim~.-age,data=Boston)
summary(back.elim.mod)
# In iteration 2, the least of the statistically insignificant coefficient was chas. So it was removed from the model.
back.elim.mod <- lm(crim~.-age-chas,data=Boston)
summary(back.elim.mod)
# In iteration 3, the least of the statistically insignificant coefficient was tax. So it was removed from the model.
back.elim.mod <- lm(crim~.-age-chas-tax,data=Boston)
summary(back.elim.mod)
# In iteration 4, the least of the statistically insignificant coefficient was rm. So it was removed from the model.
back.elim.mod <- lm(crim~.-age-chas-tax-rm,data=Boston)
summary(back.elim.mod)
# In iteration 5, the least of the statistically insignificant coefficient was indus. So it was removed from the model.
back.elim.mod <- lm(crim~.-age-chas-tax-rm-indus,data=Boston)
summary(back.elim.mod)
# In iteration 6, the least of the statistically insignificant coefficient was lstat. So it was removed from the model.
back.elim.mod <- lm(crim~.-age-chas-tax-rm-indus-lstat,data=Boston)
summary(back.elim.mod)
# In iteration 7, the least of the statistically insignificant coefficient was ptratio. So it was removed from the model.
back.elim.mod <- lm(crim~.-age-chas-tax-rm-indus-lstat-ptratio,data=Boston)
summary(back.elim.mod)
# In iteration 8, all of the predictors were statistically significant so we stopped the backward elimination. The resulting model fit crime against: zn, nox, dis, rad, black and medv.
# Forward selection next
for.sel.mod <- lm(crim~1,data=Boston)
summary(for.sel.mod)
The constant model fit was significant.
for.sel.mod <- lm(crim~zn,data=Boston)
summary(for.sel.mod)
t value for zn is 5.51e-06
for.sel.mod <- lm(crim~indus,data=Boston)
summary(for.sel.mod)
t value for indus is <2e-16
for.sel.mod <- lm(crim~chas,data=Boston)
summary(for.sel.mod)
t value for chas is 0.209
for.sel.mod <- lm(crim~nox,data=Boston)
summary(for.sel.mod)
t value for nox is < 2e-16
for.sel.mod <- lm(crim~rm,data=Boston)
summary(for.sel.mod)
t value for rm is 6.35e-07
for.sel.mod <- lm(crim~age,data=Boston)
summary(for.sel.mod)
t value for age is 2.85e-16
for.sel.mod <- lm(crim~dis,data=Boston)
summary(for.sel.mod)
t value for dis is <2e-16
for.sel.mod <- lm(crim~rad,data=Boston)
summary(for.sel.mod)
t value for rad is <2e-16
for.sel.mod <- lm(crim~tax,data=Boston)
summary(for.sel.mod)
t value for tax is <2e-16
for.sel.mod <- lm(crim~ptratio,data=Boston)
summary(for.sel.mod)
t value for ptratio is 2.94e-11
for.sel.mod <- lm(crim~black,data=Boston)
summary(for.sel.mod)
t value for black is <2e-16
for.sel.mod <- lm(crim~lstat,data=Boston)
summary(for.sel.mod)
t value for lstat is < 2e-16
for.sel.mod <- lm(crim~medv,data=Boston)
summary(for.sel.mod)
t value for medv is < 2e-16
Hence, indus was the first, equally most significant predictor and was added to the model.
for.sel.mod <- lm(crim~indus+zn,data=Boston)
summary(for.sel.mod)
t value for zn is .63048
for.sel.mod <- lm(crim~indus+chas,data=Boston)
summary(for.sel.mod)
t value for chas is 0.04473
for.sel.mod <- lm(crim~indus+nox,data=Boston)
summary(for.sel.mod)
t value for nox is < 2.27e-05
for.sel.mod <- lm(crim~indus+rm,data=Boston)
summary(for.sel.mod)
t value for rm is 0.109
for.sel.mod <- lm(crim~indus+age,data=Boston)
summary(for.sel.mod)
t value for age is 0.0035
for.sel.mod <- lm(crim~indus+dis,data=Boston)
summary(for.sel.mod)
t value for dis is 0.00135
for.sel.mod <- lm(crim~indus+rad,data=Boston)
summary(for.sel.mod)
t value for rad is <2e-16
for.sel.mod <- lm(crim~indus+tax,data=Boston)
summary(for.sel.mod)
t value for tax is <2e-16
for.sel.mod <- lm(crim~indus+ptratio,data=Boston)
summary(for.sel.mod)
t value for ptratio is 0.000336
for.sel.mod <- lm(crim~indus+black,data=Boston)
summary(for.sel.mod)
t value for black is 1.26e-10
for.sel.mod <- lm(crim~indus+lstat,data=Boston)
summary(for.sel.mod)
t value for lstat is 3.87e-11
for.sel.mod <- lm(crim~indus+medv,data=Boston)
summary(for.sel.mod)
t value for medv is 4.99e-08
The best candidate for second variable was rad
for.sel.mod <- lm(crim~indus+rad+zn,data=Boston)
summary(for.sel.mod)
t value for zn is .655
for.sel.mod <- lm(crim~indus+rad+chas,data=Boston)
summary(for.sel.mod)
t value for chas is .113
for.sel.mod <- lm(crim~indus+rad+nox,data=Boston)
summary(for.sel.mod)
t value for nox is .4198
for.sel.mod <- lm(crim~indus+rad+rm,data=Boston)
summary(for.sel.mod)
t value for rm is .0207
for.sel.mod <- lm(crim~indus+rad+age,data=Boston)
summary(for.sel.mod)
t value for age is .0707
for.sel.mod <- lm(crim~indus+rad+dis,data=Boston)
summary(for.sel.mod)
t value for dis is 0.0469
for.sel.mod <- lm(crim~indus+rad+tax,data=Boston)
summary(for.sel.mod)
t value for tax is .74840
for.sel.mod <- lm(crim~indus+rad+ptratio,data=Boston)
summary(for.sel.mod)
t value for ptratio is 0.833
for.sel.mod <- lm(crim~indus+rad+black,data=Boston)
summary(for.sel.mod)
t value for black is 0.000888
for.sel.mod <- lm(crim~indus+rad+lstat,data=Boston)
summary(for.sel.mod)
t value for lstat is 6.72e-07
for.sel.mod <- lm(crim~indus+rad+medv,data=Boston)
summary(for.sel.mod)
t value for medv is 5.31e-06
The best candidate for third variable is medv
for.sel.mod <- lm(crim~indus+rad+medv+zn,data=Boston)
summary(for.sel.mod)
t value for zn is .265
for.sel.mod <- lm(crim~indus+rad+medv+chas,data=Boston)
summary(for.sel.mod)
t value for chas is .5683
for.sel.mod <- lm(crim~indus+rad+medv+nox,data=Boston)
summary(for.sel.mod)
t value for nox is .602
for.sel.mod <- lm(crim~indus+rad+medv+rm,data=Boston)
summary(for.sel.mod)
t value for rm is .45
for.sel.mod <- lm(crim~indus+rad+medv+age,data=Boston)
summary(for.sel.mod)
t value for age is .143
for.sel.mod <- lm(crim~indus+rad+medv+dis,data=Boston)
summary(for.sel.mod)
t value for dis is .004288
for.sel.mod <- lm(crim~indus+rad+medv+tax,data=Boston)
summary(for.sel.mod)
t value for tax is .6613
for.sel.mod <- lm(crim~indus+rad+medv+ptratio,data=Boston)
summary(for.sel.mod)
t value for ptratio is 0.03393
for.sel.mod <- lm(crim~indus+rad+medv+black,data=Boston)
summary(for.sel.mod)
t value for black is 0.00784
for.sel.mod <- lm(crim~indus+medv+rad+lstat,data=Boston)
summary(for.sel.mod)
t value for lstat is 0.00637
The best candidate for fourth variable is dis

for.sel.mod <- lm(crim~indus+rad+medv+dis+zn,data=Boston)
summary(for.sel.mod)
t value for zn is .00208
for.sel.mod <- lm(crim~indus+rad+medv+dis+chas,data=Boston)
summary(for.sel.mod)
t value for chas is .475131
for.sel.mod <- lm(crim~indus+rad+medv+dis+nox,data=Boston)
summary(for.sel.mod)
t value for nox is .27842
for.sel.mod <- lm(crim~indus+rad+medv+dis+rm,data=Boston)
summary(for.sel.mod)
t value for rm is .4457
for.sel.mod <- lm(crim~indus+rad+medv+dis+age,data=Boston)
summary(for.sel.mod)
t value for age is .88487
for.sel.mod <- lm(crim~indus+rad+medv+dis+tax,data=Boston)
summary(for.sel.mod)
t value for tax is .87484
for.sel.mod <- lm(crim~indus+rad+medv+dis+ptratio,data=Boston)
summary(for.sel.mod)
t value for ptratio is 0.045146
for.sel.mod <- lm(crim~indus+rad+medv+dis+black,data=Boston)
summary(for.sel.mod)
t value for black is 0.01114
for.sel.mod <- lm(crim~indus+medv+rad+dis+lstat,data=Boston)
summary(for.sel.mod)
t value for lstat is 0.0412

The best candidate for the fifth variable is zn
for.sel.mod <- lm(crim~indus+rad+medv+dis+zn+chas,data=Boston)
summary(for.sel.mod)
t value for chas is .515
for.sel.mod <- lm(crim~indus+rad+medv+dis+zn+nox,data=Boston)
summary(for.sel.mod)
t value for nox is .176892
for.sel.mod <- lm(crim~indus+rad+medv+dis+zn+rm,data=Boston)
summary(for.sel.mod)
t value for rm is .56843
for.sel.mod <- lm(crim~indus+rad+medv+dis+zn+age,data=Boston)
summary(for.sel.mod)
t value for age is .92596
for.sel.mod <- lm(crim~indus+rad+medv+dis+zn+tax,data=Boston)
summary(for.sel.mod)
t value for tax is .35984
for.sel.mod <- lm(crim~indus+rad+medv+dis+zn+ptratio,data=Boston)
summary(for.sel.mod)
t value for ptratio is .238924
for.sel.mod <- lm(crim~indus+rad+medv+dis+zn+black,data=Boston)
summary(for.sel.mod)
t value for black is .020258
for.sel.mod <- lm(crim~indus+medv+rad+dis+zn+lstat,data=Boston)
summary(for.sel.mod)
t value for lstat is 0.074985

The best candidate for sixth variable is black
for.sel.mod <- lm(crim~indus+rad+medv+dis+zn+black+chas,data=Boston)
summary(for.sel.mod)
t value for chas is .5528
for.sel.mod <- lm(crim~indus+rad+medv+dis+zn+black+nox,data=Boston)
summary(for.sel.mod)
t value for nox is .13245
for.sel.mod <- lm(crim~indus+rad+medv+dis+zn+balck+rm,data=Boston)
summary(for.sel.mod)
t value for rm is .13245
for.sel.mod <- lm(crim~indus+rad+medv+dis+zn+black+age,data=Boston)
summary(for.sel.mod)
t value for age is .902952
for.sel.mod <- lm(crim~indus+rad+medv+dis+zn+black+tax,data=Boston)
summary(for.sel.mod)
t value for tax is .36155
for.sel.mod <- lm(crim~indus+rad+medv+dis+zn+black+ptratio,data=Boston)
summary(for.sel.mod)
t value for ptratio is .352914
for.sel.mod <- lm(crim~indus+medv+rad+dis+zn+black+lstat,data=Boston)
summary(for.sel.mod)
t value for lstat is .09057
After seven iterations, there are no new significant variables to add. The final model fits crime against indus, medv, rad, dis, zn and black. 
#Final model
for.sel.mod <- lm(crim~indus+medv+rad+dis+zn+black,data=Boston)
summary(for.sel.mod)
# (Backward) Stepwise selection
step.sel.mod <- lm(crim~.,data=Boston)
summary(step.sel.mod)
Age is the first variable to be dropped
step.sel.mod <- lm(crim~.-age,data=Boston)
summary(step.sel.mod)
Chas is the next variable to be dropped
step.sel.mod <- lm(crim~.-age-chas,data=Boston)
summary(step.sel.mod)
# Checking if age could be re-added
step.sel.mod <- lm(crim~.-chas,data=Boston)
summary(step.sel.mod)
Age is still statistically insignificant so it will remain dropped and the next variable to be dropped is tax
step.sel.mod <- lm(crim~.-age-chas-tax,data=Boston)
summary(step.sel.mod)
# Checking if age or chas could be re-added
step.sel.mod <- lm(crim~.-chas-tax,data=Boston)
summary(step.sel.mod)
step.sel.mod <- lm(crim~.-age-tax,data=Boston)
summary(step.sel.mod)
Both age and chas are still statistically insignificant so the will remain dropped and the next variable to drop is rm
step.sel.mod <- lm(crim~.-age-chas-tax-rm,data=Boston)
summary(step.sel.mod)
# Checking if age or chas or tax could be re-added
step.sel.mod <- lm(crim~.-chas-tax-rm,data=Boston)
summary(step.sel.mod)
step.sel.mod <- lm(crim~.-age-tax-rm,data=Boston)
summary(step.sel.mod)
step.sel.mod <- lm(crim~.-age-chas-rm,data=Boston)
summary(step.sel.mod)
Age, chas and tax all still are statistically insignificant and so will remain dropped and the next variable to drop is indus
step.sel.mod <- lm(crim~.-age-chas-tax-rm,data=Boston)
summary(step.sel.mod)
# Checking if age or chas or tax or rm could be re-added
step.sel.mod <- lm(crim~.-chas-tax-rm-indus,data=Boston)
summary(step.sel.mod)
step.sel.mod <- lm(crim~.-age-tax-rm-indus,data=Boston)
summary(step.sel.mod)
step.sel.mod <- lm(crim~.-age-chas-rm-indus,data=Boston)
summary(step.sel.mod)
step.sel.mod <- lm(crim~.-age-chas-tax-indus,data=Boston)
summary(step.sel.mod)
Age, chas, tax and rm all are still statistically insignificant and so will remain dropped and the next variable to drop is lstat
step.sel.mod <- lm(crim~.-age-chas-tax-rm-indus,data=Boston)
summary(step.sel.mod)
# Checking if age or chas or tax or rm or indus could be re-added
step.sel.mod <- lm(crim~.-chas-tax-rm-indus-lstat,data=Boston)
summary(step.sel.mod)
step.sel.mod <- lm(crim~.-age-tax-rm-indus-lstat,data=Boston)
summary(step.sel.mod)
step.sel.mod <- lm(crim~.-age-chas-rm-indus-lstat,data=Boston)
summary(step.sel.mod)
step.sel.mod <- lm(crim~.-age-chas-tax-indus-lstat,data=Boston)
summary(step.sel.mod)
step.sel.mod <- lm(crim~.-age-chas-tax-rm-lstat,data=Boston)
summary(step.sel.mod)
All the curently excluded variables remain statistically insignificant and so will remain dropped and the next variable to remove is ptratio
step.sel.mod <- lm(crim~.-age-chas-tax-rm-indus-lstat,data=Boston)
summary(step.sel.mod)
# Checking if age or chas or tax or rm or indus or lstat could be re-added
step.sel.mod <- lm(crim~.-chas-tax-rm-indus-lstat-ptratio,data=Boston)
summary(step.sel.mod)
step.sel.mod <- lm(crim~.-age-tax-rm-indus-lstat-ptratio,data=Boston)
summary(step.sel.mod)
step.sel.mod <- lm(crim~.-age-chas-rm-indus-lstat-ptratio,data=Boston)
summary(step.sel.mod)
step.sel.mod <- lm(crim~.-age-chas-tax-indus-lstat-ptratio,data=Boston)
summary(step.sel.mod)
step.sel.mod <- lm(crim~.-age-chas-tax-rm-indus-ptratio,data=Boston)
summary(step.sel.mod)
All the curently excluded variables remain statistically insignificant and so will remain dropped and the next variable to remove is 
# Checking if age or chas or tax or rm or indus or lstat or ptratio could be re-added
step.sel.mod <- lm(crim~.-age-chas-tax-rm-indus-lstat-ptratio,data=Boston)
summary(step.sel.mod)
All the variables are statistically significant now so the stepwise selection terminates giving the model fitting crime against zn, nox, dis, rad, black and medv.

b
# Propose a few models : justify with LOOCV , GCV , AIC, BIC , Adj Rsquare
summary(step.sel.mod)
summary(for.sel.mod)
summary(back.elim.mod)
best.subset.mod.1 <- lm(crim~dis,data=Boston)
best.subset.mod.2 <- lm(crim~dis+black,data=Boston)
best.subset.mod.3 <- lm(crim~dis+black+ptratio,data=Boston)
best.subset.mod.4 <- lm(crim~dis+lstat+medv+age,data=Boston)
best.subset.mod.5 <- lm(crim~age+dis+ptratio+lstat+medv,data=Boston)
best.subset.mod.6 <- lm(crim~age+dis+ptratio+lstat+medv+chas,data=Boston)
best.subset.mod.7 <- lm(crim~age+dis+ptratio+tax+lstat+medv+chas,data=Boston)
best.subset.mod.8 <- lm(crim~age+dis+ptratio+tax+lstat+medv+chas+black,data=Boston)
best.subset.mod.9 <- lm(crim~zn+age+dis+ptratio+tax+lstat+medv+chas+black,data=Boston)
best.subset.mod.10 <- lm(crim~nox+age+dis+ptratio+tax+lstat+medv+chas+black,data=Boston)
best.subset.mod.10 <- lm(crim~nox+age+dis+ptratio+tax+lstat+medv+chas+black,data=Boston)
best.subset.mod.11 <- lm(crim~nox+age+dis+ptratio+tax+rad+lstat+medv+chas+black,data=Boston)
best.subset.mod.12 <- lm(crim~nox+age+dis+ptratio+tax+rad+lstat+medv+chas+black+indus,data=Boston)
best.subset.mod.13 <- lm(crim~nox+age+dis+ptratio+tax+rad+lstat+medv+chas+black+indus+rm,data=Boston)

# Leave one out and Generalized CV scores
# make a vector for each entry and find the min in each measure
LCVscores <- rep(0,16)
GCVscores <- rep(0,16)
AICscores <- rep(0,16)
BICscores <- rep(0,16)
Mallowscores <- rep(0,16)
SUREscores <- rep(0,16)
ArSscores <- rep(0,16)

LCVscores[1] <-LCV(Boston[,"crim"],Boston[,c("zn","nox","dis","rad","black","medv")]) ;
GCVscores[1] <-GCV(Boston[,"crim"],Boston[,c("zn","nox","dis","rad","black","medv")]) ;
AICscores[1] <-AIC(step.sel.mod) ; BICscores[1] <-BIC(step.sel.mod) ; 
Mallowscores[1] <-myMallows(Boston,step.sel.mod); SUREscores[1] <-mySURE(step.sel.mod)
ArSscores[1] <-myArS(Boston,step.sel.mod)
#Forward Selection
LCVscores[2] <-LCV(Boston[,"crim"],Boston[,c("indus","medv","dis","rad","zn","black")])
GCVscores[2] <-GCV(Boston[,"crim"],Boston[,c("indus","medv","dis","rad","zn","black")])
AICscores[2] <-AIC(for.sel.mod) ; BICscores[2] <-BIC(for.sel.mod) ; 
Mallowscores[2] <-myMallows(Boston,for.sel.mod); SUREscores[2] <-mySURE(for.sel.mod)
ArSscores[2] <-myArS(Boston,for.sel.mod)
#Backward elimination
LCVscores[3] <-LCV(Boston[,"crim"],Boston[,c("zn","nox","dis","rad","black","medv")])
GCVscores[3] <-GCV(Boston[,"crim"],Boston[,c("zn","nox","dis","rad","black","medv")])
AICscores[3] <-AIC(back.elim.mod) ; BICscores[3] <-BIC(back.elim.mod) ; 
Mallowscores[3] <-myMallows(Boston,back.elim.mod); SUREscores[3] <-mySURE(back.elim.mod)
ArSscores[3] <-myArS(Boston,back.elim.mod)
# Best Subset models
LCVscores[4] <-LCV(Boston[,"crim"],Boston[,c("dis")])
GCVscores[4] <-GCV(Boston[,"crim"],Boston[,c("dis")])
AICscores[4] <-AIC(best.subset.mod.1) ; BICscores[4] <-BIC(best.subset.mod.1) ; 
Mallowscores[4] <-myMallows(Boston,best.subset.mod.1); SUREscores[4] <-mySURE(best.subset.mod.1)
ArSscores[4] <-myArS(Boston,best.subset.mod.1)

LCVscores[5] <-LCV(Boston[,"crim"],Boston[,c("dis","black")])
GCVscores[5] <-GCV(Boston[,"crim"],Boston[,c("dis","black")])
AICscores[5] <-AIC(best.subset.mod.2) ; BICscores[5] <-BIC(best.subset.mod.2) ; 
Mallowscores[5] <-myMallows(Boston,best.subset.mod.2); SUREscores[5] <-mySURE(best.subset.mod.2)
ArSscores[5] <-myArS(Boston,best.subset.mod.2)

LCVscores[6] <-LCV(Boston[,"crim"],Boston[,c("dis","black","ptratio")])
GCVscores[6] <-GCV(Boston[,"crim"],Boston[,c("dis","black","ptratio")])
AICscores[6] <-AIC(best.subset.mod.3) ; BICscores[6] <-BIC(best.subset.mod.3) ; 
Mallowscores[6] <-myMallows(Boston,best.subset.mod.3); SUREscores[6] <-mySURE(best.subset.mod.3)
ArSscores[6] <-myArS(Boston,best.subset.mod.3)

LCVscores[7] <-LCV(Boston[,"crim"],Boston[,c("dis","medv","age","lstat")])
GCVscores[7] <-GCV(Boston[,"crim"],Boston[,c("dis","medv","age","lstat")])
AICscores[7] <-AIC(best.subset.mod.4) ; BICscores[7] <-BIC(best.subset.mod.4) ; 
Mallowscores[7] <-myMallows(Boston,best.subset.mod.4); SUREscores[7] <-mySURE(best.subset.mod.4)
ArSscores[7] <-myArS(Boston,best.subset.mod.4)

LCVscores[8] <-LCV(Boston[,"crim"],Boston[,c("dis","medv","age","lstat","ptratio")])
GCVscores[8] <-GCV(Boston[,"crim"],Boston[,c("dis","medv","age","lstat","ptratio")])
AICscores[8] <-AIC(best.subset.mod.5) ; BICscores[8] <-BIC(best.subset.mod.5) ; 
Mallowscores[8] <-myMallows(Boston,best.subset.mod.5); SUREscores[8] <-mySURE(best.subset.mod.5)
ArSscores[8] <-myArS(Boston,best.subset.mod.5)

LCVscores[9] <-LCV(Boston[,"crim"],Boston[,c("dis","medv","age","lstat","ptratio","chas")])
GCVscores[9] <-GCV(Boston[,"crim"],Boston[,c("dis","medv","age","lstat","ptratio","chas")])
AICscores[9] <-AIC(best.subset.mod.6) ; BICscores[9] <-BIC(best.subset.mod.6) ; 
Mallowscores[9] <-myMallows(Boston,best.subset.mod.6); SUREscores[9] <-mySURE(best.subset.mod.6)
ArSscores[9] <-myArS(Boston,best.subset.mod.6)

LCVscores[10] <-LCV(Boston[,"crim"],Boston[,c("dis","medv","age","lstat","ptratio","chas","tax")])
GCVscores[10] <-GCV(Boston[,"crim"],Boston[,c("dis","medv","age","lstat","ptratio","chas","tax")])
AICscores[10] <-AIC(best.subset.mod.7) ; BICscores[10] <-BIC(best.subset.mod.7) ; 
Mallowscores[10] <-myMallows(Boston,best.subset.mod.7); SUREscores[10] <-mySURE(best.subset.mod.7)
ArSscores[10] <-myArS(Boston,best.subset.mod.7)

LCVscores[11] <-LCV(Boston[,"crim"],Boston[,c("dis","medv","age","lstat","ptratio","chas","tax","black")])
GCVscores[11] <-GCV(Boston[,"crim"],Boston[,c("dis","medv","age","lstat","ptratio","chas","tax","black")])
AICscores[11] <-AIC(best.subset.mod.8) ; BICscores[11] <-BIC(best.subset.mod.8) ; 
Mallowscores[11] <-myMallows(Boston,best.subset.mod.8); SUREscores[11] <-mySURE(best.subset.mod.8)
ArSscores[11] <-myArS(Boston,best.subset.mod.8)

LCVscores[12] <-LCV(Boston[,"crim"],Boston[,c("zn","dis","medv","age","lstat","ptratio","chas","tax","black")])
GCVscores[12] <-GCV(Boston[,"crim"],Boston[,c("zn","dis","medv","age","lstat","ptratio","chas","tax","black")])
AICscores[12] <-AIC(best.subset.mod.9) ; BICscores[12] <-BIC(best.subset.mod.9) ; 
Mallowscores[12] <-myMallows(Boston,best.subset.mod.9); SUREscores[12] <-mySURE(best.subset.mod.9)
ArSscores[12] <-myArS(Boston,best.subset.mod.9)

LCVscores[13] <-LCV(Boston[,"crim"],Boston[,c("zn","dis","medv","age","lstat","ptratio","chas","tax","black","nox")])
GCVscores[13] <-GCV(Boston[,"crim"],Boston[,c("zn","dis","medv","age","lstat","ptratio","chas","tax","black","nox")])
AICscores[13] <-AIC(best.subset.mod.10) ; BICscores[13] <-BIC(best.subset.mod.10) ; 
Mallowscores[13] <-myMallows(Boston,best.subset.mod.10); SUREscores[13] <-mySURE(best.subset.mod.10)
ArSscores[13] <-myArS(Boston,best.subset.mod.10)

LCVscores[14] <-LCV(Boston[,"crim"],Boston[,c("zn","dis","medv","age","lstat","ptratio","chas","tax","black","nox","rad")])
GCVscores[14] <-GCV(Boston[,"crim"],Boston[,c("zn","dis","medv","age","lstat","ptratio","chas","tax","black","nox","rad")])
AICscores[14] <-AIC(best.subset.mod.11) ; BICscores[14] <-BIC(best.subset.mod.11) ; 
Mallowscores[14] <-myMallows(Boston,best.subset.mod.11); SUREscores[14] <-mySURE(best.subset.mod.11)
ArSscores[14] <-myArS(Boston,best.subset.mod.11)

LCVscores[15] <-LCV(Boston[,"crim"],Boston[,c("zn","dis","medv","age","lstat","ptratio","chas","tax","black","nox","rad","indus")])
GCVscores[15] <-GCV(Boston[,"crim"],Boston[,c("zn","dis","medv","age","lstat","ptratio","chas","tax","black","nox","rad","indus")])
AICscores[15] <-AIC(best.subset.mod.12) ; BICscores[15] <-BIC(best.subset.mod.12) ; 
Mallowscores[15] <-myMallows(Boston,best.subset.mod.12); SUREscores[15] <-mySURE(best.subset.mod.12)
ArSscores[15] <-myArS(Boston,best.subset.mod.12)

LCVscores[16] <-LCV(Boston[,"crim"],Boston[,c("zn","dis","medv","age","lstat","ptratio","chas","tax","black","nox","rad","indus","rm")])
GCVscores[16] <-GCV(Boston[,"crim"],Boston[,c("zn","dis","medv","age","lstat","ptratio","chas","tax","black","nox","rad","indus","rm")])
AICscores[16] <-AIC(best.subset.mod.13) ; BICscores[16] <-BIC(best.subset.mod.13) ; 
Mallowscores[16] <-myMallows(Boston,best.subset.mod.13); SUREscores[16] <-mySURE(best.subset.mod.13)
ArSscores[16] <-myArS(Boston,best.subset.mod.13)

c
# Does the chosen model have all the features from the dataset ? Why or why not ?

# Best models by metric
match(min(LCVscores),LCVscores)
match(min(GCVscores),GCVscores)
match(min(AICscores),AICscores)
match(min(BICscores),BICscores)
match(min(SUREscores),SUREscores)
match(min(Mallowscores),Mallowscores)
match(max(ArSscores),ArSscores)
Model 1, obtained from stepwise selection was top in terms of the msot metrics: AIC, BIC and GCV. Second most frequently "top" model was the best subset selection model of size one.
Neither of these top recommended models uses all of the predictors. This is because there were a few predictors that were not statistically significant when fit against the response and therefore you wouldn't expect those predictors to be present in the best final model.
