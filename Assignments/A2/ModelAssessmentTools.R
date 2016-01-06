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

# AIC (to minimize) - built-in funciton AIC
myAIC <- function(model) {
        
        MSSresid <- mean((model$residuals)^2)
        AIC <- length(model$residuals)* log(MSSresid) + 2*(length(model$coefficients))   
        return(AIC)
}        

# BIC (to minimize) - built-in function BIC
myBIC <- function(model) {
        n <- length(model$residuals)
        MSSresid <- mean((model$residuals)^2)
        BIC <- n*log(MSSresid) + log(n)*(length(model$coefficients))            
        return(BIC)
}        
# Mallow's Ck
# Assuming data has the response in column 1 and the other columns
# are the predictors
myMallows <- function(data,model) {
        n <- length(model$residuals)
        pmod <- lm(data[,1]~.,data=data)
        MSSresid <- mean((pmod$residuals)^2)
        RSS <- sum((model$residuals)^2)
        Mallows <- RSS/MSSresid - (2*(length(model$coefficients)))
        return(Mallows)
} 

# SURE 
mySURE <- function(model) {
        n <- length(model$residuals)
        RSS <- sum((model$residuals)^2)
        SURE <- RSS - (n - 2*length(model$coefficients))*RSS            
        return(SURE)
}     

# Adjusted R square
myArS <- function(data,model) {
        
        n <- length(model$residuals)
        RSS <- sum((model$residuals)^2)
        pp1 <- length(model$coefficients)
        TSS <- sum((data[,1] - mean(data[,1]))^2)
        ArS <- 1 - (RSS/(n-pp1))/ (TSS/(n-1))
        return(ArS)
}