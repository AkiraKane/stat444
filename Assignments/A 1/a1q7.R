#######################################
###      STAT444                    ###
###      Christopher Alert          ###
###      Assignment 1 Question 7    ###
#######################################
auto.data <- read.csv("~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A 1/Auto.csv")
#View(auto.data)
# a) Scatterplot matrix of all variables
plot(auto.data)
quant.data <- auto.data[,-9]
num.data[,"horsepower"] <- as.numeric(quant.data[,"horsepower"])
num.data <- data.frame(num.data)
# b) Matrix of correlations of numeric variables 
cor(num.data)
# c) Linear model fit and analysis
efficiency.model <- lm(mpg~.,data=num.data)
summary(efficiency.model)
# i. Is there a relationship between the predictors and the response?
# There is a relationship between predictors and the response. In particular, the F statistic
# is very significant suggesting that you reject the idea that there is no relationship
# between the predictors and the response.
#
# ii. Which predictors appear to have a statistically significant relationship to the response?
# Displacement, weight, acceleration, year and origin all have statistically significant 
# p-values suggesting that they have a significant relationship to the response.
#
# iii. What does the coefficient for the year variable suggest?
# The coefficients for year and horsepower suggest strong positive relationships between
# the year and horsepower of the vehicle and the response. In simialr fashion, 
# the weight of the vehicle is strongly negatively related to the response. Acceleration,
# displacement and origin have weaker positive relationships with the response and
# the other variables have non-significant p-values.

# d) Use the plot() function to produce diagnostic plots of the linear regression fit. 
# Comment on any problems you see with the fit. 
attach(num.data)
par(mfrow=c(1,2))
plot(cylinders,mpg)
plot(displacement,mpg)
# Cylinders look like discrete values which doesn't allow for a smooth linear relaiton
# to be fit.
# Displacement seems to have a polynomial relationship with the response.
plot(horsepower,mpg)
plot(weight,mpg)
# Horsepower has two clustered groups of response based on the covariate level. The
# relationshop seems non-linear.
# Weight seems to have a polynomial relaitonship with the response as well.
plot(acceleration,mpg)
plot(year,mpg)
# Year displays a roughly linear relationship with the repsonse despite its discrete 
# nature.
# Acceleration has a roughly linear trend with the response as well but there is 
# large variation.
par(mfrow=c(1,1))
plot(origin,mpg)
# Do the residual plots suggest any unusually large outliers? 
par(mfrow=c(1,3))
plot(fitted(efficiency.model),rstudent(efficiency.model),ylab="studentized residuals",
     xlab="Fitted values")
plot(fitted(efficiency.model),rstandard(efficiency.model),ylab="standardized residuals",
     xlab="Fitted values")
qqnorm(rstudent(efficiency.model))
qqline(rstudent(efficiency.model))
# There do not seem to be any clear outliers though there are a few points that vary from the residual mean. The residuals also look approximately normal.

# plot(cylinders,efficiency.model$residuals,ylab="residuals")
# plot(displacement,efficiency.model$residuals,ylab="residuals")
# # Cylinders seems to have some outlier residuals but looks random for the most part.
# # Displacement's residuals show a non-random looking trend and non-constant variance.
# plot(horsepower,efficiency.model$residuals,ylab="residuals")
# plot(weight,efficiency.model$residuals,ylab="residuals")
# # Horsepower seems to have a few outlier residuals as well but for the most part the 
# # residuals lack a pattern.
# # Weight has an almost quadratic looking pattern to its residuals and the variance 
# # changes across observations. There are also some outliers.
# plot(acceleration,efficiency.model$residuals,ylab="residuals")
# plot(year,efficiency.model$residuals,ylab="residuals")
# par(mfrow=c(1,1))
# plot(origin,mpg)

# Does the leverage plot identify any observations with unusually high leverage?
hat.efficiency <- lm.influence(efficiency.model)$hat
cbind(lev=hat.efficiency)[hat.efficiency > 3*mean(hat.efficiency)]
par(mfrow=c(1,1))
plot(hat.efficiency)
identify(hat.efficiency)
# There is one high leverage point based on the plot of the leverage values. This point corresponds to the buick estate wagon (sw) and has leverage 0.1336 which is the most leverage of all the observed cars.
# e) Use the * and : symbols to fit linear regression models with interaction effects.
# Do any interactions appear to be statistically significant?
interaction.mod <- lm(mpg~.*.,data=num.data)
summary(interaction.mod)
# Displacement, acceleration, origin, acceleration:year and acceleration:origin
# all have statistically significant effects. The Adjusted R squared value is 
# .87 - a better R squared value than the original model. 
interaction.mod <- lm(mpg~cylinders*displacement*horsepower*weight*acceleration*year*origin,data=num.data)
summary(interaction.mod)
# The model fitting all interaction effects has a high R squared of 0.92 
# however, many of the terms are not statistically significant. The terms which
# had significant p-values were: cylinders:origin, cylinders:year:origin,cylinders:displacement:weight:origin  and cylinders:weight:acceleration:origin. This model could potentially be overfit to the data due to the large number of covariates used.
# f) Try a few different transformations of the variables, 
# such as log(X), √X, X2. Comment on your findings.
# Compare adjsuted r square values. Compare coefficient p-values, F values,
# compare effect sizes
log.efficiency.model <- lm(mpg~.,data=log(num.data))
summary(log.efficiency.model)
# The coefficients for weight and year are statistically significant but every other
# covariate's linear relaitonship is weakened by the log transform.
#
root.data <- num.data^0.5 ; root.data <- data.frame(root.data)
root.efficiency.model <- lm(mpg~.,data=root.data)
summary(root.efficiency.model)
# The weight, year and origin variables are significant. The root model has an R squared value of .86. 
#
sq.data <- num.data^2 ; sq.data <- data.frame(sq.data)
sq.efficiency.model <- lm(mpg~.,data=sq.data)
summary(sq.efficiency.model)
# When the quadratic data is fit, many new variables show statistical significance. This could be related to the polynomial type relationship between the response and several covariates discovered in the exploratory plots. This model has significant cylinders, displacement, horsepower, weight, acceleration, year and origin covariates. The Adjusted R square is .70, however, it is not a linear fit so that may not be the best metric for comparison.