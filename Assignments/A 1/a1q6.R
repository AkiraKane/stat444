#######################################
###      STAT444                    ###
###      Christopher Alert          ###
###      Assignment 1 Question 6    ###
#######################################
set.seed(1)
# a) Generate covariate vector x
x <- rnorm(100,0,1)
# b) Generate error vector
eps <- rnorm(100,0,0.25)
# c) Fit the true model y ~ x + e
y <- -1 + 0.5*x + eps
sprintf("Y is of length: %d.",length(y))
sprintf("Beta 0 is -1 , Beta 1 is 0.5")
# d) Scatter plot y vs x
plot(x,y)
#sprintf("There is a positive linear relationship between x and y with y values centered around -1 and the majority of x values clustred around 0.")

# There is a positive linear relationship between x and y with y values centered around -1 and the 
# majority of x values clustred around 0.

# e) Fit the model and comment
mod.ols1 <- lm(y~x)
summary(mod.ols1) ; anova(mod.ols1)
#sprintf("The model p-values suggest that the coefficients estimated are significant.")
#sprintf("The estimated beta hat 0 and beta hat 1 are extremely close in value to the true model (+/-.001.")

# The model p-values suggest that the coefficients estimated are significant.
# The estimated beta hat 0 and beta hat 1 are extremely close in value to the true model (+/-.001).

# f) Plot the ols regression line and population regression on the scatterplot
# population regression line ?  add a legend()
abline(mod.ols1,col="blue",lty=2)
abline(-1,0.5,col="red")
legend("topleft",title="Plot Legend",
       legend=c("OLS line","Population regression line"),
       lty=c(2,1),
       col=c("blue","red"),
       cex=0.5)
# g)
# check slides for how to polynomial regression
x2 <- x^2
mod.poly<-lm(y~x+x2)
summary(mod.poly) ; anova(mod.poly)
#sprintf("The model p-value for x^2 suggests that the quadratic term is not significant. The adjusted r-square value for the polynomial model was slightly higher than the linear model, however, but the difference is negligible.")

# The model p-value for x^2 suggests that the quadratic term is not significant. The adjusted 
# r-square value for the polynomial model was slightly higher than the linear model, however, 
# but the difference is negligible.

# h) Repeat a to f with less noisy data
set.seed(1)
# i) Generate covariate vector x
x <- rnorm(100,0,1)
# ii) Generate error vector
eps <- rnorm(100,0,0.1)
# iii) Fit the true model y ~ x + e
y <- -1 + 0.5*x + eps
sprintf("Y is of length: %d.",length(y))
sprintf("Beta 0 is -1 , Beta 1 is 0.5")
# iv) Scatter plot y vs x
plot(x,y)
# sprintf("There is a positive linear relationship between x and y with y values tightly centered around -1 and x values still mostly clustered around 0.")

# There is a positive linear relationship between x and y with y values tightly centered around -1 
# and x values still mostly clustered around 0.

# v) Fit the model and comment
mod.ols2 <- lm(y~x)
summary(mod.ols2) ; anova(mod.ols2)
#sprintf("The model p-values suggest that the coefficients estimated are significant.")
#sprintf("The estimated beta hat 0 and beta hat 1 are extremely close in value to the true model (+/-.001.")
#sprintf("Compared to the first model, the intercept is a little farther off (0.003) but still close to the true. Also the adjusted R square is much improved (from .77 to .95)")

# The model p-values suggest that the coefficients estimated are significant.
# The estimated beta hat 0 and beta hat 1 are extremely close in value to the true model (+/-.001).
# Compared to the first model, the intercept is a little farther off (0.003) but still close to the
# true. Also the adjusted R square is much improved (from .77 to .95)

# vi) Plot the ols regression line and population regression on the scatterplot
abline(mod.ols2,col="blue",lty=2)
abline(-1,0.5,col="red")
legend("topleft",title="Plot Legend",
       legend=c("OLS line","Population regression line"),
       lty=c(2,1),,
       col=c("blue","red"),
       cex=0.5)
# i) Repeat a to f with noisier data
set.seed(1)
# i) Generate covariate vector x
x <- rnorm(100,0,1)
# ii) Generate error vector
eps <- rnorm(100,0,0.4)
# iii) Fit the true model y ~ x + e
y <- -1 + 0.5*x + eps
sprintf("Y is of length: %d.",length(y))
sprintf("Beta 0 is -1 , Beta 1 is 0.5")
# iv) Scatter plot y vs x
plot(x,y)
#sprintf("There is a positive linear relationship between x and y with y values randomly scattered around -1 and x values still primarily clustered around 0.")

# There is a positive linear relationship between x and y with y values randomly scattered around -1 
# and x values still primarily clustered around 0.

# v) Fit the model and comment
mod.ols3 <- lm(y~x)
summary(mod.ols3) ; anova(mod.ols3)
#sprintf("The model p-values suggest that the coefficients estimated are significant.")
#sprintf("The estimated beta hat 0 and beta hat 1 are less close in value to the true model than the first two cases (+/-.04.")
#sprintf("Compared to the first model, the intercept is a little farther off (0.02). Also the adjusted R square is much worse (from .77 to .66)")

# The model p-values suggest that the coefficients estimated are significant.
# The estimated beta hat 0 and beta hat 1 are less close in value to the true model than the first 
# two cases (+/-.04).
# Compared to the first model, the intercept is a little farther off (0.02). Also the adjusted R 
# square is much worse (from .77 to .66)

# vi) Plot the ols regression line and population regression on the scatterplot
abline(mod.ols3,col="blue",lty=2)
abline(-1,0.5,col="red")
legend("topleft",title="Plot Legend",
       legend=c("OLS line","Population regression line"),
       lty=c(2,1),,
       col=c("blue","red"),
       cex=0.5)
# j) Confidence intervals for parameters in each case
confint(mod.ols1)
confint(mod.ols2)
confint(mod.ols3)
# sprintf("The confidence intervals for both the intercept and the single covariate are widest for the noisier data, and most narrow for the less noisy data. They original data had confidence intervals of intermediate width.")

# The confidence intervals for both the intercept and the single covariate are widest for the noisier 
# data, and most narrow for the less noisy data. They original data had confidence intervals of 
# intermediate width.