set.seed(1)
e=rnorm(100)
x=rnorm(100)
y=x-2*x^2+e
# n is 100
# p is 2
$\mathbf{Y} = \mathbf{x -\mathbf{2x}^{2} + e} , where e \mathbf{~ N(0,1)} $

b
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/q5.png')
plot(x,y);dev.off()
# There is an inverted u strend in the data centered at 0 and rising at most just above 2

c
set.seed(2)
e=rnorm(100)
x=rnorm(100)
y=x-2*x^2+e
#LOOCV 
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

datX <- data.frame(y,x)
modi <- lm(y~x,data=datX) ; summary(modi)
LOOCVi <- LCV(y,datX[,-1])
datX <- data.frame(y,x,x^2)
modii <- lm(y~x+x.2,data=datX) ; summary(modii)
LOOCVii <- LCV(y,datX[,-1]) 
datX <- data.frame(y,x,x^2,x^3)
modiii <- lm(y~x+x.2+x.3,data=datX) ; summary(modiii)
LOOCViii <- LCV(y,datX[,-1])
datX <- data.frame(y,x,x^2,x^3,x^4) 
modiv <- lm(y~x+x.2+x.3+x.4,data=datX) ; summary(modiv)
LOOCVvi <- LCV(y,datX[,-1])

d
set.seed(45)
e=rnorm(100)
x=rnorm(100)
y=x-2*x^2+e

datX <- data.frame(y,x)
modi <- lm(y~x,data=datX) ; summary(modi)
LOOCVi <- LCV(y,datX[,-1])
datX <- data.frame(y,x,x^2)
modii <- lm(y~x+x.2,data=datX) ; summary(modii)
LOOCVii <- LCV(y,datX[,-1]) 
datX <- data.frame(y,x,x^2,x^3)
modiii <- lm(y~x+x.2+x.3,data=datX) ; summary(modiii)
LOOCViii <- LCV(y,datX[,-1])
datX <- data.frame(y,x,x^2,x^3,x^4) 
modiv <- lm(y~x+x.2+x.3+x.4,data=datX) ; summary(modiv)
LOOCVvi <- LCV(y,datX[,-1])

e
Smallest LOOCV error was for the quadratic model which was as expected since the true mdoel has a quadratic form


f
The linear models beta coefficient was moderately significant.
The quadratic models linear and quadratic components were very significant while the intercept was insignificant
The cubic model had significant linear and quadratic terms but insignificant intercept and cubic coefficients.
The cuartic model had significant linear and quadratic terms with insignificant quartic, intercept and cubic coefficients.
These results agree with the crossvalidation results that showed the least error in the quadratic model. Each of the models fit with at least quadratic terms had only statistically significant  linear and quadratic coefficients. 
