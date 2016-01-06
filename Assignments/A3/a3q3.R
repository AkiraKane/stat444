glass <- read.csv("~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A3/glass.dat",sep="")
Y <- glass[,1]
x <- glass[,4]

# For each use CV to smooth, estimate variance, 95% confidence bands
# picka few values of x and for each,  plot the effective kernel for each smoothing method and visually compare them
#regressogram
cs <- c(-Inf,quantile(x,probs=c(1/5,2/5,3/5,4/5)),Inf)
cs1 <- c(-Inf,seq(0.29,3.6,1),Inf)
cs2 <- c(-Inf,seq(0.29,3.6,0.5),Inf)
cs3 <- c(-Inf,seq(0.29,3.6,0.25),Inf)
cs4 <- c(-Inf,seq(0.29,3.6,0.05),Inf)
cs5 <- c(-Inf,seq(0.29,3.6,0.01),Inf)
o <- order(x)
R <- cut(x,cs3)
plot(x,Y,main="Bin Smoother",pch=".")
lines(x[o],lm(Y~R)$fitted[o])
## GCV  for regressogram - cite http://wwwf.imperial.ac.uk/~bm508/teaching/AppStats/Lecture7.pdf
regresso.gcv <- function(y,x,cs) {
        n <- length(y)
        R <- cut(x,cs)
        denom <- 1 - length(summary(R))/n
        num <- y - lm(y~R)$fitted
        GCV <- mean((num/denom)^2)
        return(GCV)
}
> regresso.gcv(Y,x,cs)
[1] 7.713926
> regresso.gcv(Y,x,cs1)
[1] 8.3945
> regresso.gcv(Y,x,cs2)
[1] 7.825354
> regresso.gcv(Y,x,cs3)
[1] 7.007995
> regresso.gcv(Y,x,cs4)
[1] 9.049001
> regresso.gcv(Y,x,cs5)
[1] 7.603488
# CS3 has the smallest GCV and so we choose that bin width
> names(summary(R))
"(-Inf,0.29]" "(0.29,0.54]" "(0.54,0.79]" "(0.79,1.04]" "(1.04,1.29]" "(1.29,1.54]" "(1.54,1.79]" "(1.79,2.04]"
[9] "(2.04,2.29]" "(2.29,2.54]" "(2.54,2.79]" "(2.79,3.04]" "(3.04,3.29]" "(3.29,3.54]" "(3.54, Inf]"
## Variance Regressogram
simple.variance <- function(y){
        for(i in 1:(length(y)-1)){ 
                s <- (Y[i+1] - Y[i])^2
        }
        s <-mean(s)
        return(s/2)
}
o<-order(x)
Yo<-Y[o]
binsigma2hat1 <- simple.variance(Yo)
# Regressogram smoother
bin.smooth.matrix<-function(y,bins,ordering,x){
        n<-length(y)
        A<-matrix(0,n,n)
        for (i in 1:n){
                y<-rep(0,n)
                y[i]=1
                bin.sm <- lm(y~bins)
                yi= lm(y~bins)$fitted[ordering]
                #yi <- yi[ordering]
                A[,i]=yi
        }
        return((A+t(A))/2)
}
S <- bin.smooth.matrix(Y,R,o,x)
##Estimating variance with the first variance formula
num<-sum((Y-lm(Y~R)$fitted)^2)
denum<-length(x)-2*sum(diag(S))+sum(diag(t(S)%*%S))
binsigma2hat2<-num/denum
norm.s <- norm(S)
#for(i in nrow(S)){ norm.s[i] <- sqrt(sum(S[i,]^2))}
##
# 95% confidence band
lines(x[o],lm(Y~R)$fitted[o]+qnorm(0.975)*sqrt(binsigma2hat2*norm.s),col=2,lwd=3,lty=2)
lines(x[o],lm(Y~R)$fitted[o]-qnorm(0.975)*sqrt(binsigma2hat2*norm.s),col=2,lwd=3,lty=2)
##
smooth.matrix<-function(x,df){
        n<-length(x)
        A<-matrix(0,n,n)
        for (i in 1:n){
                y<-rep(0,n)
                y[i]=1
                yi=predict(smooth.spline(x,y,df=df),x)$y
                A[,i]=yi
        }
        return((A+t(A))/2)
}
## Effective kernel
Yi <- rep(Y[200],length(x))
Si <- S[200,]
plot(Si,Yi)


#kernel
plot(x[o],Y[o],main="Kernel Smoother",pch=".")
aux <- ksmooth(x[o],Y[o],kernel="normal",bandwidth=0.35)
lines(aux$x,aux$y)
# GCV kernel - cite : http://wwwf.imperial.ac.uk/~bm508/teaching/AppStats/Lecture7.pdf

## Variance kernel
o<-order(x)
Yo<-Y[o]
kernsigma2hat1 <- simple.variance(Yo)
# Kernel smoother
k.smooth.matrix<-function(y,band,ordering,x){
        n<-length(y)
        A<-matrix(0,n,n)
        for (i in 1:n){
                y<-rep(0,n)
                y[i]=1
                kern <- ksmooth(x[ordering],y[ordering],kernel="normal",
                                bandwidth=band)
                yi= kern$y
                A[,i]=yi
        }
        return((A+t(A))/2)
}
S <- k.smooth.matrix(Y,0.35,o,x)
##Estimating variance with the first variance formula
num<-sum((Y-aux$y)^2)
denum<-length(x)-2*sum(diag(S))+sum(diag(t(S)%*%S))
ksigma2hat2<-num/denum
norm.s <- norm(S)
#for(i in nrow(S)){ norm.s[i] <- sqrt(sum(S[i,]^2))}
##
# 95% confidence band
lines(x[o],aux$y[o]+qnorm(0.975)*sqrt(ksigma2hat2*norm.s),col=2,lwd=3,lty=2)
lines(x[o],aux$y[o]-qnorm(0.975)*sqrt(ksigma2hat2*norm.s),col=2,lwd=3,lty=2)
##
smooth.matrix<-function(x,df){
        n<-length(x)
        A<-matrix(0,n,n)
        for (i in 1:n){
                y<-rep(0,n)
                y[i]=1
                yi=predict(smooth.spline(x,y,df=df),x)$y
                A[,i]=yi
        }
        return((A+t(A))/2)
}
## Effective kernel
Yi <- rep(Y[22],length(x))
Si <- S[22,]
plot(Si,Yi)
#local linear
library(locfit)
h<-seq(0.29,3.5,0.01)
alphamatrix<-matrix(0,ncol=2,nrow=length(h))
alphamatrix[,2]<- h
## compute GCV scores for all values given by h
gcvYlocfit<-gcvplot(Y~x,alpha=alphamatrix,deg=1)
###### Find the optimal h
opt.h<-max(gcvYlocfit$alpha[gcvYlocfit$values==
                                      min(gcvYlocfit$values),2])
###### perform locfit with optimal bandwidth h
Ylocfit.opt<-locfit(Y~x,alpha=c(0,opt.h),deg=1)
plot(x,Y,xlab="x since seroconversion",ylab="Y",
     main="Local linear regression with optimum h")
lines(Ylocfit.opt,lwd=3,col="blue")
#
# Fit local linear to estimate variance function
Z=log((Y-predict(Ylocfit.opt,x))^2)
h<-seq(0.29,3.5,0.01)
alphamatrix<-matrix(0,ncol=2,nrow=length(h))
alphamatrix[,2]<-h
gcvsig2locfit<-gcvplot(Z~x,alpha=alphamatrix,deg=1) 
opt.h<-max(gcvsig2locfit$alpha[gcvsig2locfit$values==min(gcvsig2locfit$values),2])
Zlocfit<-locfit(Z~x,alpha=c(0,opt.h),deg=1)
Zlocfitpred<-predict(Zlocfit,where="data") 
Sigma2f<-exp(Zlocfitpred)
plot(x[o],Sigma2f[o],xlab="x",ylab="Variance", main="Local linear estimate of variance function","l",lwd=2)
nu1<-as.numeric(Ylocfit.opt$dp[6])
nu2<-as.numeric(Ylocfit.opt$dp[7]) 
lfsigma2hat1<-sum(residuals(Ylocfit.opt)^2)/(length(x)-2*nu1+nu2)
lfsigma2hat1
diaghat<-predict(Ylocfit.opt,where="data",what="inf1")
norm.s<-predict(Ylocfit.opt,where="data",what="vari") 
critval<-kappa0(Ylocfit.opt,cov=0.975)$crit.val 
locfitYpred<-predict(Ylocfit.opt,where="data")
##############Pointwise bands
locfitYpred <- predict(Ylocfit.opt,x)
plot(x[o],locfitYpred[o],lwd=3,xlab="x",ylab="Y",
     main="Local linear regression fit, opt.h=0.72, and 95% conf. band",
     cex=3,cex.axis=1.3,,ylim=c(-6,6),cex.lab=1.3,type="l") 
lines(x[o],locfitYpred[o]+qnorm(0.975)*sqrt(lfsigma2hat1*norm.s[o]),col=2,lwd=3,lty=2)
lines(x[o],locfitYpred[o]-qnorm(0.975)*sqrt(lfsigma2hat1*norm.s[o]),col=2,lwd=3,lty=2) 
legend(c(1,5),lwd=c(3,3),lty=c(1,2),col=c(1,2),
       c("estimate","95% pointwise band, constant variance"))
######## Simultaneous bands
plot(x[o],locfitYpred[o],lwd=3,ylim=c(-6,6),xlab="x",ylab="Y",main="Local linear regression fit, opt.h=0.72, and 95% conf. band",
     cex=3,cex.axis=1.3,cex.lab=1.3,type="l")
lines(x[o],locfitYpred[o]+critval*sqrt(lfsigma2hat1*norm.s[o]),col=2,lwd=3,lty=2)
lines(x[o],locfitYpred[o]-critval*sqrt(lfsigma2hat1*norm.s[o]),col=2,lwd=3,lty=2)
legend(c(1,5),c(1,5),lwd=c(3,3),lty=c(1,2),col=c(1,2),c("estimate","95% simultaneous band, constant variance"))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
##
smooth.matrix<-function(x,df){
        n<-length(x)
        A<-matrix(0,n,n)
        for (i in 1:n){
                y<-rep(0,n)
                y[i]=1
                yi=df$y
                A[,i]=yi
        }
        return((A+t(A))/2)
}
S <- smooth.matrix(x,locfitYpred)
## Effective kernel
Yi <- rep(Y[22],length(x))
Si <- S[22,]
plot(Si,Yi)

# cubic spline 
# fit smoothing spline
splinefit1<-smooth.spline(x,Y,cv=FALSE,
                          all.knots=FALSE,nknots=15)
# The scatter plot of the data
plot(x,Y,xlab="x since ",
     ylab="Y",main="Cubic spline fit with 15 knots")
# added the fitted curve to the scatter plot
lines(splinefit1,lwd=3,col="blue")
#######
smooth.matrix<-function(x,df){
        n<-length(x)
        A<-matrix(0,n,n)
        for (i in 1:n){
                y<-rep(0,n)
                y[i]=1
                yi=predict(smooth.spline(x,y,df=df),x)$y
                A[,i]=yi
        }
        return((A+t(A))/2)
}
splinefit1<-smooth.spline(x,Y,cv=FALSE,
                          all.knots=FALSE,nknots=15)
df<-splinefit1$df
S<-smooth.matrix(x,df)
## Variance Regressogram
# Cubic Spline smoother
##Estimating variance with the first variance formula
num<-sum((Y-lm(Y~R)$fitted)^2)
denum<-length(x)-2*sum(diag(S))+sum(diag(t(S)%*%S))
cubsigma2hat2<-num/denum
norm.s <- norm(S)
#for(i in nrow(S)){ norm.s[i] <- sqrt(sum(S[i,]^2))}
##
# 95% confidence band
lines(x[o],predict(smooth.spline(x,Y,df=df),x)$y[o]+qnorm(0.975)*sqrt(cubsigma2hat2*norm.s),col=2,lwd=3,lty=2)
lines(x[o],predict(smooth.spline(x,Y,df=df),x)$y[o]-qnorm(0.975)*sqrt(cubsigma2hat2*norm.s),col=2,lwd=3,lty=2)
## Effective kernel
Yi <- rep(Y[22],length(x))
Si <- S[22,]
plot(Si,Yi)
