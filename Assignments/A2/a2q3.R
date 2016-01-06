library(nlstools)
enzyme.data <- read.csv("~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/Enzyme.txt",sep="")
trans.data <- 1/enzyme.data

fit.prime <- lm(Y~x,data=trans.data)


ii
par.ini=c(b0=(1/fit.prime$coefficient[1]), 
          b1=(fit.prime$coefficient[2]/fit.prime$coefficient[1]))
names(par.ini) <- c("b0","b1")
Y <- enzyme.data$Y ; X <- enzyme.data$x
enzyme.fit=nls(Y~(b0*X)/(b1+X),start=par.ini,trace=TRUE)

iii
# Adapt this code
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/q3.png')
plot(X,Y,type="p",xlab="X",ylab="Response (Y)",
     main="Enzymes non-linear fit",
     xlim=c(0,50),ylim=c(0,23))
xx<-0:50
b<-c(28.13705,12.57445)
yy<-b[1]*xx/(b[2]+xx)
points(xx,yy,"l")
dev.off()
iv
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/q3i.png')
plot(y=nlsResiduals(enzyme.fit)$resi1[,2],x=nlsResiduals(enzyme.fit)$resi1[,1]); dev.off()
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/q3ii.png')
plot(y=nlsResiduals(enzyme.fit)$resi1[,2],x=X);dev.off()
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/q3iii.png')
plot(rnorm(18)) ;dev.off()

v
An advantage of more replciations in fewer groups would be to see the variability 
in response within concentrations to get a better idea of how representative
the sample drawn from any one concentration may be.
A disadvantage would be the loss of information about a larger variety of concentrations
and differences between those concentrations. You trade more accuracy of an estimate
in any one concentration for more varied concentrations being evaluated.

vi
1)confint(enzyme.fit)
2) Test whether or not Beta1 = 20, alpha = 0.05
H0 : beta1 = 20 , h1 : beta 1 != 20 
Beta 1 - 20 / t* se(beta1)
The alternative hypothesis is that Beta != 20 and based on the confidence interval, the interval does not contain 20 with 95% confidence , hence, we reject H0.


