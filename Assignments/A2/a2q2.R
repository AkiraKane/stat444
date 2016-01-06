enzyme.data <- read.csv("~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A 2/")
trans.data <- 1/enzyme.data

fit.prime <- lm(~,data=trans.data)


ii
par.ini=c(b0=(1/fit.prime$coefficient[1]), 
          b1=(fit.prime$coefficient[2]/fit.prime$coefficient[1]))
enzyme.fit=nls(y~(b0*x)/(b1+x),start=par.ini,trace=TRUE)

iii
# Adapt this code
# plot(x,y,type="p",xlab="Days",ylab="Weight (kg)",
#      +       main="Weight loss for an obese patient with polynomial fit",
#      +       xlim=c(0,500),ylim=c(100,200))
#  xx<-0:500
#  b<-c(81.374,102.684,141.911)
#  yy<-b[1]+b[2]*2^(-xx/b[3])
#  points(xx,yy,"l")
# lines(year,fitted.values(fit.pop),lwd=2)

iv
par(mfrow=c(1,2))
plot(y=enzyme.fit$residuals,x=predict(enzyme.fit,newdata=enzyme.data))
plot(y=enzyme.fit$residuals,x=X)
plot(rnorm(18))

v
# An advantage of more replciations in fewer groups would be to see the variability 
# in response within concentrations to get a better idea of how representative
# the sample drawn from any one concentration may be.
# A disadvantage would be the loss of information about a larger variety of concentrations
# and differences between those concentrations. You trade more accuracy of an estimate
# in any one concentration for more varied concentrations being evaluated.

vi
1)confint(enzyme.fit)
2) Test whether or not Beta1 = 20, alpha = 0.05
H0 : beta1 = 20 , h1 : beta 1 != 20 