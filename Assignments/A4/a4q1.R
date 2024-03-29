library(MASS); library(wavethresh)library(waveslim)
# Wavelets to regress acceleration ~ time w/ soft and hard thresh
# universal, sure and cv selection
# plot estimates
x<-mcycle$times
Y<-mcycle$accel
set.seed(1)
Ytrans <- Y[1:128]
xtrans <- x[1:128]
set.seed(1)
par(mfrow=c(1,1))
plot(xtrans,Ytrans,ylab="Acceleration",main="Scatter plot of mcycle data")
lines(xtrans,Ytrans,lwd=3,col="blue")
# Universal
par(mfrow=c(3,2))
# soft thresholding with universal tuning using function threshold() 
Ywd<-wd(Ytrans,filter.number=1,family="DaubExPhase")
plot(Ywd,main="wavelet coefficients")
accelsoft<-threshold(Ywd,type="soft",policy="universal")
# inverting the wavelet transformatuion to get the fitted values # using function wr()
unifitted<-wr(accelsoft)
plot(xtrans,Ytrans,ylab="Y",main="scatter plot & wavelet soft thresholding")
lines(xtrans,unifitted,lwd=3,col="blue")
plot(xtrans,unifitted,ylab="Y",main="Wavelet soft thresholding", type="l",lwd=3,col="blue")
## hard thresholding with universal tuning using function threshold() 
hardthresh<-threshold(Ywd,type="hard",policy="universal")
# inverting the wavelet transformatuion to get the fitted values using function wr() 
hardfit<-wr(hardthresh)
plot(xtrans,Ytrans,ylab="Y",main="scatter plot & wavelet hard thresholding") 
lines(xtrans,hardfit,lwd=3,col="blue")
plot(xtrans,hardfit,ylab="Y",main="Wavelet hard thresholding", type="l",lwd=3,col="blue")
# SURE
par(mfrow=c(3,2))
# soft thresholding with sure tuning using function threshold() 
Ywd<-wd(Ytrans,filter.number=1,family="DaubExPhase")
plot(Ywd,main="wavelet coefficients")
accelsoft<-threshold(Ywd,type="soft",policy="sure")
# inverting the wavelet transformatuion to get the fitted values # using function wr()
unifitted<-wr(accelsoft)
plot(xtrans,Ytrans,ylab="Y",main="scatter plot & wavelet soft thresholding")
lines(xtrans,unifitted,lwd=3,col="blue")
plot(xtrans,unifitted,ylab="Y",main="Wavelet soft thresholding", type="l",lwd=3,col="blue")
## hard thresholding with universal tuning using function threshold() 
hardthresh<-threshold(Ywd,type="hard",policy="universal")
# inverting the wavelet transformatuion to get the fitted values using function wr() 
hardfit<-wr(hardthresh)
plot(xtrans,Ytrans,ylab="Y",main="scatter plot & wavelet hard thresholding") 
lines(xtrans,hardfit,lwd=3,col="blue")
plot(xtrans,hardfit,ylab="Y",main="Wavelet hard thresholding", type="l",lwd=3,col="blue")
# CV
par(mfrow=c(3,2))
# soft thresholding with cv tuning using function threshold() DopplerSoft<-threshold(Ywd,type="soft",policy="cv")
Ywd<-wd(Ytrans,filter.number=1,family="DaubExPhase")
plot(Ywd,main="wavelet coefficients")
accelsoft<-threshold(Ywd,type="soft",policy="cv")
# inverting the wavelet transformatuion to get the fitted values # using function wr()
unifitted<-wr(accelsoft)
plot(xtrans,Ytrans,ylab="Y",main="scatter plot & wavelet soft thresholding")
lines(xtrans,unifitted,lwd=3,col="blue")
plot(xtrans,unifitted,ylab="Y",main="Wavelet soft thresholding", type="l",lwd=3,col="blue")
## hard thresholding with universal tuning using function threshold() 
hardthresh<-threshold(Ywd,type="hard",policy="universal")
# inverting the wavelet transformatuion to get the fitted values using function wr() 
hardfit<-wr(hardthresh)
plot(xtrans,Ytrans,ylab="Y",main="scatter plot & wavelet hard thresholding") 
lines(xtrans,hardfit,lwd=3,col="blue")
plot(xtrans,hardfit,ylab="Y",main="Wavelet hard thresholding", type="l",lwd=3,col="blue")

