salaries <- read.csv(file = "~/Dropbox//Academic notes//5 A//STAT 444//Assignments//A4//ceosalareis.txt",sep = "")
# Cleaning
salaries<- salaries[-which(salaries$SAL == "*"),]
salaries[,2] <- factor(salaries[,2])
salaries$SAL <- as.numeric(levels(salaries$SAL))[salaries$SAL]
#nvestigate the distribution of salaries using a histogram and a kernel 
# density estimator. 
#Use least squares cross-validation to choose the amount of smoothing. 
# Also consider the Normal reference rule for picking a bandwidth for the
# kernel. There appear to be a few bumps in the density. Are they real? 
# Use confidence bands to address this question. Finally, try using various 
# kernel shapes and comment on the resulting estimates.
par(mfrow = c(1, 3))
hist(x=salaries$SAL,breaks=seq(from=21,to=1150,by=30))
#salary.hist <- ggplot(salaries, aes(x=AGE))
#salary.hist + geom_histogram(binwidth = 1)
LSCV <- function(resp, binwidth){
        n <- length(resp)
        constant <- 2 / ((n - 1)*binwidth)
        term21 <- (n+1)/((n-1)*binwidth)
        bins <- seq(from=summary(resp)[1],to=summary(resp)[6],by= binwidth)
        pk <- rep(0,length(bins)-1)
        for(i in 1:length(bins)-1){
                pk[i] <- sum(resp > bins[i] & resp < bins[i+1])/n
        }
        pk2 <- pk^2
        term22 <- sum(pk2)
        term2 <- term21*term22
        return(constant - term2)
}

lscvs<- seq(15,500,5)
for(i in 1:length(lscvs)){
        lscvs[i]<- LSCV(salaries$SAL,lscvs[i])        
}
plot(lscvs)
which(min(lscvs) == lscvs)
seq(15,500,5)[which(min(lscvs) == lscvs)]
# 95 nearest to zero so use 95
#salary.hist <- ggplot(salaries, aes(x=AGE))
#salary.hist + geom_histogram(binwidth = 3)
hist(x=salaries$SAL,breaks=seq(from=21,to=1246,by=175))

## Normal reference rule is the default for R
normrefrule <- 3.5*sqrt(var(salaries$SAL))*(length(salaries$SAL))^-(1/3)

bins <- seq(from=summary(salaries$SAL)[1] - 1,
            to=summary(salaries$SAL)[6]+1,
            by= normrefrule)
bins <- c(bins,bins[4]+normrefrule)
pk <- rep(0,length(bins)-1)
for(i in 1:length(bins)-1){
        pk[i] <- sum(salaries$SAL > bins[i] & salaries$SAL < bins[i+1])/length(salaries$SAL)
}
fhat <- pk/normrefrule
yfhat <- c(rep(fhat[1],13),rep(fhat[2],17),rep(fhat[3],16),rep(fhat[4],13))
####
maxf <- sqrt(fhat)
c <- (1.96/(2*length(bins)))/2
maxfminc <- maxf - c
maxfminc <- sapply(maxfminc,function(x){ max(x,0)})
l <- (maxf)^2
l <- c(rep(l[1],13),rep(l[2],17),rep(l[3],16),rep(l[4],13))
u <- maxf + c
u <- c(rep(u[1],13),rep(u[2],17),rep(u[3],16),rep(u[4],13))
#bins <- c(rep(bins[1],13),rep(bins[2],17),rep(bins[3],16),rep(bins[4],13))
# salary.hist + geom_histogram(binwidth = normrefrule) +
#         geom_point(aes(x = bins,y =l)) +
#         geom_point(aes(x = bins,y = u)) 
#hist(x=salaries$SAL,breaks=seq(from=2,to=54,by=13))
par(mfrow=c(1,3))
# reference y
## Normal reference rule is the default for R
normrefrule <- 3.5*sqrt(var(salaries$SAL))*(length(salaries$SAL))^-(1/3)

bins <- seq(from=summary(salaries$SAL)[1] - 1,
            to=summary(salaries$SAL)[6]+1,
            by= normrefrule)
bins <- c(bins,bins[4]+normrefrule)
pk <- rep(0,length(bins)-1)
for(i in 1:length(bins)-1){
        pk[i] <- sum(salaries$SAL > bins[i] & salaries$SAL < bins[i+1])/length(salaries$SAL)
}
fhat <- pk/normrefrule
yfhat <- c(rep(fhat[1],13),rep(fhat[2],17),rep(fhat[3],16),rep(fhat[4],13))
#
smooth.matrix<-function(x,h){
        n<-nrow(x)
        A<-matrix(0,n,n)
        for (i in 1:n){
                y<-rep(0,n)
                y[i]=1
                dens <- density(x$SAL,bw=h)
                BAR <- with(dens, approxfun(x = x, y = y))
                yi=with(x, BAR(x$AGE))
                A[,i]=yi
        }
        return((A+t(A))/2)
}
#
h <- seq(50,300,10)
GCVs <- seq(50,300,10)
for(i in 1:length(h)){
        S <- smooth.matrix(salaries,GCVs[i])
        vonn <- sum(diag(S)) / nrow(salaries)
        dens <- density(salaries$SAL,bw=h[i])
        BAR <- with(dens, approxfun(x = x, y = y))
        num <- (yfhat - with(salaries, BAR(salaries$AGE)))
        denom <- 1 - vonn
        quotient <- (num/denom)^2
        GCVs[i]<- (mean(quotient))
}
plot(x=h,y=GCVs)
minh <- h[which(min(GCVs) == GCVs)]
sal.density <- density(salaries$SAL,bw=minh)
plot(sal.density,main="Density of CEO Salaries with GCV determined bandwidth")
#
n <- length(salaries$SAL)
sig <- min(sd(salaries$SAL),IQR(salaries$SAL))
normref <- 1.06*sig*n^-(1/5)
sal.density <- density(salaries$SAL,bw=normref)
plot(sal.density,main="Density plot of CEO Salaries with Normal reference rule")
dn <- cumsum(salaries$SAL)/sum(salaries$SAL)
li <- which(dn>=0.05)[1]
ui <- which(dn>=0.95)[1]
abline(v=salaries$SAL[order(salaries$SAL)][c(li,ui)],lty=2)
axis(side=1, at=c(149,808), labels=c(149,808))
The bumps more or less fall within the confidence bands and so they may not be "real bumps".
par(mfrow=c(2,3))
plot(density(salaries$SAL,bw=normref,kernel="gaussian"),main="Gaussian density")
plot(density(salaries$SAL,bw=normref,kernel="epanechnikov"),main="Epanechinkov density")
plot(density(salaries$SAL,bw=normref,kernel="rectangular"),main="Rectangular density")
plot(density(salaries$SAL,bw=normref,kernel="cosine"),main="Cosine density")
plot(density(salaries$SAL,bw=normref,kernel="biweight"),main="Biweight density")
plot(density(salaries$SAL,bw=normref,kernel="triangular"),main="Triangular density")
Little difference in the density, the rectangular kernel is very noisy but there is little difference between the other kernels for a given bandwith.

