library(MASS)
par(mfrow=c(1,2))
summary(Boston) ; attach(Boston) 
# Univariate models
znmod <- lm(crim~zn,data=Boston)
summary(znmod) ; anova(znmod) # Significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot1.png')
plot(y=crim,x=zn) 

abline(znmod,col="blue",lty=2)
legend("topleft",title="Plot Legend",
       legend=c("OLS line"),
       lty=c(2),
       col=c("blue"),
       cex=0.5) ; dev.off() 
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot2.png')
plot(znmod$resid)   
dev.off()

indusmod <- lm(crim~indus,data=Boston)
summary(indusmod) ; anova(indusmod) #significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot3.png')
plot(y=crim,x=indus) 
abline(indusmod,col="blue",lty=2)
legend("topleft",title="Plot Legend",
       legend=c("OLS line"),
       lty=c(2),
       col=c("blue"),
       cex=0.5) ; dev.off() 
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot4.png')
plot(indusmod$resid)
dev.off()

chasmod <- lm(crim~chas,data=Boston)
summary(chasmod) ; anova(chasmod) # Insignificant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot5.png')
plot(y=crim,x=chas) 

abline(chasmod,col="blue",lty=2)
legend("topleft",title="Plot Legend",
       legend=c("OLS line"),
       lty=c(2),
       col=c("blue"),
       cex=0.5) ; dev.off()
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot6.png')
plot(chasmod$resid) # The crime rate changes completely independently of the chas dummy variable
dev.off()

noxmod <- lm(crim~nox,data=Boston)
summary(noxmod) ; anova(noxmod) # Significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot7.png')
plot(y=crim,x=nox) 

abline(noxmod,col="blue",lty=2)
legend("topleft",title="Plot Legend",
       legend=c("OLS line"),
       lty=c(2),
       col=c("blue"),
       cex=0.5)  ; dev.off()
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot8.png')
plot(noxmod$resid)
dev.off()
rmmod <- lm(crim~rm,data=Boston)
summary(rmmod) ; anova(rmmod) # Significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot9.png')
plot(y=crim,x=rm) 
abline(rmmod,col="blue",lty=2)
legend("topleft",title="Plot Legend",
       legend=c("OLS line"),
       lty=c(2),
       col=c("blue"),
       cex=0.5) ; dev.off()
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot10.png')
plot(rmmod$resid)
dev.off()

agemod <- lm(crim~age,data=Boston)
summary(agemod) ; anova(agemod) #Significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot11.png')
plot(y=crim,x=age) 
abline(agemod,col="blue",lty=2)
legend("topleft",title="Plot Legend",
       legend=c("OLS line"),
       lty=c(2),
       col=c("blue"),
       cex=0.5) ; dev.off()
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot12.png')
plot(agemod$resid) ; dev.off()

dismod <- lm(crim~dis,data=Boston)
summary(dismod) ; anova(dismod) # Significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot13.png')
plot(y=crim,x=dis) 
abline(dismod,col="blue",lty=2)
legend("topleft",title="Plot Legend",
       legend=c("OLS line"),
       lty=c(2),
       col=c("blue"),
       cex=0.5)  ; dev.off()
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot14.png')
plot(dismod$resid) ; dev.off()

radmod <- lm(crim~rad,data=Boston)
summary(radmod) ; anova(radmod) # Significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot15.png')
plot(y=crim,x=rad) 
abline(radmod,col="blue",lty=2)
legend("topleft",title="Plot Legend",
       legend=c("OLS line"),
       lty=c(2),
       col=c("blue"),
       cex=0.5)  ; dev.off()
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot16.png')
plot(radmod$resid) ; dev.off()

taxmod <- lm(crim~tax,data=Boston)
summary(taxmod) ; anova(taxmod) # Significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot17.png')
plot(y=crim,x=tax) 
abline(taxmod,col="blue",lty=2)
legend("topleft",title="Plot Legend",
       legend=c("OLS line"),
       lty=c(2),
       col=c("blue"),
       cex=0.5) ; dev.off()
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot18.png')
plot(taxmod$resid) ; dev.off()

ptratiomod <- lm(crim~ptratio,data=Boston)
summary(ptratiomod) ; anova(ptratiomod) # Significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot19.png')
plot(y=crim,x=ptratio) 
abline(ptratiomod,col="blue",lty=2)
legend("topleft",title="Plot Legend",
       legend=c("OLS line"),
       lty=c(2),
       col=c("blue"),
       cex=0.5) ; dev.off()
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot20.png')
plot(ptratiomod$resid) ; dev.off()

bmod <- lm(crim~black,data=Boston)
summary(bmod) ; anova(bmod) # Significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot21.png')
plot(y=crim,x=black) ; dev.off()
abline(bmod,col="blue",lty=2)
legend("topleft",title="Plot Legend",
       legend=c("OLS line"),
       lty=c(2),
       col=c("blue"),
       cex=0.5) ; dev.off()
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot22.png')
plot(bmod$resid) ; dev.off()

lstatmod <- lm(crim~lstat,data=Boston)
summary(lstatmod) ; anova(lstatmod) # Significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot23.png')
plot(y=crim,x=lstat) ; dev.off()
abline(lstatmod,col="blue",lty=2)
legend("topleft",title="Plot Legend",
       legend=c("OLS line"),
       lty=c(2),
       col=c("blue"),
       cex=0.5) ; dev.off()
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot24.png')
plot(lstatmod$resid) ; dev.off()

medvmod <- lm(crim~medv,data=Boston)
summary(medvmod) ; anova(medvmod) # Significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot25.png')
plot(y=crim,x=medv)  ; dev.off()
abline(medvmod,col="blue",lty=2)
legend("topleft",title="Plot Legend",
       legend=c("OLS line"),
       lty=c(2),
       col=c("blue"),
       cex=0.5) ; dev.off()
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot26.png')
plot(medvmod$resid) ; dev.off()

# Multiple Regression model
multimod <- lm(crim~.,data=Boston)
summary(multimod)
# The variables chas, rm, age, nox, tax, lstat and ptratio were all not statistically significant in the multiple regression model. Of these, chas was the only variable initially insignificant when fit alone against crime.  This means we could reject H0 that Bj = 0 for each predictor j in {zn,indus,dis,rad,black,medv}. Of the significant variables, zn and rad are positively correlated with crime while indus, dis, black and medv were negatively correlated with crime.
beta.uni <- c(znmod$coefficients[-1],indusmod$coefficients[-1],chasmod$coefficients[-1],
              noxmod$coefficients[-1],rmmod$coefficients[-1],agemod$coefficients[-1],
              dismod$coefficients[-1],radmod$coefficients[-1],taxmod$coefficients[-1],
              ptratiomod$coefficients[-1],bmod$coefficients[-1],lstatmod$coefficients[-1],
              medvmod$coefficients[-1])
beta.multi <- multimod$coefficients[-1] 
predictors <- c("zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b",
                "lstat","medv")
par(mfrow=c(1,2))
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot27.png')
plot(x=beta.uni,y=beta.multi,xlab="Univariate regression Betas",ylab="Multiple regresion Betas")
#plot(x=beta.uni[-4],y=beta.multi[-4],xlab="Univariate regression Betas without nox",ylab="Multiple regresion Betas without nox")
dev.off()
# Most coefficients looked more or less the same
# NonLinear fits
par(mfrow=c(1,1))
Boston3 <- data.frame(Boston,Boston^2,Boston^3)
znmod <- lm(crim~zn+zn.1+zn.2,data=Boston3)
summary(znmod)  # Significant linear term only
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot28.png')
plot(znmod$resid)   ; dev.off()

indusmod <- lm(crim~indus+indus.1+indus.2,data=Boston3)
summary(indusmod) #significant linear, quadratic and cubic terms
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot29.png')
plot(indusmod$resid) ; dev.off()

chasmod <- lm(crim~chas+chas.1+chas.2,data=Boston3)
summary(chasmod) # Insignificant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot30.png')
plot(chasmod$resid) ; dev.off() # The crime rate changes completely independently of the chas dummy variable

noxmod <- lm(crim~nox+nox.1+nox.2,data=Boston3)
summary(noxmod) # Significant linear, quadratic and cubic terms
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot31.png')
plot(noxmod$resid) ; dev.off()

rmmod <- lm(crim~rm+rm.1+rm.2,data=Boston3)
summary(rmmod) # Insignificant in al three terms
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot32.png')
plot(rmmod$resid) ; dev.off()

agemod <- lm(crim~age+age.1+age.2,data=Boston3)
summary(agemod) #Significant quadratic and cubic terms
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot33.png')
plot(agemod$resid) ; dev.off()

dismod <- lm(crim~dis+dis.1+dis.2,data=Boston3)
summary(dismod) # Significant quadratic and cubic terms
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot34.png')
plot(dismod$resid) ; dev.off()

radmod <- lm(crim~rad+rad.1+rad.2,data=Boston3)
summary(radmod) # None of the terms are significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot35.png')
plot(radmod$resid)  ; dev.off()

taxmod <- lm(crim~tax+tax.1+tax.2,data=Boston3)
summary(taxmod) # None of the terms are significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot36.png')
plot(taxmod$resid) ; dev.off()

ptratiomod <- lm(crim~ptratio+ptratio.1+ptratio.2,data=Boston3)
summary(ptratiomod) # Significant linear, quadratic and cubic trms
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot37.png')
plot(ptratiomod$resid) ; dev.off()

bmod <- lm(crim~black+black.1+black.2,data=Boston3)
summary(bmod) # No non-linear relationship of significance
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot38.png')
plot(bmod$resid) ; dev.off()

lstatmod <- lm(crim~lstat+lstat.1+lstat.2,data=Boston3)
summary(lstatmod) # No non linar relationship of significance
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot39.png')
plot(lstatmod$resid) ; dev.off()

medvmod <- lm(crim~medv+medv.1+medv.2,data=Boston3)
summary(medvmod) # Significant in linear, quadratic and cubic terms
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot40.png')
plot(medvmod$resid) ; dev.off()

#b)
# Univariate models
znmod <- lm(medv~zn,data=Boston)
summary(znmod) # Significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot41.png')
plot(y=medv,x=zn) 
abline(znmod,col="blue",lty=2)
legend("topleft",title="Plot Legend",
       legend=c("OLS line"),
       lty=c(2),
       col=c("blue"),
       cex=0.5) ; dev.off()
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot42.png')
plot(znmod$resid)   ; dev.off()

indusmod <- lm(medv~indus,data=Boston)
summary(indusmod) #significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot43.png')
plot(y=medv,x=indus)  
abline(indusmod,col="blue",lty=2)
legend("topleft",title="Plot Legend",
       legend=c("OLS line"),
       lty=c(2),
       col=c("blue"),
       cex=0.5) ; dev.off()
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot44.png')
plot(indusmod$resid); dev.off()

chasmod <- lm(medv~chas,data=Boston)
summary(chasmod) # Significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot45.png')
plot(y=medv,x=chas) 
abline(chasmod,col="blue",lty=2)
legend("topleft",title="Plot Legend",
       legend=c("OLS line"),
       lty=c(2),
       col=c("blue"),
       cex=0.5) ; dev.off()
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot46.png')
plot(chasmod$resid) ; dev.off()

noxmod <- lm(medv~nox,data=Boston)
summary(noxmod) # Significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot47.png')
plot(y=medv,x=nox) 
abline(noxmod,col="blue",lty=2)
legend("topleft",title="Plot Legend",
       legend=c("OLS line"),
       lty=c(2),
       col=c("blue"),
       cex=0.5) ; dev.off()
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot48.png')
plot(noxmod$resid); dev.off()

rmmod <- lm(medv~rm,data=Boston)
summary(rmmod) # Significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot49.png')
plot(y=medv,x=rm) 
abline(rmmod,col="blue",lty=2)
legend("topleft",title="Plot Legend",
       legend=c("OLS line"),
       lty=c(2),
       col=c("blue"),
       cex=0.5) ; dev.off()
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot50.png')
plot(rmmod$resid) ; dev.off()

agemod <- lm(medv~age,data=Boston)
summary(agemod) #Significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot51.png')
plot(y=medv,x=age) 
abline(agemod,col="blue",lty=2)
legend("topleft",title="Plot Legend",
       legend=c("OLS line"),
       lty=c(2),
       col=c("blue"),
       cex=0.5) ; dev.off()
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot52.png')
plot(agemod$resid);dev.off()

dismod <- lm(medv~dis,data=Boston)
summary(dismod)# Significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot53.png')
plot(y=medv,x=dis) 
abline(dismod,col="blue",lty=2)
legend("topleft",title="Plot Legend",
       legend=c("OLS line"),
       lty=c(2),
       col=c("blue"),
       cex=0.5) ; dev.off()
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot54.png')
plot(dismod$resid);dev.off()

radmod <- lm(medv~rad,data=Boston)
summary(radmod) # Significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot55.png')
plot(y=medv,x=rad) 
abline(radmod,col="blue",lty=2)
legend("topleft",title="Plot Legend",
       legend=c("OLS line"),
       lty=c(2),
       col=c("blue"),
       cex=0.5) ; dev.off()
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot56.png')
plot(radmod$resid); dev.off()

taxmod <- lm(medv~tax,data=Boston)
summary(taxmod) # Significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot57.png')
plot(y=medv,x=tax) 
abline(taxmod,col="blue",lty=2)
legend("topleft",title="Plot Legend",
       legend=c("OLS line"),
       lty=c(2),
       col=c("blue"),
       cex=0.5) ; dev.off()
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot58.png')
plot(taxmod$resid); dev.off()

ptratiomod <- lm(medv~ptratio,data=Boston)
summary(ptratiomod) # Significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot59.png')
plot(y=medv,x=ptratio) 
abline(ptratiomod,col="blue",lty=2)
legend("topleft",title="Plot Legend",
       legend=c("OLS line"),
       lty=c(2),
       col=c("blue"),
       cex=0.5) ; dev.off()
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot60.png')
plot(ptratiomod$resid); dev.off()

bmod <- lm(medv~black,data=Boston)
summary(bmod) # Significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot61.png')
plot(y=medv,x=black) 
abline(bmod,col="blue",lty=2)
legend("topleft",title="Plot Legend",
       legend=c("OLS line"),
       lty=c(2),
       col=c("blue"),
       cex=0.5) ; dev.off()
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot62.png')
plot(bmod$resid);dev.off()

lstatmod <- lm(medv~lstat,data=Boston)
summary(lstatmod) # Significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot63.png')
plot(y=medv,x=lstat) 
abline(lstatmod,col="blue",lty=2)
legend("topleft",title="Plot Legend",
       legend=c("OLS line"),
       lty=c(2),
       col=c("blue"),
       cex=0.5) ; dev.off()
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot64.png')
plot(lstatmod$resid);dev.off()

crimmod <- lm(medv~crim,data=Boston)
summary(crimmod) # Significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot65.png')
plot(x=crim,y=medv) 
abline(crimmod,col="blue",lty=2)
legend("topleft",title="Plot Legend",
       legend=c("OLS line"),
       lty=c(2),
       col=c("blue"),
       cex=0.5) ; dev.off()
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot66.png')
plot(crimmod$resid); dev.off()

# Multiple Regression model
multimod <- lm(medv~.,data=Boston)
summary(multimod)
# The variables indus and age were not statistically significant in the multiple regression model. This means we could reject H0 that Bj = 0 for every predictor except age and indus. Of the significant variables, zn,chas,rm,rad and black are positively correlated with the median value while crim, nox, dis, tax, ptratio and lstat were negatively correlated with medv.
beta.uni <- c(znmod$coefficients[-1],indusmod$coefficients[-1],chasmod$coefficients[-1],
              noxmod$coefficients[-1],rmmod$coefficients[-1],agemod$coefficients[-1],
              dismod$coefficients[-1],radmod$coefficients[-1],taxmod$coefficients[-1],
              ptratiomod$coefficients[-1],bmod$coefficients[-1],lstatmod$coefficients[-1],
              crimmod$coefficients[-1])
beta.multi <- multimod$coefficients[-1] 
predictors <- c("zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b",
                "lstat","medv")
par(mfrow=c(1,2))
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot67.png')
plot(x=beta.uni,y=beta.multi); dev.off()
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot67i.png')
plot(x=beta.uni[-c(4,5)],y=beta.multi[-c(4,5)],xlab="Univariate regression Betas without nox and rm",ylab="Multiple regresion Betas without nox and rm")
dev.off()
# Most coefficients looked more or less the same
# NonLinear fits
par(mfrow=c(1,1))
Boston3 <- data.frame(Boston,Boston^2,Boston^3)
znmod <- lm(medv~zn+zn.1+zn.2,data=Boston3)
summary(znmod)  # Significant in linear, quadratic and cubic terms
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot68.png')
plot(znmod$resid)   
dev.off()
indusmod <- lm(medv~indus+indus.1+indus.2,data=Boston3)
summary(indusmod) # Significant linear, quadratic and cubic terms
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot69.png')
plot(indusmod$resid);dev.off()

chasmod <- lm(medv~chas+chas.1+chas.2,data=Boston3)
summary(chasmod) # Significant linear term
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot70.png')
plot(chasmod$resid) ;dev.off()

noxmod <- lm(medv~nox+nox.1+nox.2,data=Boston3)
summary(noxmod) # Significant cubic term
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot71.png')
plot(noxmod$resid); dev.off()

rmmod <- lm(medv~rm+rm.1+rm.2,data=Boston3)
summary(rmmod) # Significant in linear, quadratic and cubic terms
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot72.png')
plot(rmmod$resid);dev.off()

agemod <- lm(medv~age+age.1+age.2,data=Boston3)
summary(agemod) # Insifnificant in all three terms
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot73.png')
plot(agemod$resid);dev.off()

dismod <- lm(medv~dis+dis.1+dis.2,data=Boston3)
summary(dismod) # Significant linear, quadratic and cubic terms
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot74.png')
plot(dismod$resid); dev.off()

radmod <- lm(medv~rad+rad.1+rad.2,data=Boston3)
summary(radmod) # Significant in linear, quadratic and cubic terms
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot75.png')
plot(radmod$resid); dev.off()

taxmod <- lm(medv~tax+tax.1+tax.2,data=Boston3)
summary(taxmod) # None of the terms are significant
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot76.png')
plot(taxmod$resid) ; dev.off()

ptratiomod <- lm(medv~ptratio+ptratio.1+ptratio.2,data=Boston3)
summary(ptratiomod) # No non-linear relationsip of significance
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot77.png')
plot(ptratiomod$resid) ; dev.off()

bmod <- lm(medv~black+black.1+black.2,data=Boston3)
summary(bmod) # No non-linear relationship of significance
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot78.png')
plot(bmod$resid);dev.off()

lstatmod <- lm(medv~lstat+lstat.1+lstat.2,data=Boston3)
summary(lstatmod) # Significant in linear, quadratic and cubic terms
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot79.png')
plot(lstatmod$resid);dev.off()

crimmod <- lm(medv~crim+crim.1+crim.2,data=Boston3)
summary(crimmod) # Significant in linear, quadratic and cubic terms
png('~/Dropbox/Academic notes/5 A/STAT 444/Assignments/A2/plot80.png')
plot(crimmod$resid); dev.off()


