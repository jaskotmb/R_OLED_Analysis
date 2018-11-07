########################################################################################################
##### Giebink Harmonic Analysis Tools
########################################################################################################
fileName = "EQEout.csv"
sampleName = "181030Dv48-V4 10% Ir(ppy)3:mCBP 1A/sec 25C"
eqeData <- read.csv(fileName)
# Naming as per Price, Giebink 2015 APL QE Harmonic Analysis paper
L0 <- eqeData$L..photons.out.cm.2.
J0 <- eqeData$J..excitons.in.cm.2.
EQE0 <- eqeData$EQE
EQE1 <- calcDeriv(J0,L0)
giebinkY <- EQE0 - EQE1
giebinkX <- (2*EQE1 - EQE0)*L0
colfunc <- colorRampPalette(c("blue","red"))
xMin <- 0
xMax <- 1.6e14
yMin <- -.03
yMax <- 0.02
plot(giebinkX,giebinkY,xlim=c(xMin,xMax),ylim=c(yMin,yMax),col=colfunc(length(giebinkX)),main=sampleName)
fitMin <- 100
fitMax <- 150
points(giebinkX[fitMin:fitMax],giebinkY[fitMin:fitMax],col="black",pch=19)
lineFit <- lm(giebinkY[fitMin:fitMax]~giebinkX[fitMin:fitMax])
abline(lineFit$coefficients)
fitSlope <-  lineFit$coefficients[2]
# Assumptions for constants:
tau <- 1.6e-6 #Ir(ppy)3 lifetime
nOC <- 0.2 # outcoupling efficiency
nPL <- 1 # PL efficiency
a0 <- 30e-7 #thk of EML (cm units)
kXX <- fitSlope*a0*nPL*nOC/(tau*tau) # Units cm^3/sec
kTTA = 2*kXX
# Text on Chart
text(0.2*(xMin+xMax),yMin,paste("tau = ",tau*1e6,"us"))
text(0.2*(xMin+xMax),0.05*(yMax-yMin)+yMin,paste("nOC = ",nOC))
text(0.2*(xMin+xMax),0.1*(yMax-yMin)+yMin,paste("nPL = ",nPL))
text(0.2*(xMin+xMax),0.15*(yMax-yMin)+yMin,paste("a0 = ",a0*1e7,"nm"))
text(0.3*(xMin+xMax),0.25*(yMax-yMin)+yMin,paste("kTTA = ",signif(kTTA,3),"cm^3/sec"),cex=1.5)