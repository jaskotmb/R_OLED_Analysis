rm(list=ls())
source("C:/Users/Morty/Documents/R_OLED_Analysis/OLED_R_functions.R")
fnMatrix = initializeFiles()
cols = getColors(fnMatrix)
howManyTypes = dim(fnMatrix)[1]

##### creates y/n matrix for which EQE files are good to plot
okMatrixEQE = matrix("",nrow=dim(fnMatrix)[1],ncol=dim(fnMatrix)[2])
for(k in seq(1,howManyTypes)){
  for(i in seq(1,length(fnMatrix[k,]))){
    if(!(is.na(fnMatrix[k,i]))){
      plot(NA,xlim=c(-9,-3),ylim=c(-4,1))
      EQEplot(fnMatrix[k,i],cols[k])
      okMatrixEQE[k,i] = readline(paste(fnMatrix[k,i],"OK? (y/n): "))
    }
  }
}

plot(NA,xlim=c(-7,-3),ylim=c(-1.7,-.9),xlab="log Brightness",ylab="log EQE",main="180830 OLEDs")
for(k in seq(1,howManyTypes)){
  for(i in seq(1,length(fnMatrix[k,]))){
    if((!(is.na(fnMatrix[k,i])))&&(okMatrixEQE[k,i]=='y')){
      EQEplot(fnMatrix[k,i],cols[k])
    }
  }
}
legend(-6,-1.7,c("180718 CBP:Ir(ppy)3 1A/s","180718 CBP:Ir(ppy)3 25C(Dv3)",
                 "180730 CBP:Ir(ppy)3 1A/s","180730 CBP:Ir(ppy)3 -50C(Dv7)"),
       col=c("black","blue","brown","purple"),pch=15)

plot(NA,xlim=c(-9,-3),ylim=c(0.0,.1),xlab="log Brightness",ylab="EQE",main="180830 OLEDs")
for(k in seq(1,howManyTypes)){
  for(i in seq(1,length(fnMatrix[k,]))){
    if((!(is.na(fnMatrix[k,i])))&&(okMatrixEQE[k,i]=='y')){
      EQEplotLinear(fnMatrix[k,i],cols[k])
    }
  }
}
legend(-9,.06,c("CBP:Ir(ppy)3 25C(Dv3)","CBP:Ir(ppy)3 -50C(Dv7)"),col=c("black","blue"),pch=15)

