########################################################################################################
##### Gets filenames, etc
########################################################################################################
initializeFiles <- function(){
  dir = "C:/Users/Morty/Documents/R_OLED_Analysis/test3"
  dirNew = readline(paste("Current Dir is:",dir,"\n","    Press q to change, enter to continue"))
  if(dirNew =='q'){
    newDir = readline("Directory Name: ")
    dir = newDir
  }
  setwd(dir)
  print("Selected Directory: ",end='')
  print(dir)
  readline()
  files = list.files(dir)
  print(files)
  print("Files:",end='')
  print(length(files))
  A = 0.04
    
  howManyTypes = as.numeric(readline("How many types of device?: "))
  print(howManyTypes)
  
  fnMatrix = matrix(nrow=howManyTypes,ncol=length(files))
  typeNames = rep("",howManyTypes)
  for(i in seq(1,howManyTypes)){
    typeNames[i] = readline(paste(i,"th name: ",sep=""))
  }
  
  for(i in seq(1,howManyTypes)){
    temp = files[grep(typeNames[i],files)]
    fnMatrix[i,seq(1,length(temp))] = temp
  }
  
  names(fnMatrix) <- typeNames
  return(fnMatrix)
}

getColors <- function(fnMatrixin){
  howManyTypes = dim(fnMatrixin)[1]
  cols = rep("",howManyTypes)
  for(i in seq(1,howManyTypes)){
    cols[i] = readline(paste("Color of ",names(fnMatrixin)[i],": ",sep=''))
  }
  return(cols)
}

########################################################################################################
##### plots EQE curve of given filename
########################################################################################################
EQEplot = function(IVfilename,color){
  offset = 2e-7 #2e-7
  A = .04
  x = read.csv(IVfilename)
  x$Brightness.Current..A. = x$Brightness.Current..A. - offset
  x = getPosOnly(x)
  electronsIn = -x$Current..A./(1.602176634*(10e-19))
  OLEDWattsOut = x$Brightness.Current..A./(0.39)
  #OLEDWattsOut = x$Brightness.Current..A./(integrated responsivity of diode w/ EL spectrum (A/W))
  #OLEDWattsOut = (photons/(sec)) * (q)*1.24/(avg wavelength in um)
  OLEDPhotonsOut = OLEDWattsOut/((1.602176634*(10e-19))*1.24/(0.52))
  EQE = OLEDPhotonsOut/electronsIn
  lines(log10(abs(x$Brightness.Current..A.)),log10(abs(EQE)),col=color)
  maxEQEspot = which(EQE==max(EQE))
  print(maxEQEspot)
  points(log10(abs(x$Brightness.Current..A.[maxEQEspot])),log10(abs(EQE[maxEQEspot])),
         lwd=5,col=color,pch=19)
  outputData = cbind(x$Voltage..V.,x$Current..A.,x$Brightness.Current..A.,EQE,OLEDPhotonsOut/A,electronsIn/A)
  colnames(outputData) <- c("Voltage (V)","Current (A)","Brightness Current (A)","EQE","L (photons out/cm^2)","J (excitons in/cm^2)")
  write.csv(outputData,file="EQEout.csv")
}


########################################################################################################
##### plots linear EQE curve of given filename
########################################################################################################
EQEplotLinear = function(IVfilename,color){
  offset = 2e-7
  x = read.csv(IVfilename)
  x$Brightness.Current..A. = x$Brightness.Current..A. - offset
  x = getPosOnly(x)
  electronsIn = -x$Current..A./(1.602176634*(10e-19))
  OLEDWattsOut = x$Brightness.Current..A./(0.39)
  #OLEDWattsOut = x$Brightness.Current..A./(integrated responsivity of diode w/ EL spectrum (A/W))
  #OLEDWattsOut = (photons/(sec)) * (q)*1.24/(avg wavelength in um)
  OLEDPhotonsOut = OLEDWattsOut/((1.602176634*(10e-19))*1.24/(0.52))
  EQE = OLEDPhotonsOut/electronsIn
  lines(log10(abs(x$Brightness.Current..A.)),EQE,col=color)
}

########################################################################################################
##### returns the forward bias forward sweep portion of the IV curve only
########################################################################################################
getPosOnly = function(IVdata){
  posOnly = IVdata
  posOnly[,1] = -posOnly[,1] # Positive Bias set when OLED output is on
  posOnly = posOnly[which(posOnly[,3]>0)[1]:which(posOnly[,1]==max(posOnly[,1]))[1],]
  return(posOnly)
}  
########################################################################################################
##### Calculate centered numerical derivative
########################################################################################################
calcDeriv = function(x,y){
  d <- rep('',length(x))
  d[1] <- (y[2] - y[1])/(x[2]-x[1])
  for(i in seq(2,length(d)-1)){
    d[i] <- 0.5*((y[i+1]-y[i])/(x[i+1]-x[i])) + 0.5*((y[i]-y[i-1])/(x[i]-x[i-1]))
  }
  d[length(d)] <- (y[length(d)]-y[length(d)-1])/(x[length(d)]-x[length(d)-1])
  return(as.numeric(d))
}





