setwd("C:/Users/lenovo/Desktop")

if(!dir.exists("DX File Loader")){
  dir.create("DX File Loader")
  stop("Please Load DX Files into DX File Loader.")
}

Char = getwd()
Char = paste(Char,"/DX File Loader",sep = "")
setwd(Char)
Filelist = list.files()

plotOD = function(filelist){
  for(i in 1:length(filelist)){
    tem = read.table(file = filelist[i])
    Wavelength = tem$V1[11:711]
    Absorbtion = tem$V2[11:711]
    Spectra = cbind(Wavelength,Absorbtion)
    
    if(i == length(filelist)){
      plot(Spectra,type = "l",
           xlab = "Wavelength",
           ylab = "Absorbtion",
           col = i,
           xlim = c(200,900),
           ylim = c(0,4),
           main = "The Overall Spectra")
      par(new = FALSE)
    }else{
      plot(Spectra,type = "l",
           xlab = "",
           ylab = "",
           col = i,
           xlim = c(200,900),
           ylim = c(0,4))
      par(new = TRUE)
    }
  }
}
windows()
plotOD(filelist = Filelist)


getODvalue = function(wavelength){
  ODvalue = vector(length = length(Filelist))
  for(i in 1:length(Filelist)){
    tem = read.table(file = Filelist[i])
    Wavelength = tem$V1[11:711]
    Absorbtion = tem$V2[11:711]
    Spectra = cbind(Wavelength,Absorbtion)
    ODvalue[i] = Spectra[wavelength - 200,2]
  }
  plot(1:length(Filelist),ODvalue,
       type = "l",
       xlab = "Fraction Number",
       ylab = "Absorbance",
       main = paste("OD",wavelength," VS Fraction Number",sep = ""))
  return(ODvalue)
}

windows()
OD_628 = getODvalue(628)
windows()
OD_292 = getODvalue(292)
windows()
OD_280 = getODvalue(280)

Yield = function(OD,wavelength){
  OD = OD[OD > 0]
  OD = OD[OD < 2]
  if(wavelength == 628){
    sigma = 5900
  }else if(wavelength == 292){
    sigma = 5817
  }else if(wavelength == 280){
    sigma = 6690
  }else{
    stop("No Sigma Value Stored.")
  }
  return(sum((OD/sigma) * 28000))
}

Yield(OD_628,628)
Yield(OD_280,280)
Yield(OD_292,292)








