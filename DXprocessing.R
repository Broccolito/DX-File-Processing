#This script is used to process DX files
rm(list = ls())
current_wd = getwd()

dx_to_matrix = function(textfile){
  temfile = unlist(strsplit(textfile, " "))
  return(as.matrix(cbind(as.numeric(temfile[seq(1,length(temfile),2)]),
                         as.numeric(temfile[seq(2,length(temfile),2)]))))
}

#Choose the dir where all DX files are stored.
rdx <- read_dx <- function(){
  setwd(choose.dir())
  filelist <<- list.files()
  for(i in 1:length(filelist)){
    eval(parse(text = paste("spec",
                            i,
                            " <<- ", 
                            "readLines(filelist[",i,"]",
                            ",encoding='UTF-8')[25:900]",
                            sep = "")))
    eval(parse(text = paste("spec",
                            i,
                            " <<- ",
                            "dx_to_matrix(spec",
                            i,
                            ")",
                            sep = "")))
  }
  print("Processed filelist: ")
  print(filelist)
}

read_dx()
varlist = ls()

pdx <- plot_dx <- function(fraction = 1,add_on = FALSE,method = "single"){
  speclist = c(varlist[nchar(varlist) == 5],varlist[nchar(varlist) == 6])
  if(method == "all"){
    windows(width = 7680,height = 4320) #8K Resolution
    for(i in 1:length(speclist)){
      eval(parse(text = paste(
        paste("plot(",speclist[i],",type = 'l', col = ",i,
              ",xlim = c(200,1000), ylim = c(0,4),xlab = 'Wavelength',ylab = 'absorbtion')",
              sep = ""))))
      eval(parse(text = paste(
        "par(new = TRUE)"
      )))
    }
    par(new = add_on)
  }else if(method == "single"){
    specname = paste("spec", fraction,sep = "")
    windows(width = 7680,height = 4320) # 8K Resolution
    eval(parse(text = paste(
      "plot(",specname,",type = 'l', col = ",i,
      ",xlim = c(200,1000), ylim = c(0,4),xlab = 'Wavelength',ylab = 'absorbtion')",
      sep = ""
    )))
    par(new = add_on)
  }
}

plot_dx(method = "all")

setwd(current_wd)