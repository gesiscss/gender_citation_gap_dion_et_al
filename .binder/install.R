packages <- c("tidyverse",
             "library",
             "foreign",
             "Hmisc",
             "IRdisplay",
             "IRkernel",
             "optimx",
             "rms",
             "kableExtra")

for(package in packages){
  if(!(package %in% installed.packages())){  
    install.packages(package)
  }
}
