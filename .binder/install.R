packages <- c(
  "flextable",
  "dplyr",
  "kableExtra",
  "foreign",
  "knitr",
  "IRdisplay",
  "IRkernel",
  "MASS",
  "optimx",
  "rmarkdown",
  "rms",
  "tidyverse"
)

for(package in packages){
  
  install.packages(package,repos = "https://cloud.r-project.org/", dependencies=TRUE)
  
}