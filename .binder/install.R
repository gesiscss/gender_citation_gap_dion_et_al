packages <- c(
  "flextable",
  "kableExtra",
  "foreign",
  "knitr",
  "IRdisplay",
  "MASS",
  "optimx",
  "rmarkdown",
  "rms",
  "tidyverse"
)

for(package in packages){
  
  install.packages(package,repos = "https://cloud.r-project.org/")
  
}