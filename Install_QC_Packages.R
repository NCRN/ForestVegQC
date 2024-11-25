### list of needed CRAN packages
QC_Packages<-c("devtools", "DT", "knitr",  "lubridate", "rmarkdown", "tidyverse")

### install CRAN packages if not already installed
Package_Check<-function(package_name){
  if(nchar(system.file(package=package_name))==0)  install.packages(package_name, dependencies = TRUE)
}

lapply(QC_Packages, FUN=Package_Check)

## install NPSForVeg package from GitHub if its not installed
if(nchar(system.file(package="NPSForVeg"))==0)  devtools::install_github("NCRN/NPSForVeg")


