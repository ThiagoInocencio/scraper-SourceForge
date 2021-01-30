installPackages <- function(){
  install.packages("magrittr")
  install.packages("rvest")
  install.packages("purrr")
  install.packages("Hmisc")
  install.packages("xlsx")
  install.packages("magrittr") # package installations are only needed the first time you use it
  install.packages("dplyr")    # alternative installation of the %>%
}

loadPackages <- function() {
  library(httr)
  library(rvest)
  library(dplyr)
  library(purrr)
  library(plyr)
  library(magrittr)
  library(stringr)
  library(Hmisc)
  library(magrittr) # needs to be run every time you start R and want to use %>%
  library(dplyr)    # alternatively, this also loads %>%
}
