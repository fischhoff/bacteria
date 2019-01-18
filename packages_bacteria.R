#remove packages if they are present in lib
pkgrmTest <- function(x)
{
  if (x %in% rownames(installed.packages()) == TRUE) {
    remove.packages(x)    
  }
}

packagesRemove <- c("tibble", "tidyselect")
for (package in packagesRemove){pkgrmTest(package)}

#not installing taxize in order to use dev version
pkgTest <- function(x)
{
  if (x %in% rownames(installed.packages()) == FALSE) {
    install.packages(x, dependencies= TRUE,repos = "http://cran.us.r-project.org")    
  }
  library(x, character.only = TRUE)
}
#installing curl before taxize
#removed "stringi"
#"glue" and "rlang" seem needed for dplyr, after deleting each from library manually
neededPackages <- c("data.table", "glue", "rlang","tibble", "tidyselect", "dplyr", "reshape2", "corrplot", 
                    "RColorBrewer","curl", "taxize", "myTAI", "usethis", "stringr",
                    "naniar", "Hmisc",
                    "ggplot2",
                    "CHNOSZ"
                    )
#"rstan"
for (package in neededPackages){pkgTest(package)}
#biocLite not available for this version of R
# install.packages("biocLite")
# library("biocLite")
# biocLite("biomaRt")
#requires biomaRt, which we can't install
# install.packages("biomartr")
# library("biomartr")

devtools::install_github('TIBHannover/BacDiveR')

devtools::install_github("ropensci/taxizedb")
library(taxizedb)