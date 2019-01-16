#not installing taxize in order to use dev version
pkgTest <- function(x)
{
  if (x %in% rownames(installed.packages()) == FALSE) {
    install.packages(x, dependencies= TRUE,repos = "http://cran.us.r-project.org")    
  }
  library(x, character.only = TRUE)
}
#installing curl before taxize
neededPackages <- c("data.table", "dplyr", "reshape2", "corrplot", "RColorBrewer","curl", "taxize", "myTAI", "usethis", "taxizedb", "stringr",
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