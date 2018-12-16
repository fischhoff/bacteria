pkgTest <- function(x)
{
  if (x %in% rownames(installed.packages()) == FALSE) {
    install.packages(x, dependencies= TRUE)    
  }
  library(x, character.only = TRUE)
}
neededPackages <- c("data.table", "dplyr", "reshape2", "corrplot", "RColorBrewer", "taxize", "myTAI", "usethis", "taxizedb", "stringr")

for (package in neededPackages){pkgTest(package)}
