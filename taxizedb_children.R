devtools::install_github("ropensci/taxizedb")
library(taxizedb)

# x <- downstream("Bryophyta", db = "ncbi", downto="family")
# tibble::as_tibble(x$Bryophyta)

#Commenting out because yields error: 
# b <- downstream("Bacteria", db = "ncbi", downto="species", out_type = "summary")
#  Error in name2taxid(x[is_named], db = "ncbi") : 
#  Some of the input names are ambiguous, try setting out_type to 'summary'

x <- downstream(2, db = "ncbi", downto = "species")
x <- x[[1]]
bacteria_species = x
write.csv(bacteria_species, file = "bacteria_species.csv")