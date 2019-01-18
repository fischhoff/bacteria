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
# a = 1
#Start the clock
ptm<-proc.time()

# for (a in 1:10){#for each bacterial species

# for (a in 1:dim(x)[1]){#for each bacterial species
#   class = classification(x$childtaxa_id[a], db = "ncbi")
#   class = class[[1]]
#   order = subset(class, rank == "order")
#   if (dim(order)[1]>0){
#     x$order[a]=order$name
#   } else {
#     x$order[a]=""
#   }
# }

#Stop the clock
print((proc.time()-ptm)/60)

bacteria_species = x
write.csv(bacteria_species, file = "bacteria_species.csv")