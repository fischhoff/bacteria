load("sp.Rdata")
dim = dim(sp)[1]
print(dim)

#Start the clock
ptm<-proc.time()

a = 100
for (a in 1:2){
# for (a in 1:dim){
  tmp =classification(sp$id[a], db = "ncbi")
  tmp = tmp[[1]]
  nrows = dim(tmp)[1]
  if (nrows>0){#if at least one row
    tmp_species = subset(tmp, rank == "species")
    tmp$species = tmp_species$name 
    out = rbind(out,tmp)
  }
}

#Stop the clock
print((proc.time()-ptm)/60)

species_classified =out
# save(species_classified, file = "species_classified.Rdata")