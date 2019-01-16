path= "taxdmp/"
nodes = getnodes(path)
#nodes = getnodes(paste0(path,"names.dmp"))
nodes[1,]

names = getnames(path)
#merge nodes and names
all = merge(nodes, names)

#find the id of Bacteria
out =NULL
species_id = which(all$type == "scientific name" & all$rank == "species")
sp = all[species_id,]
dim = dim(sp)[1]
print(dim)

#Start the clock
ptm<-proc.time()

# for (a in 1:100){
for (a in 1:dim){
  tmp =classification(sp$id[a], db = "ncbi")
  tmp = tmp[[1]]
  if (!is.na(tmp)){#if isn't NA
    tmp_species = subset(tmp, rank == "species")
    tmp$species = tmp_species$name 
    out = rbind(out,tmp)
  }
}

#Stop the clock
print((proc.time()-ptm)/60)

species_classified =out
save(species_classified, file = "species_classified.Rdata")