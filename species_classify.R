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
save(sp, file = "sp.Rdata")
