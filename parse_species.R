load("E.Rdata")
options(warn = -1)
E$speciesDescription=as.character(E$speciesDescription)
out = NULL
# z = 1
# a = 1
# for (z in 1:10){
for (z in 1:dim(E)[1]){
  if (E$speciesDescription[z] == ""){
    tmp = E[z,]
    tmp$animal.origin = ""
    tmp$animal.species = ""
    out = rbind(out, tmp)#add this row without any changes
  }
  if (E$speciesDescription[z] != ""){#if there is an entry
    #split by comma
    species.split <- strsplit(E$speciesDescription[z], ",")
    #get number of species
    nspecies = length(species.split[[1]])/2
    #get indices of wild vs. domestic
    inds_origin = seq(1,length(species.split[[1]]), by = 2)
    #get indices of species descriptor
    inds_species = seq(2,length(species.split[[1]]), by = 2)
    for (a in 1:nspecies){#for each species
      origin =species.split[[1]][inds_origin[a]]#get wild vs. domestic 
      #print(origin)
      species = species.split[[1]][inds_species[a]]
      #print(species)
      tmp = E[z,]
      tmp$animal.origin = origin
      tmp$animal.species = species
      out = rbind(out, tmp)
    }#end a'th species
  }#end if statement re: speciesDescription not empty
}#end row of E

out$animal.species = trimws(out$animal.species)
out$animal.origin = trimws(out$animal.origin)
save(out, file = "out.Rdata")
options(warn = 1)