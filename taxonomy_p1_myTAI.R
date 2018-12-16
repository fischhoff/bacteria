load("p1.Rdata")
df = p1
# API = "0a46db034d980435244491d666cf375d2f08"
species = rep("unassigned", dim(df)[1])
for (a in 1:dim(df)[1]){
  tmp = taxonomy(df$Organism_name[a])
  species.tmp=tmp[tmp$rank == "species","name"]
  if (is.null(species.tmp)==FALSE){
    species[a]= species.tmp
  }
  if (is.null(species.tmp)==TRUE){
    species[a]= "unassigned"
  }
}
