df = read.csv("GIDEON_bacterium_dx - 20181219.csv")
df = df[,c("bacterial_disease", "pathogen", "vector")]
df$pathogen=as.character(df$pathogen)
out = NULL
# z = 1
# a = 1
# for (z in 1:10){
for (z in 1:dim(df)[1]){
  if (df$pathogen[z] == ""){
    tmp = df[z,]
    out = rbind(out, tmp)#add this row without any changes
  }
  if (df$pathogen[z] != ""){#if there is an entry
    #split by comma
    species.split <- strsplit(df$pathogen[z], ",")
    #get number of species
    nspecies = length(species.split[[1]])
    for (a in 1:nspecies){#for each species
      species = species.split[[1]][a]
      tmp = df[z,]
      tmp$pathogen = species
      out = rbind(out, tmp)
    }#end a'th species
  }#end if statement re: speciesDescription not empty
}#end row of df

out$pathogen = trimws(out$pathogen)
human_bacteria = out

save(human_bacteria, file = "human_bacteria.Rdata")
