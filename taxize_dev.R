#https://github.com/ropensci/taxize/issues/583

devtools::install_github("ropensci/taxize")
library(taxize)

# phylum = downstream(2, db = "ncbi", downto = "phylum")
# print(phylum)

family = downstream(2, db = "ncbi", downto = "family")

family_df = family$`2`
dim = dim(family_df)
species_out = NULL
for (a in 1:dim){#for each family 
  tmp = family_df[a,]
  genus_tmp = downstream(tmp$childtaxa_id, db = "ncbi", downto = "genus")
  genus_tmp = genus_tmp[[1]]
  dimg = dim(genus_tmp)[1]
  for (b in 1:dimg){#for each genus
    genus_tmp_1 = genus_tmp[b,]
    species_tmp = downstream(genus_tmp_1$childtaxa_id, db = "ncbi", downto = "species")
    species_tmp= species_tmp[[1]]
    dims = dim(species_tmp)
    # if (dims>0){
      rbind(species_out, species_tmp)
    # }
  }
}
#Error: Too Many Requests (HTTP 429) with genus

#hard to tell what to do with intermediate data.frames. so instead go down to species, then use classification to get full info on each species
# bacteria_species_list = downstream(2, db = "ncbi", downto = "family",
#                                    intermediate=TRUE)
# species = downstream(2, db = "ncbi", downto = "species")

# bacteria_species_list_df = bacteria_species_list$`2`

# save(bacteria_species_list_df, file = "bacteria_species_list_df.Rdata")