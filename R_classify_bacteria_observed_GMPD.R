load("../DATA/PROCESSED/G.Rdata")
#Start the clock
ptm<-proc.time()
out =NULL#initialize output
# for (a in 1:10){

df = G
df$order_pathogen = ""#initialize order as blank
df$family_pathogen = ""#initialize order as blank
df$genus_pathogen = ""#initialize order as blank

df_no_parasite = subset(df,is.na(tax_id))
dim(df_no_parasite)
df = subset(df,!is.na(tax_id))
dim = dim(df)[1]

for (a in 1:dim){
  #print(a)
  # if (dim %%)
  #Sys.sleep(0.15)
  tmp = classification(df$tax_id[a], db = "ncbi")
  tmp = tmp[[1]]
  # nrows = dim(tmp)[1]
  # if (nrows>0){#if at least one row
    tmp_order = subset(tmp, rank == "order")
    dim_o = dim(tmp_order)[1]
    # df$order[a] = ""
    if(dim_o>0){
      df$order_pathogen[a] = tmp_order$name 
    # }
    }
    tmp_family = subset(tmp, rank == "family")
    dim_o = dim(tmp_family)[1]
    if(dim_o>0){
      df$family_pathogen[a] = tmp_family$name 
      # }
    }
    tmp_genus = subset(tmp, rank == "genus")
    dim_o = dim(tmp_genus)[1]
    if(dim_o>0){
      df$genus_pathogen[a] = tmp_genus$name 
    }
    
    tmp_species = subset(tmp, rank == "species")
    dim_o = dim(tmp_species)[1]
    if(dim_o>0){
      df$species_pathogen_classified[a] = tmp_species$name 
    } else {
      df$species_pathogen_classified[a] = ""
    }
    
}
df$pathogen_species_match_classified = (df$pathogen == df$species_pathogen_classified)

                                        #Stop the clock
inds_change = which(df$pathogen_species_match_classified==FALSE)
df$pathogen_original = df$pathogen
df$pathogen = as.character(df$pathogen)
df$species_pathogen_classified = as.character(df$species_pathogen_classified)
df$pathogen[inds_change] = df$species_pathogen_classified[inds_change] 

print((proc.time()-ptm)/60)
# df_all = rbind(df, df_no_parasite)
G_classified = df
save(G_classified, file = "../DATA/PROCESSED/G_classified.Rdata")
save(G_classified, file = "G_classified.Rdata")

# save(df_all, file = "df_all.Rdata")