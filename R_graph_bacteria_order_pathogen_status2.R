#load pathogen data
load("../DATA/PROCESSED/bacteria_pathogenic_mammals.Rdata")
df1 = bacteria_pathogenic_mammals
#remove rows for hosts that do not host pathogens
df1 = subset(df1, pathogen != "")
load("../DATA/PROCESSED/bacteria_species_out.Rdata")
df2 = bacteria_species_out

df2 = subset(df2, rank == "species")
# df2$row = seq(1,dim(df2)[1])

df2 <- df2 %>%
  select (-c(rank))
#rename column
df2 = rename(df2, bacteria_species = name)
df2 = rename(df2, tax_id = id)
df2 = rename(df2, order_pathogen = order)
df2$bacteria_original = ""

df1 = rename(df1, bacteria_species = pathogen)
df1 = rename(df1, bacteria_original = pathogen_original)

inds_df1_missing_in_df2 = which(!(df1$bacteria_species %in% df2$bacteria_species))

# print("present in pathogen list, absent in full bacteria list; these should all be genus-level")
# print(df1$bacteria_species[inds_df1_missing_in_df2])

inds_df1_original_missing_in_df2 = which(!(df1$bacteria_original %in% df2$bacteria_species))

# print("present in original pathogen list, absent in full bacteria list")
df1$bacteria_original=as.character(df1$bacteria_original)
# print(df1$pathogen_original[inds_df1_original_missing_in_df2])

df1 <- df1 %>% 
  select(c(bacteria_species,
           order_pathogen,
           tax_id,
           bacteria_original
  ))


df1=df1[inds_df1_original_missing_in_df2,]
df1$pathogenic = 1
print(unique(df1$bacteria_original))
#find inds in  master list matching pathogenic species
inds = which(df2$pathogen_species %in% c(df1$pathogen_species))

df2$pathogenic = 0
df2$pathogenic[inds] = 1

df2 <- rbind(df1, df2)
bacteria_species_out2 = df2
save(bacteria_species_out2, file = "../DATA/PROCESSED/bacteria_species_out2.Rdata")
