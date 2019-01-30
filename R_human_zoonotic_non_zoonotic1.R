load("G_classified.Rdata")#load 
# G_classified = G
G_classified$host_pathogen = paste(G_classified$host_species, 
                                   G_classified$pathogen)

# wd = getwd()
# setwd(wd)
file.exists("../DATA/PROCESSED/df_all.Rdata")#two dots and slash to go up two levels
load("../DATA/PROCESSED/df_all.Rdata")#load df_all

df = df_all
#get only the species-level data
df = subset(df, pathogen_level == "species")

df = subset(df, Label == 1)
#get only the records for which there is a host, because our analysis is from host perspective
df <- df %>%
  select(-c(pathogen_level, Label, row, 
            Note,
            ParasiteCorrectedNameGMPD, 
            multiple.countries,
            Citation
  ))

names(df)[names(df)=="Spp"]="host_species"
#df <- rename(df, host_species= Spp )
df <- rename(df, host_order = Order)
df <- rename(df, vector_type = vector)
df$vector_type[df$vector_type == ""]="none"
df$vector = NA#assign vector as 1/0
df$vector[df$vector_type == "none"]=0
df$vector[df$vector_type != "none"]=1
###load G_classified
# load("../DATA/PROCESSED/G_classified.Rdata")#load 
G_classified = rename(G_classified, host_species = HostCorrectedName )
G_classified = rename(G_classified, host_order = HostOrder )


#find records in G_classified that have not already been added to df_all
#concatenate host and pathogen

df$host_pathogen = paste(df$host_species, 
                         df$pathogen)

#inds of G_classified that are not in df
#this is not enough to find non-zoonotic, because a zoonotic bacteria could be in a species that hasn't been identified in GIDEON as a host  
# inds_not = which(!(G_classified$host_pathogen %in% df$host_pathogen))
# 
# G_non_zoonotic = G_classified[inds_not,]
# G_non_zoonotic$disease = "Non-zoonotic"

out = NULL
dim = dim(G_classified)[1]
G_classified$pathogen = as.character(G_classified$pathogen)
df$pathogen=as.character(df$pathogen)
G_classified$host_species= as.character(G_classified$host_species)
df$host_species=as.character(df$host_species)
a = 1
ct = 0
for (a in 1:dim){
  tmp_G =G_classified[a,]
  #for each record in G_classified, find out whether the pathogen is in df
  tmp_df = subset(df, pathogen == tmp_G$pathogen)  
  tmp_df_host_path = subset(df, pathogen == tmp_G$pathogen & host_species == tmp_G$host_species)  
  dimt = dim(tmp_df)[1]
  dim_hp = dim(tmp_df_host_path)[1]
  if (dimt == 0){  #if the pathogen is not in df, 
    ct = ct+1 #print(tmp_G$pathogen)
    tmp_G$vector_type = NA
    tmp_G$disease = "Not zoonotic"#  then assign to that record in G_classified "Not zoonotic" as disease 
    tmp_G$recorded_host_GIDEON = NA
    tmp_G$zoonotic_category = "wild only"
  }
  else if (dimt > 0 & dim_hp == 0){  #if the pathogen IS in df, but not with this host  
    #add vector_species
    tmp_G$vector_type = tmp_df$vector_type[1]#
    tmp_G$disease = tmp_df$disease[1]#  then assign to that record the disease 
    tmp_G$recorded_host_GIDEON = 0
    tmp_G$zoonotic_category = "zoonotic"
  }
  else if (dimt > 0 & dim_hp == 1){  #if the pathogen IS in df, AND with this host  
    #add vector_species
    tmp_G$vector_type = tmp_df$vector_type[1]#
    tmp_G$disease = tmp_df$disease[1]#  #then assign disease to that record in G_classified --> output_G
    tmp_G$recorded_host_GIDEON = 1
    tmp_G$zoonotic_category = "zoonotic"
  }
  out =  rbind(out, tmp_G)
}
dim(out)[1]==dim(G_classified)[1]

print(table(out$zoonotic_category))

#rename vector in df to vector_type
#after making output_G, find host_pathogen in df that are not in output_G (this includes human-hosted dx as well as wildlife  hosts that were not in GMPD). 
#assign NA to GMPD traits in these rows, then rbind with output_G
inds_not = which(!(df$host_pathogen %in% out$host_pathogen))
df_add = df[inds_not,]
print(dim(df_add))

setdiff(names(out), names(df_add))
# setdiff(names(df_add), names(out))
df_add$recorded_host_GIDEON = 1#includes humans
df_add$close = NA
df_add$nonclose = NA
df_add$intermediate = NA
out = unique(out)#not sure why there are multiple records but this removes them
out <-out %>%
  select(-c(ParasiteTraitsCitation, ParType,
            species_pathogen_classified,
            pathogen_species_match_classified
  ))
#assign zoonotic category
df_add$pathogen_original = NA
df_add$zoonotic_category = NA
df_add$zoonotic_category[df_add$host_species == "Homo sapiens"]="human"
df_add$zoonotic_category[df_add$host_species != "Homo sapiens"]="zoonotic"

#could delete these next three lines
df_add1 <- df_add %>%
  group_by(disease) %>%
  mutate(host_types_count = length(unique(zoonotic_category)))

out_2 = NULL
ud = unique(df_add1$disease)
for (a in 1:length(ud)){
  tmp = subset(df_add1, disease == ud[a])
  zoonotic = subset(tmp, zoonotic_category =="zoonotic")
  dimz = dim(zoonotic)[1]
  human = subset(tmp, zoonotic_category == "human")
  dimh = dim(human)[1]
  if (dimh >0 & dimz >0){
    tmp$zoonotic_category = "zoonotic"
  } else if (dimh >0 & dimz == 0){
    tmp$zoonotic_category = "human only"
  }
  out_2 = rbind(out_2, tmp)
}
df_add1 <- out_2

df_add <- df_add1# 

# df_add$zoonotic_category[df_add$host_types_count == 1]="human only"
# df_add$zoonotic_category[df_add$host_types_count == 2]="zoonotic"
print(table(df_add$zoonotic_category))

#remove temporary field
df_add <- df_add %>%
  select(-c(host_types_count
  ))

# out$zoonotic_category= NA
# out$zoonotic_category[out$disease=="Not zoonotic"]="wild only"
# out$zoonotic_category[out$disease!="Not zoonotic"]="zoonotic"

df_add = as.data.frame(df_add)
out1 = rbind(out, df_add)
print(table(out1$zoonotic_category))

bacteria_pathogenic_mammals <- out1
save(bacteria_pathogenic_mammals, file = "../DATA/PROCESSED/bacteria_pathogenic_mammals.Rdata")
