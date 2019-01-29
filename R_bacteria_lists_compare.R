df = read.csv("bacteria_species.csv")

load("df_all.Rdata")
df_all$pathogen=trimws(df_all$pathogen)
df_all$pathogen=as.character(df_all$pathogen)

df_parasite = subset(df_all, pathogen !="")
df_no_parasite = subset(df_all, pathogen == "")
df_no_parasite$pathogen_level = ""
missing = setdiff(df_parasite$pathogen, df$childtaxa_name)

missing_spp = missing[5]
species.split <- strsplit(missing_spp, "spp")
species.split = species.split[[1]]
#length(species.split[1])
test = stri_detect_fixed(missing_spp,c("spp"))

# df_parasite = subset(df_parasite, pathogen == "Borrelia spp")
z = 1
out = NULL
spp_list = c("spp", "sp.")
out_master = NULL
out_synonym = NULL
for (z in 1:dim(df_parasite)[1]){
    tmp = df_parasite[z,] 
    tmp$pathogen_level = "species"
    
  #split by comma
    test = stri_detect_fixed(tmp$pathogen,spp_list)
    ind_true = which(test == TRUE)
    if (length(ind_true)==1){
      split_char = spp_list[ind_true]#find the term we need to split by
      #print(split_char)
      #split by split_char
      species.split <- strsplit(tmp$pathogen, split_char,
                                fixed=TRUE)
      species.split = trimws(species.split[[1]])
      #print(species.split)
      tmp$pathogen = species.split
      id = name2taxid(species.split, out_type = "summary")
      taxid = id$tax_id[1]
      class = classification(taxid, db = "ncbi")
        class = class[[1]]
        genus = subset(class, rank == "genus")
        if (dim(genus)[1]>0 & genus$name == tmp$pathogen){
          tmp$pathogen_level = "genus"
        } else {
          tmp$pathogen_level = "unknown"
        }
    }#end if there is spp or sp.
    else {
      #check if in list of all bacteria
      ind = which(df$childtaxa_name == tmp$pathogen)
      #if missing from list of all bacteria
      if (length(ind)==0){
        id = name2taxid(tmp$pathogen, out_type = "summary")
        taxid = id$tax_id[1]
        # print(tmp$pathogen)
        # print(taxid)
        if (is.na(taxid)){#if we still don't know taxid
          tmp$pathogen_level="not found in ncbi"
        } else {#otherwise is found in ncbi, get classification
          class = classification(taxid, db = "ncbi")
          class = class[[1]]
          species = subset(class, rank == "species")
          synonym = subset(class, rank = "synonym")
          dim_syn =  dim(synonym)[1]
          if (dim_syn>0){
           # print(dim_syn)
            out_synonym=rbind(out_synonym,class)#save the synonyms
            
          }
          dims =dim(species)[1] 
          if (dims>0){
            tmp$pathogen = species$name
            #print(species$name)
            #this may result in duplicates in master list, where a synonym is corrected here
            tmp_master= data.frame(X = 1,
                                   childtaxa_id = taxid,
                                   childtaxa_name = species$name,
                                   rank = "species")
            out_master = rbind(out_master, tmp_master)#output to master
          } else {#no species found
            genus = subset(class, rank == "genus")
            if (dim(genus)[1]>0){
              tmp$pathogen_level = "genus"
              tmp_master= data.frame(X = 1,
                                     childtaxa_id = taxid,
                                     childtaxa_name = genus$name,
                                     rank = "genus")
              out_master = rbind(out_master, tmp_master)#output to master
            } else {
              family = subset(class, rank == "family")
              if (dim(family)[1]>0){
                tmp$pathogen_level = "family"
                tmp_master= data.frame(X = 1,
                                       childtaxa_id = taxid,
                                       childtaxa_name = family$name,
                                       rank = "family")
                out_master = rbind(out_master, tmp_master)#output to master
              # tmp$pathogen_level = "unknown"
              # print("check")
              # print(tmp$pathogen)
              }
            }
          }#end else for check
        }
      }
    }
    out = rbind(out, tmp)
}#end row of df

#hand-check not_found
not_found = subset(out, pathogen_level == "not found in ncbi")
write.csv(not_found, file = "not_found.csv")
#combine out with df_no_parasite
df_all = rbind(out, df_no_parasite)
save(df_all, file = "df_all.Rdata")
# save(df_all, file = "../DATA/df_all.Rdata")
#df: remove X, add out_master, find unique
df = df[,c("childtaxa_id", "childtaxa_name", "rank")]
out_master= out_master[,c("childtaxa_id", "childtaxa_name", "rank")]
df = rbind(df, out_master)
bacteria_species = df
save(bacteria_species, file = "bacteria_species.Rdata")
print(dim(df))
df = unique(df)
print(dim(df))
#separate file: use classification to get order -- wait and try to get that from BacDive

save(out_synonym, file = "out_synonym.Rdata")