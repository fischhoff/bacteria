load("df_parasite.Rdata")
p1= df_parasite
#if this comes in with numbers associated with each species, that's because row.names =TRUE when species names were exported for NCBI
species_ncbi = fread("parasiteGMPD_tax_report.txt")
#this doesn't work
 #species_ncbi = read.table("parasiteGMPD_tax_report.txt")
#species_ncbi = read.csv("parasiteGMPD_tax_report.csv")
species_ncbi = data.frame(species_ncbi)
df = species_ncbi[c(2:dim(species_ncbi)[1]),]
names(df)[names(df)=="name"]="Organism_name"
#names(df)[names(df)=="X..1"]="Organism_name"

df <- df %>% 
  mutate(Organism_name = str_replace_all(Organism_name, '\"', "")
         )
head(df$Organism_name)
head(df$preferred.name)

#find number with non-preferred species name
print("number with non-preferred species name")
print(length(setdiff(df$Organism_name,df$preferred.name)))
inds.diff = which(df$Organism_name != df$preferred.name)
check = df[inds.diff,]
print("number with preferred species name")
print(length(intersect(df$Organism_name,df$preferred.name)))

#this should be TRUE if each  parasite occurs only once in p1, else FALSE
length(intersect(p1$parasiteGMPD,df$Organism_name)) == dim(p1)[1]
names(df)[names(df)=="Organism_name"]="parasiteGMPD"
df = df[,c("parasiteGMPD",
           "preferred.name",
           "taxid")]
p1 = merge(p1, df, by = "parasiteGMPD")
#get unique rows
p1= unique(p1)
df_parasite = p1

load("df_no_parasite.Rdata")
#add missing fields
df_no_parasite$preferred.name = ""
df_no_parasite$taxid = NA
#put together data with parasite and without
df_parasite = rbind(df_parasite, df_no_parasite)
save(df_parasite,
     file = "df_parasite.Rdata")

#save(p1, file = "p1.Rdata")
