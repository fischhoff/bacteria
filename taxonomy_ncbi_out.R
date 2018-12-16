load("p1.Rdata")
species_ncbi = fread("tax_report.txt")
species_ncbi = data.frame(species_ncbi)
df = species_ncbi[c(2:dim(species_ncbi)[1]),]
# df$name = str_replace_all(df$name, '\"', "")
df <- df %>% 
  mutate(name = str_replace_all(name, '\"', "")
         )
head(df$name)
names(df)[names(df)=="name"]="Organism_name"

#find number with non-preferred species name
print("number with non-preferred species name")
print(length(setdiff(df$Organism_name,df$preferred.name)))
#this should be TRUE
length(intersect(p1$Organism_name,df$Organism_name)) == dim(p1)[1]

p1 = merge(p1, df, by = "Organism_name")
save(p1, file = "p1.Rdata")
