load("p2.Rdata")
dfp = p2
#tmp = read.csv("species2.csv")
species_ncbi = fread("tax_report2.txt")
species_ncbi = data.frame(species_ncbi)
#first row is "x"
df = species_ncbi[c(2:dim(species_ncbi)[1]),]

# df$name = str_replace_all(df$name, '\"', "")
df <- df %>% 
  mutate(name = str_replace_all(name, '\"', "")
         )
head(df$name)
names(df)[names(df)=="name"]="Organism_name"

df_ct <- df %>% count(Organism_name)
df_x = subset(df_ct, n>1)

#check how many there are
print(dim(df_x)[1])

#there are two different preferred names for some organisms!
df_x_sample = subset(df, Organism_name == df_x$Organism_name[1])

#remove from traits dataset the organisms that have multiple preferred names
df_1 = subset(df_ct, n == 1)

#remove from name translation dataset
df = subset(df, Organism_name %in% df_1$Organism_name)
#remove  from traits dataset
dfp = subset(dfp, Organism_name %in% df$preferred.name)
dim(p2)[1]


#find number with non-preferred species name
print("number with non-preferred species name")
print(length(setdiff(df$Organism_name,df$preferred.name)))
#this should be TRUE
print("check")
print(length(intersect(dfp$Organism_name,df$Organism_name)) == dim(dfp)[1])

dfp = merge(dfp, df, by = "Organism_name")
p2 = dfp
save(p2, file = "p2.Rdata")
