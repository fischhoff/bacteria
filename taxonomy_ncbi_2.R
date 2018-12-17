load("p2.Rdata")
p2$Organism_name = paste(p2$Genus, p2$Species)
dim(p2)[1]
#get unique rows
p2u = unique(p2)
dim(p2u)[1]
#reassign p2 
p2 = p2u
df <- p2 %>% count(Organism_name)
df_x = subset(df, n>1)
#number of species with multiple, non-unique rows
print(dim(df_x)[1])
#examine one of them, confirm that it differs in values for some columns
df_x_sample = subset(p2, Organism_name == df_x$Organism_name[1])

#get the organisms with one row
df_u = subset(df, n == 1)
#get just the rows of organisms with one row
p2 <- subset(p2, Organism_name %in% df_u$Organism_name)
dim(p2)[1]
save(p2, file = "p2.Rdata")
df = p2
species = p2$Organism_name
#need to make row.names =FALSE for output to be recognized as species in NCBI webpage (webpage needs only one column)
#.txt or .csv work
write.csv(species, file = "species2.csv", row.names=FALSE)
