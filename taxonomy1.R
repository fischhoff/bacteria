load("p1.Rdata")
db = "col"
p1$Organism_name_corrected = synonyms(as.character(p1$Organism_name),
                                      db = db)
print(head(p1$Organism_name_corrected))
# print(head(p1[,c("Organism_name", "Organism_name_corrected")]))