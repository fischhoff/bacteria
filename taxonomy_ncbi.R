load("p1.Rdata")
species = p1$Organism_name
#need to make row.names =FALSE for output to be recognized as species in NCBI webpage (webpage needs only one column)
#.txt or .csv work
write.csv(species, file = "species.csv", row.names=FALSE)
# write.table(species, file = "species.txt", row.names = FALSE)