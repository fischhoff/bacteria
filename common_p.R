load("p1.Rdata")
load("p2.Rdata")

common = intersect(p1$Organism_name,
                   p2$Organism_name)
length(common)
