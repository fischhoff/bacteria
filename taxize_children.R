
# install.packages("taxize")
# library("taxize")
#this returns species in genus Apis
ncbi_downstream(id = 7459, downto="species")

#this returns an error
# id = 2
# bacteria_species_ncbi = ncbi_downstream(id = 2, downto = "species")
