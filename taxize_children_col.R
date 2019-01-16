#https://cran.r-project.org/web/packages/taxize/vignettes/taxize_vignette.html
#get id by running line below and choosing entry for bacteria at kingdom level
bac_out = get_colid("Bacteria")
id = "36bea735613185bbd9ce135fb0d9382c"

bacteria_species = downstream(id, downto = "species", db = "col")

# print(head(bacteria_species))
# print(dim(bacteria_species))
bac_sp_df = data.frame(bacteria_species)
