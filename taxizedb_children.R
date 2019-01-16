devtools::install_github("ropensci/taxizedb")
x <- downstream("Bryophyta", db = "ncbi", downto="family")
tibble::as_tibble(x$Bryophyta)

# b <- downstream("Bacteria", db = "ncbi", downto="species", out_type = "summary")
# tibble::as_tibble(b$Bacteria)