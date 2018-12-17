
load("p1.Rdata")
df = p1
#replace "?" with NA
#https://cran.r-project.org/web/packages/naniar/vignettes/replace-with-na.html
df <- df %>% replace_with_na_all(condition = ~.x == "?")
#for numeric columns
cols.num <- c(3:426)
#make numeric
#https://stackoverflow.com/questions/22772279/converting-multiple-columns-from-character-to-numeric-format-in-r
df[cols.num] <- sapply(df[cols.num],as.numeric)

#obtain correlation matrix for numeric columns 
mat= df[sapply(df, is.numeric)]
mat = mat[,c(2:425)]#remove taxid and code columns
mat = as.matrix(mat)

res <- cor(mat, use = "pairwise.complete.obs")
# corrplot(res, type = "upper", 
#          tl.pos = "n",
#          na.label = "")
