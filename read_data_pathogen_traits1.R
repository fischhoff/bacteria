p1 = fread("ProTraits_binaryIntegratedPr0.90.txt",blank.lines.skip=TRUE)
save(p1, file = "p1.Rdata")
print("dimensions")
print(dim(p1))
