context("Color Scheme according to the RasMol 'shapely models'")

library(msacolorscheme)
data(shape_nucle_coldf)
data(shape_aa_coldf)
shape_seq <- as.matrix(c("A","T","A","G","C","G","-"))
shape_seqcolor <-c("light blue","light green","light blue","medium salmon","light orange","medium salmon","white")


test_that("the situation that color_scheme_2 is a color scheme of sequences which are nucleotides aligned", {
  expect_equal(as.vector(msa_color_sheme(shape_seq, "shape_nucle_coldf")), shape_seqcolor)
})

shape_aa_seq<-c("A","F","W","-","R","S","D")
shape_aa_seqcolor<-c("dark grey","mid blue","purple","white","blue","orange","bright red")
test_that("the situation that color_scheme_2 is a color scheme of sequences which are amino acids aligned", {
  expect_equal(as.vector(msa_color_sheme(shape_aa_seq, "shape_aa_coldf")), shape_aa_seqcolor)
})
