context("Color Scheme according to Taylor")

library(msacolorscheme)
data(Taylor_aa_coldf)
data(Taylor_nucle_coldf)
Taylor_seq <- as.matrix(c("A","T","A","G","C","G","-"))
Taylor_seqcolor <-c("lime-grass","red","lime-grass","purple","yellow","purple","white")


test_that("the situation that color_scheme_2 is a color scheme of sequences which are nucleotides aligned", {
  expect_equal(as.vector(msa_color_sheme(Taylor_seq, "Taylor_nucle_coldf")), Taylor_seqcolor)
})

Taylor_aa_seq<-c("A","F","W","-","R","S","D")
Taylor_aa_seqcolor<-c("lemon","emerald","cyan","white","blue","scarlet","red")
test_that("the situation that color_scheme_2 is a color scheme of sequences which are amino acids aligned", {
  expect_equal(as.vector(msa_color_sheme(Taylor_aa_seq, "Taylor_aa_coldf")), Taylor_aa_seqcolor)
})
