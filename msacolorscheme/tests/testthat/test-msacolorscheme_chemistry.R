context("Color Scheme according to chemistry")

library(msacolorscheme)
data(chemi_aa_coldf)
data(chemi_aa_coldf)
chemi_nu_seq <- as.matrix(c("A","T","A","G","C","G","C"))
chemi_nu_seqcolor <-c("red","green","red","orange","blue","orange","blue")


test_that("the situation that color_scheme_2 is a color scheme of sequences which are nucleotides aligned", {
  expect_equal(as.vector(msa_color_sheme(chemi_nu_seq, "chemi_nucle_coldf")), chemi_nu_seqcolor)
})

chemi_aa_seq<-c("A","F","W","-","R","S","D")
chemi_aa_seqcolor<-c("orange","yellow","yellow","white","blue","green","pink")
test_that("the situation that color_scheme_2 is a color scheme of sequences which are amino acids aligned", {
  expect_equal(as.vector(msa_color_sheme(chemi_aa_seq, "chemi_aa_coldf")), chemi_aa_seqcolor)
})

