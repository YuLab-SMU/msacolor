context("Color Scheme according to Zappo")

library(msacolorscheme)
data(Zappo_aa_coldf)
data(Zappo_nucle_coldf)
Zappo_seq <- as.matrix(c("A","T","A","G","C","G","-"))
Zappo_seqcolor <-c("red","blue","red","purple","carnatio","purple","white")


test_that("the situation that color_scheme_2 is a color scheme of sequences which are nucleotides aligned", {
  expect_equal(as.vector(msa_color_sheme(Zappo_seq, "Zappo_nucle_coldf")), Zappo_seqcolor)
})

Zappo_aa_seq<-c("A","F","W","-","R","S","D")
Zappo_aa_seqcolor<-c("salmon","orange","orange","white","blue","green","red")
test_that("the situation that color_scheme_2 is a color scheme of sequences which are amino acids aligned", {
  expect_equal(as.vector(msa_color_sheme(Zappo_aa_seq, "Zappo_aa_coldf")), Zappo_aa_seqcolor)
})