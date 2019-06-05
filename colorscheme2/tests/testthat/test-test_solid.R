context("Color Scheme according to solid")

library(colorscheme2)
data(solid_nucle_dimmed_coldf)
data(solid_nucle_dark_coldf)
solid_dimmed_seq <- as.matrix(c("A","T","A","G","C","G","-"))
solid_dimmed_seqcolor <- c("light pink","light green","light pink","light orange","light blue","light orange","white")
solid_dark_seq <- as.matrix(c("A","T","A","G","C","G","-"))
solid_dark_seqcolor <- c("dark pink","dark green","dark pink","dark orange","dark blue","dark orange","white")


test_that("the situation that color_scheme_2 is a color scheme of sequences which are nucleotides immediately adjacent to the alignment block", {
  expect_equal(as.vector(color_scheme_2(solid_dimmed_seq, solid_nucle_dimmed_coldf)), solid_dimmed_seqcolor)
})


test_that("the situation that color_scheme_2 is a color scheme of sequences which are amino acids aligned", {
  expect_equal(as.vector(color_scheme_2(solid_dark_seq, solid_nucle_dark_coldf)), solid_dark_seqcolor)
})