context("Color Scheme according to col_df")

library(msacolorscheme)
data(col_df)
seq1 <- as.matrix(c('A','H','H','W','W','Y','T','S','M','H'))
seq1_color <-c("blue","cyan","cyan","blue","blue","cyan","green","green","blue","cyan")


test_that("color_scheme is a color scheme of sequences aligned", {
  expect_equal(as.vector(msa_color_sheme(seq1, "col_df")), seq1_color)
})
