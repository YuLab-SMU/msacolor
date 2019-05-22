context("Color Scheme")

library(color.scheme)
data(col_df)
seq1 <- as.matrix(c('A','H','H','W','W','Y','T','S','M','H'))
seq1_color <-c("blue","cyan","cyan","blue","blue","cyan","green","green","blue","cyan")


test_that("color_scheme is a color scheme of sequences aligned", {
  expect_equal(as.vector(color_scheme(seq1, col_df)), seq1_color)
})
