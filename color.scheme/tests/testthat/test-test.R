context("Color Scheme")
library(color.scheme)
data(col_df)
data(testseq)
data(testseqcolor)

test_that("color_scheme is a color scheme of sequences aligned", {
  expect_equal(as.character(color_scheme(testseq, col_df)), 
               as.character(testseqcolor))
})
