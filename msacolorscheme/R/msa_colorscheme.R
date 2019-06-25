#' msa_color_sheme
#'
#' @param seq aligned data frame
#' @param scheme_df  color scheme data frame

#' @return  color matched with sequence
#'
#' @export
#'
#' @examples
#' msa_color_sheme(sampleseq, "chemi_aa_coldf")
#' @author huina huang
msa_color_sheme <- function(seq, scheme_df) {
  switch(scheme_df, 
         col_df =  color_scheme(seq, col_df),
         chemi_aa_coldf = color_scheme_2(seq, chemi_aa_coldf),
         chemi_nucle_coldf = color_scheme_2(seq, chemi_nucle_coldf),
         shape_aa_coldf = color_scheme_2(seq, shape_aa_coldf),
         shape_nucle_coldf = color_scheme_2(seq, shape_nucle_coldf),
         solid_nucle_dark_coldf = color_scheme_2(seq, solid_nucle_dark_coldf),
         solid_nucle_dimmed_coldf = color_scheme_2(seq, solid_nucle_dimmed_coldf),
         Taylor_aa_coldf = color_scheme_2(seq, Taylor_aa_coldf),
         Taylor_nucle_coldf = color_scheme_2(seq, Taylor_nucle_coldf),
         Zappo_aa_coldf = color_scheme_2(seq, Zappo_aa_coldf),
         Zappo_nucle_coldf = color_scheme_2(seq, Zappo_nucle_coldf)
  )
}