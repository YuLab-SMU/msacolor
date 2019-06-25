#' colorscheme2
#'
#' @param seq aligned data frame
#' @param scheme_df  color scheme data frame
#' @return  color matched with sequence
#'
#' @export
#'
#' @examples
#' color_scheme_2(sampleseq,chemi_aa_coldf)
#' @author huina huang

color_scheme_2 <- function(seq, scheme_df){
  col <- rep(scheme_df$color, times = nchar(as.character(scheme_df[,1])))
  names(col) <- unlist(strsplit(as.character(scheme_df[,1]), ""))
  seqcolor <- matrix(col[toupper(as.matrix(seq))], ncol = ncol(as.matrix(seq)))
  return(seqcolor)
}