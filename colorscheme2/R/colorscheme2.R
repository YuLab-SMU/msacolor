#' colorscheme2
#'
#' @param seq aligned data frame
#' @param scheme_df  color scheme data frame
#' @importFrom dplyr %>%
#' @return  color matched with sequence
#'
#' @export
#'
#' @examples
#' color_scheme_2(sampleseq,chemi_aa_coldf)
#' @author huina huang
color_scheme_2 <-function(seq, scheme_df){
  seq <- toupper(as.matrix(seq))    ##make sure the suquence is capital format
  seq_unique <- unique(unlist(as.list(seq)))  ##remain the unique character in seq and save them in seq_unique
  
  ##get the matched lines of each character of seq_unique in scheme_df
  line <- vapply(seq_unique, function(aa) i <- grep(aa,scheme_df[,1]),numeric(1))  
  
  ##assign color to character in seq_unique
  y <- rep("white", length(line))
  names(y) <- names(line)
  col_convert <- vapply(line, function(pos){
    y[pos] <- scheme_df$color[pos] %>% as.character
  }, character(1)) 
  
  
  seqcolor <- col_convert[seq] 
  dim(seqcolor) <- dim(seq) 
  return(seqcolor)
}




