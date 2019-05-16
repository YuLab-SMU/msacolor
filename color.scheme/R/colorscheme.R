#' colorscheme
#'
#' @param seq aligned data frame
#' @param scheme_df  color scheme data frame
#' @importFrom dplyr %>%
#' @return  color matched with sequence
#'
#' @export
#'
#' @examples
#' color_scheme(sampleseq,col_df)
#' @author huina huang
color_scheme<-function(seq,scheme_df){
  #preprocess data
  seq<-as.matrix(seq)
  seq<-toupper(seq)         ##make sure the suquence is capital format


  seqlist <- apply(seq, 2, table)  ##calculate character frequency
  re_gp<-sapply(scheme_df$re_gp,function(i){strsplit(i, '')[[1]]})  ##seperate the scheme_df$re_gp

  ##assign color to character based from frequency
  col_convert <- lapply(seqlist, function(x) {
    y <- rep("white", length(x))
    names(y) <- names(x)
    r <- x/sum(x)
    y<-sapply(seq_along(x),function(pos){
      char <- names(x)[pos]
      i <- grep(char, col_df$re_position)
      for (j in i) {
        rr<-r[re_gp[[j]]]
        rr<-rr[!(is.na(rr))]     ##get frequency of the character in the col_df$re_gp[j]
        
        ##the situation without , in 're_gp'
        if (sum(rr) > col_df$thred[j] && col_df$type[j]=="combined" && length(rr)>1) {
          y[pos] <- col_df$colour[j]
          break
        }
        
        ##the situation with , in 're_gp'
        if(any(rr>col_df$thred[j])  && col_df$type[j]=="individual"){
          y[pos] <- col_df$colour[j]
          break
        }
      }
      return(y[pos])
    })
    return(y)
  })

  seqcolor <- lapply(seq_along(col_convert), function(i) {
    col_convert[[i]][seq[,i]]
  }) %>% do.call('cbind', .)

  return(seqcolor)

}


