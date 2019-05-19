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
    r <- x/sum(x)
    xi<-lapply(names(x),function(char){grep(char, col_df$re_position)})  ##get the matched lines of each character in x
    names(xi)<-names(x)
    xxi<-xi[!duplicated(xi)] ##remain the non-duplicated element of xi in xxi 
    
    y<-sapply(seq_along(xxi),function(pos){
      y <- rep("white", length(xxi))
      names(y) <- names(xxi)
      char <- names(xxi)[pos]
      i<-xxi[[pos]]
      
      for (j in i) {
        rr<-r[re_gp[[j]]]
        rr<-rr[!(is.na(rr))]     ##get frequency of the character in the col_df$re_gp[j]
        
        ##the situation without , in re_gp, that is to say the type of 're_gp' is 'combined'
        if (sum(rr) > col_df$thred[j] && col_df$type[j]=="combined" && length(rr)>1) {
          y[pos] <- col_df$colour[j]
          break
        }
        
        ##the situation with , in re_gp, that is to say the type of 're_gp' is 'individual'
        if(any(rr>col_df$thred[j])  && col_df$type[j]=="individual"){
          y[pos] <- col_df$colour[j]
          break
        }
      }
      return(y[pos])
    })
    
    dup_aa<-xi[duplicated(xi)]   ##remain the duplicated element of xi in dup_aa
    
    ##mapping the color to the character in dup_aa which are the duplicated element in xi
    dup_color<-sapply(dup_aa,function(aa){
      col<-y[xxi %in% list(aa)]
      col<-unlist(col)
      names(col)<-names(aa)
      return(col)
    })
    
    yy<-c(y,dup_color)
    return(yy)
  })

  seqcolor <- lapply(seq_along(col_convert), function(i) {
    col_convert[[i]][seq[,i]]
  }) %>% do.call('cbind', .)

  return(seqcolor)

}


