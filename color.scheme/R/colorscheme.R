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
color_scheme <- function(seq, scheme_df) {
  #preprocess data
  seq <- as.matrix(seq)
  seq <- toupper(seq)       ##make sure the suquence is capital format


  seqlist <- apply(seq, 2, table)  ##calculate character frequency
  ##seperate the re_gp
  re_gp <- sapply(scheme_df$re_gp, function(i) {
    strsplit(i, "")[[1]]
  }) 


  
  ##assign color to character based from frequency
  col_convert <- lapply(seqlist, function(x) {
    r <- x/sum(x)
    
    ##get the matched lines of each character of x in scheme_df
    xi <- lapply(names(x), function(char) {
      grep(char, scheme_df$re_position)
    })      
    names(xi) <- names(x)
    
    ##seperate the character in xi based on the matched lines of character whether are duplicated 
    xxi <- xi[!duplicated(xi)]  ##remain the non-duplicated element of xi 
    dup_aa <- xi[duplicated(xi)]   
    
    
    ##assign color to character which are the names of elements in xxi 
    y <- sapply(seq_along(xxi), function(pos) {
      y <- rep("white", length(xxi))
      names(y) <- names(xxi)
      char <- names(xxi)[pos]
      i <- xxi[[pos]]
      
      for (j in i) {
        rr <- r[re_gp[[j]]]
        rr <- rr[!(is.na(rr))]     ##get frequency of the character in the scheme_df$re_gp[j]

        ##the situation without , in re_gp, that is to say the type of 're_gp' is 'combined'
        if (sum(rr) > scheme_df$thred[j] && scheme_df$type[j] == "combined" && length(rr) > 1) {
          y[pos] <- scheme_df$colour[j]
          break
        }

        ##the situation with , in re_gp, that is to say the type of 're_gp' is 'individual'
        if (any(rr > scheme_df$thred[j]) && scheme_df$type[j] == "individual") {
          y[pos] <- scheme_df$colour[j]
          break
        }
      }
      
      return(y[pos])
    })
    
   
    
    ##assign color to the character in dup_aa based from the element the same as that of xxi
    dup_color <- sapply(dup_aa, function(aa) {
      col <- y[xxi %in% list(aa)]
      col <- unlist(col)
      names(col) <- names(aa)
      return(col)
    })

  
    ##combine the color of character in xxi and dup_aa 
    yy <- c(y, dup_color)
    return(yy)
  })

  
  seqcolor <- lapply(seq_along(col_convert), function(i) {
    col_convert[[i]][seq[,i]]
  }) %>% do.call('cbind', .)

  return(seqcolor)
}


