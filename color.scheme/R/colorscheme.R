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
  re_gp <- lapply(scheme_df$re_gp, function(i) strsplit(i, "")[[1]])
  
  ##the situation that the seq has only a column
  if(!(is.list(seqlist))){
    seqlist1 <- as.vector(seqlist)
    names(seqlist1) <- rownames(seqlist)
    seqlist2 <- list(as.table(seqlist1))
    seqlist<-seqlist2
  }
  
  
  ##assign color to character based from frequency
  col_convert <- lapply(seqlist, function(x) {
    r <- x/sum(x)
    
    ##get the matched lines of each character of x in scheme_df
    re_pos <- lapply(names(x), function(char) {
      grep(char, scheme_df$re_position)
    })      
    names(re_pos) <- names(x)
    
    ##seperate the character in re_pos based on the matched lines of character whether are duplicated 
    re_pos_unique <- re_pos[!duplicated(re_pos)] ##remain the non-duplicated element of re_position 
    re_pos_dup <- re_pos[duplicated(re_pos)]   
    
    
    ##assign color to character which are the names of elements in re_pos_unique 
    unique_color <- lapply(seq_along(re_pos_unique), function(pos) {
      y <- rep("white", length(re_pos_unique))
      names(y) <- names(re_pos_unique)
      char <- names(re_pos_unique)[pos]
      i <- re_pos_unique[[pos]]
      
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
    }) %>% unlist
    
    
    ##assign color to the character which are the names of elements in re_pos_dup 
    ##based from the element the same as that of re_pos_unique
    dup_color <- lapply(re_pos_dup, function(aa) {
      col <- unique_color[re_pos_unique %in% list(aa)]
      col <- unlist(col)
      names(col) <- names(aa)
      return(col)
    }) %>% unlist
    
    
    ##combine the color of character which are the names of elements in re_pos_unique and dup_color
    char_col <- c(unique_color, dup_color)
    return(char_col)
  })
  
  
  seqcolor <- lapply(seq_along(col_convert), function(i) {
    col_convert[[i]][seq[, i]]
  }) %>% do.call('cbind', .)
  
  return(seqcolor)
}


