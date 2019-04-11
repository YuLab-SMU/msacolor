#'colorscheme
#'
#' @param seq aligned data frame
#' @param scheme_df  color scheme data frame
#' @importFrom stringr str_detect
#' @importFrom stringr str_match
#' @return  color matched with sequence
#'
#' @export
#'
#' @examples
#' color_scheme(sampleseq,col_df)
#' @author huina huang 
color_scheme<-function(seq,scheme_df){
  #preprocess data
  seq<-as.matrix(testseq)       
  seq<-toupper(seq)         #make sure the suquence is capital format
  scheme_df<-scheme_df[-24,]    #remove the last line containing character "-" in scheme_df
  seqnum<-dim(seq)[1]    #get the sequence number of aligned sequence 
  
  
  
  seqcolor<-apply(seq,2,function(col_seq,col_df=scheme_df,num=seqnum){
    
    color<-rep("white",times=num)  #the default color is white
    
    
    numdf<-as.data.frame(table(factor(col_seq)))  #get the number of each type of character in the column sequence
    numdf$Freq<-numdf$Freq/num                  #get the frequency of each type of character in the column sequence
    numdf$Var1<-as.character(numdf$Var1)        
    numdf<-numdf[!(numdf$Var1 %in% '-'),]       #remove the line containing the type of character "-"
    chars<-numdf$Var1                #chars is a vector containing type of character in the column sequence except "-"
    fre<-numdf$Fre                   #fre is a vector containing frequency of chars in the column sequence
    
    gap_index<-which(!(is.na(match(col_seq,'-'))))  #get all location of "-" in the column sequence
    color[gap_index]<-"white"                       
   
    
    #match each type of character in the column sequence with colour except "-"
    for(i in 1:nrow(numdf)){
      
      e<-chars[i]          #e is one type of chars in the column sequence  
      e_index<-which(!(is.na(match(col_seq,e))))  #get all location of e in the column sequence
      col_df1<-col_df[str_detect(col_df$re_position,e),]  #extract the lines with e in re_position frome color scheme data frame 

      
      for(j in 1:nrow(col_df1)){
        gp<-col_df1$re_gp[j]      #gp is the thredthold character group related to thredthold 
        
        group<-str_match(gp,chars)    
        group<-group[!is.na(group)]    #find character both in gp and chars 
        if(length(group)==0){next;}  #if the column sequence do not have character which is in gp,then skip into next circulation
        
        group_ind<-which(!(is.na(match(chars,group))))   #find the group character position in chars in order to find the frequency of them 
        if(col_df1$type[j]=="individual" && any(fre[group_ind]>col_df1$thred[j])){
          color[e_index]<-col_df1$colour[j]     #if conditions are met, match the type of e in the column sequence with col_df1$colour[j]
          i=i+1    
          break   #finish matching the type of e with color and start to match next character type with color
        }else{
          if(col_df1$type[j]=="combined" && length(group_ind)>1 && sum(fre[group_ind],na.rm = T)>col_df1$thred[j]){
            color[e_index]<-col_df1$colour[j]
            i=i+1
            break
            }
        }
        
      }
      
  }
    
    
    return(color)
})  
  return(seqcolor)
  
}
  

