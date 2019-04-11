#'colorscheme
#'
#' @param seq aligned data frame
#' @param num number of aligned sequence
#' @importFrom stringr str_detect
#' @importFrom stringr str_split
#' @return  color matched with sequence
#'
#' @export
#'
#' @examples
#' color_scheme(sampleseq)
#' @author huina huang 
color_scheme<-function(seq){
  seq<-as.matrix(seq)       #将数据框形式的多序列比对的序列格式转换为矩阵
 
  seq<-toupper(seq)         #确保进行操作比对的字母是大写形式
  
  #以多序列比对结果的列col_seq为单位，对seq进行配色
  seqcolor<-apply(seq,2,function(col_seq){
    num=length(col_seq)            #得到多序列比对的序列条数
    color<-rep("white",times=num)  #默认每个字母颜色为白色
    
    #创建颜色方案的数据框索引
    category<-rep(c("Hydrophobic","Positive charge","Negative charge","Polar","Cysteines","Glycines","Prolines","Aromatic","Unconserved"),c(2,2,6,8,1,1,1,2,1))
    colour<-rep(c("blue","red","magenta","green","pink","orange","yellow","cyan","white"),c(2,2,6,8,1,1,1,2,1))
    re_position<-rep(c('A,I,L,M,F,W,V','C','K,R','E','D','N','Q','S,T','C','G','P','H,Y',"-,any"),c(1,1,2,3,3,2,3,3,1,1,1,2,1))
    thred<-c(rep(0.6,each=3),0.8,0.6,0.5,0.85,0.6,0.85,0.5,0.5,0.85,rep(c(0.6,0.5,0.85),times=2),0.85,0,0,0.6,0.85,0)
    re_gp<-c(rep('WLVIMAFCHP',each=2),'KR','KRQ','KR','QE','EQD','KR','KRQ','ED','N','NY','KR','QE','QEKR','WLVIMAFCHP','TS','ST','C','G','P','WLVIMAFCHP','WYACPQFHILMV',"if none of the above criteria are met")
    type<-rep("combined",times=23)
    type[24]<-"any"
    type[c(4,7,9,11,12,15,18,19,20,21,23)]<-"individual"
    col_df<-data.frame(category,colour,re_position,thred,re_gp,type)
    col_df$colour<-as.character(col_df$colour)
    col_df$re_gp<-as.character(col_df$re_gp)
    col_df$re_position<-as.character(col_df$re_position)
    col_df$type<-as.character(col_df$type)
    
    #对col_seq的每个字母进行判定配色
    for(i in 1:num){
      e<-col_seq[i]
      if(e=='-'){
        color[i]='white'
      }else{
        
        len<-dim(col_df)[1]  #得到col_df的行数len
        for(j in 1:len){     #e对颜色方案的数据框col_df进行遍历，以col_df$re_position和col_df$re_gp和col_df$thred为判定条件
          
          if(str_detect(col_df$re_position[j],e)){    #判断e是否出现在col_df$re_position[j]上，判断配色的第一条件
            
            #根据阈值字符组合col_df$re_gp[j]有无逗号，分两种情况即combinded和individual情况讨论阈值问题
            if(str_detect(col_df$re_gp[j],",")){    #individual情况，阈值字符组合中任一字符在col_seq出现的频率大于阈值即可配色
              
              gp<-unlist(str_split(col_df$re_gp[j],","))  #以逗号为分隔符，分割阈值字符组合为含有单个字符的向量gp
              countdf<-table(factor(col_seq,levels=gp))  #以gp为因子，计算col_seq中出现在阈值组合的各字符的频数
              countdf<-as.data.frame(countdf)    #转换为数据框以便后面计算
              fre<-countdf$Freq/num      #fre为频率向量，含有阈值组合中各字符在col_seq序列中的频率
              if(any(fre>col_df$thred[j])){color[i]<-col_df$colour[j];break} #individual情况下，任一字母的频率大于阈值便配色
            }else{
              #combind情况，先计算阈值，再根据阈值字符组合单个或多个的情况判断阈值配色
              gp<-unlist(str_split(col_df$re_gp[j],""))   #分割阈值字符组合为含有单个字符的向量gp
              countdf<-table(factor(col_seq,levels=gp))  #以gp为因子，计算col_seq中出现在阈值组合的各字符的频数
              countdf<-as.data.frame(countdf)            #转换为数据框以便后面计算
              countdf$Freq<-countdf$Freq/num             #除以col_seq总长得到频率
              sumfre<-sum(countdf$Freq)                  #sumfre为col_seq中所有出现在阈值字符组合col_df$re_gp[j]的总频率之和
              if(length(countdf$Freq[countdf$Freq>0])==1){    #combinded情况下，若阈值字符组合为单个则按照颜色方案的四个特殊情况判定配色
                
                if(col_df$re_gp[j]=='C'&& sumfre>0.85){color[i]<-"pink";break}else{
                  if(col_df$re_gp[j]=='G'&& sumfre>0){color[i]<-"orange";break}else{
                    if(col_df$re_gp[j]=='P'&& sumfre>0){color[i]<-"yellow";break}else{
                      if(col_df$re_gp[j]=='N'&& sumfre>0.5){color[i]<-"green";break}
                    }
                  }
                  
                }
                
                
                
              }else{  #combinded情况下，若阈值字符组合为多个，直接判断
                
                if(sumfre>col_df$thred[j]){color[i]<-col_df$colour[j];break}  #sumfre与阈值比较，匹配颜色
                
              }
            }
          }
        }
      }
    }
    return(color)  
  })
  return(seqcolor)
  
}