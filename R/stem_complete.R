#' @export stem_complete
stem_complete <- function(txt,lang = "en"){
  library(stringr)
  library(dplyr)
  library(stringdist)
  library(data.table)
# save(file="dom",list=ls())
  # load("C:/Users/Dominique/Desktop/Stat_Regie/data/application_data/dom")
  txt<-factor(str_split(paste(txt,collapse   = " MOT_SEPARATEUR_DE_VERBATIM ")," ")[[1]])
  txt<-txt[str_trim(txt)!=""]
  length(txt)
  t<-table(txt)
  l <- levels(txt)
  l2<-sapply(l,SnowballC::wordStem,language=lang)
  out <- data.frame(l=l,l2=l2,t=as.vector(t),stringsAsFactors = FALSE)
  x<-out %>%group_by(l2)%>%summarise(t=max(t))
  x<-inner_join(out,x,by=c("l2","t"))
  x<-dplyr::rename(x,l3=l)
  x<-x%>%dplyr::select(-t)%>%as.data.table
  x[,d:=stringdist(l2,l3,method="osa") + stringdist(l2,l3,method="cosine")/5  + stringdist(l2,l3,method="soundex")/20 ]
  y<-x%>%group_by(l2)%>%summarise(d=min(d))
  x<-inner_join(x,y,by=c("l2","d"))%>%as.data.table
  is(x)
  x<-x%>%dplyr::select(l2,l3)
  setkeyv(x,"l2")
  x<-x%>%unique
  x<-inner_join(out,x,by=c("l2"))%>%as.data.table
  l3<-plyr::mapvalues(l,x$l,x$l3,warn_missing = FALSE)
  l3<-ifelse(is.na(l3),l,l3)
  levels(txt) <- l3
  txt<-as.character(txt)
  txt <- paste(txt,collapse=" ")
  txt <- str_trim(str_split(txt,"MOT_SEPARATEUR_DE_VERBATIM")[[1]])
  return(txt)


}


