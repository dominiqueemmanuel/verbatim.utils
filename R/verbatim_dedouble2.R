#' @export verbatim_dedouble2
#'
#'
#'
verbatim_dedouble2<-function(txt,ngram_min= 3L, ngram_max=5L, seuil = 0.25){
  # ngram_min= 3; ngram_max=5L; seuil = 0.25
  library(tokenizers)
  library(dplyr)
  library(stringr)
  library(Matrix)
  library(igraph)
  library(data.table)
  names(txt)<-NULL
  print("0. Simple deduplication")
  txt<-data.table(txt=txt,id=seq_along(txt),key="txt")
  txt2<-unique(txt)
  setnames(txt2,"id","id2")
  txt2<-txt[txt2]
  txt2<-txt2[order(txt2$id),]
  id2<-txt2$id2
  id<-which(!duplicated(txt2$id2))
  txt<-txt2$txt[id]



  print("1. begin tokenize_ngrams")
  x<-tokenizers::tokenize_ngrams(x = txt,lowercase = FALSE,n=ngram_max,n_min = ngram_min)
  print("2. treatments")
  y0<-unlist(x)
  z0<-unlist(lapply(seq_along(x),function(t)rep(t,length(x[[t]]))))
  y<-data.table(ng=y0)
  dim(y)
  y[,un:=1]
  print("3. unique")
  system.time(y<-y[,.(freq=length(un)),by="ng"])
  y<-subset(y,freq>1)
  dim(y)
  print("4. number of words")
  y$nw<-sapply(tokenize_words(y$ng, lowercase = FALSE),length)
  y<-subset(y,nw<=ngram_max)
  q<-data.table(ng=y0,id=z0)
  print("5. joins")
  setkeyv(q,"ng")
  setkeyv(y,"ng")
  z<-y[q]
  z[,freq:=ifelse(is.na(freq),0,freq)]
  z[,nw:=ifelse(is.na(nw),0,nw)]
  print("6. sort")
  setorder(z,-nw,-freq,ng)
  z2<-unique(z,by="id")
  z3<-data.table(id=seq_along(txt),ng2=txt)
  setkeyv(z2,"id")
  setkeyv(z3,"id")
  z3<-z2[z3]
  z3[,ng:=ifelse(is.na(ng),ng2,ng)]
  print("7. sort 2")
  setorder(z3,ng,ng2,id)
  ID<-z3$id
  d<-function(x,y){length(intersect(x,y))/length(unique(c(x,y)))}
  print("8. similarities")
  b<-sapply(seq(length(ID)-1),function(t){
    d(x[[ID[t]]],x[[ID[t+1]]])
  })
  print("9. last claculus")
  a<-which(b>seuil)
  b<-ID[a+1]
  a<-ID[a]

  M<-sparseMatrix(i=c(a,b),j=c(b,a),x=1,dims = rep(length(txt),2))
  g<-graph_from_adjacency_matrix(M,"undirected")
  cg<-components(g)
  id0<-cg$membership
  w<-cg$csize


  id0<-id0[fastmatch::fmatch(id2,id)]
  w<-(data.frame(id=id0)%>%group_by(id)%>%summarise(n=n())%>%arrange(id))$n
  return(list(id=id0,weight=w))


}
