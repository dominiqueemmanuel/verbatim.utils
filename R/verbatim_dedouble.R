#' @export verbatim_dedouble
verbatim_dedouble<-function(txt,exact=FALSE,mc.cores = 4L){
  library(textreuse)
  library(dplyr)
  library(stringr)
  library(Matrix)
  library(igraph)
  names(txt)<-NULL
  np<-options("mc.cores")$mc.cores
  on.exit(options("mc.cores" = np))
  options("mc.cores" = mc.cores)


  minhash <- minhash_generator(n = 48, seed = 3552)
  e2<-suppressWarnings(TextReuseCorpus(text=txt, tokenizer = tokenize_ngrams, n = 3,
                                       minhash_func = minhash, keep_tokens = TRUE,
                                       progress = FALSE))
  a0<-skipped(e2)%>%expand.grid(a=.,b=.)
  a0$score<-NA
  e2<-tryCatch({
    buckets <- lsh(e2, bands = 8, progress = FALSE)
    candidates <- lsh_candidates(buckets)
    e2<-lsh_compare(candidates, e2, jaccard_similarity, progress = FALSE)
    e2<-as.data.frame(e2)
    e2<-rbind(e2,a0)
  },error=function(err){print(err);a0})
  a<-substr(e2$a,5,1000)%>%as.numeric
  b<-substr(e2$b,5,1000)%>%as.numeric
  e2$ta<-txt[a]
  e2$tb<-txt[b]
  d<-stringdist::stringdist(e2$ta,method="cosine",e2$tb,nthread= mc.cores)
  e2$d<-d
  e2<-subset(e2,d<=if(exact) 1e-6 else 1-cos(pi/5))
  a<-substr(e2$a,5,1000)%>%as.numeric
  b<-substr(e2$b,5,1000)%>%as.numeric
  M<-sparseMatrix(i=c(a,b),j=c(b,a),x=1,dims = rep(length(txt),2))
  g<-graph_from_adjacency_matrix(M,"undirected")
  cg<-components(g)
  return(list(id=cg$membership,weight=cg$csize))
}
