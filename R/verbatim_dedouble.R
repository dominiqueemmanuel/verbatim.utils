# data <- readxl::read_excel("c:/Users/Dominique/Desktop/Documents présentation TNS 28 06 2016/data/tbl_master.xlsx")
# library(dplyr)
# library(verbatim.utils)
# # txt<-c(data$`Full Text`%>%rep(1),paste0("Je me demande si en fait il faut faire ",data$`Full Text`%>%rep(1)))
# txt<-data$`Full Text`
# length(txt)

#' @export verbatim_dedouble
verbatim_dedouble<-function(txt,exact=FALSE,mc.cores = 4L, n_minhashes= 100L, bands = 5L , threshold = 1-cos(pi/3) ,progress=FALSE){
  library(textreuse)
  library(dplyr)
  library(stringr)
  library(Matrix)
  library(igraph)
  names(txt)<-NULL
  np<-options("mc.cores")$mc.cores
  on.exit(options("mc.cores" = np))
  options("mc.cores" = mc.cores)


  minhash <- minhash_generator(n = n_minhashes, seed = 3552)
  e20<-suppressWarnings(TextReuseCorpus(text=txt, tokenizer = tokenize_ngrams, n = 4,
                                       minhash_func = minhash, keep_tokens = TRUE,
                                       progress = progress))
  a0<-skipped(e20)%>%expand.grid(a=.,b=.)%>%subset(a!=b)

  e2<-tryCatch({
    buckets <- lsh(e20, bands = bands, progress = progress)
    candidates <- lsh_candidates(buckets)
    e2<-lsh_compare(candidates, e20, jaccard_similarity, progress = progress)
    e2<-as.data.frame(e2)



  a<-substr(e2$a,5,1000)%>%as.numeric
  b<-substr(e2$b,5,1000)%>%as.numeric
   # e2$ta<-txt[a]
   # e2$tb<-txt[b]
   # d<-stringdist::stringdist(e2$ta,e2$tb,method="cosine",nthread= mc.cores)
   a2<-paste0("doc-",a)
   b2<-paste0("doc-",b)
  d2<-unlist(parallel::mclapply(seq_along(a),function(t)jaccard_dissimilarity(e20[[a2[t]]]$tokens,e20[[b2[t]]]$tokens),mc.cores = if(.Platform[[1]]=="windows") 1 else mc.cores))
  e2$d<-d2
  e2<-subset(e2,d<=if(exact) 1e-6 else threshold)
  a<-substr(e2$a,5,1000)%>%as.numeric
  b<-substr(e2$b,5,1000)%>%as.numeric
  },error=function(err){print(err);NULL})
  a0$a<-a0$a%>%as.character%>%substr(5,1000)%>%as.numeric
  a0$b<-a0$b%>%as.character%>%substr(5,1000)%>%as.numeric
   d<-stringdist::stringdist(txt[a0$a],txt[a0$b],method="cosine",nthread= mc.cores)
  a0$d<-d
   a0<-a0[d<=if(exact) 1e-6 else 0.05,]
   e2<-rbind(e2,a0)

   a<-e2$a
   b<-e2$b
  M<-sparseMatrix(i=c(a,b),j=c(b,a),x=1,dims = rep(length(txt),2))
  g<-graph_from_adjacency_matrix(M,"undirected")
  cg<-components(g)
  return(list(id=cg$membership,weight=cg$csize))
}
