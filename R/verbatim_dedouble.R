# library(dplyr)
# library(readxl)
# library(verbatim.utils)
# f1<-read_excel("e:/Travail - Freelance/TNS/brexit/StreamFrenchQueryBrexit1.xlsx")
# dim(f1)
# f2<-read_excel("e:/Travail - Freelance/TNS/brexit/StreamFrenchQuery2.xlsx")
# dim(f2)
# f<-rbind(f1,f2)
# f$`Sound Bite Text`[1]
# txt<-f$`Sound Bite Text`
# txt=txt[10001:20000]
# data <- readxl::read_excel("c:/Users/Dominique/Desktop/Documents présentation TNS 28 06 2016/data/tbl_master.xlsx")
# txt<-data$`Full Text`
# library(dplyr)
# library(verbatim.utils)
# # txt<-c(data$`Full Text`%>%rep(1),paste0("Je me demande si en fait il faut faire ",data$`Full Text`%>%rep(1)))
# txt<-data$`Full Text`
# length(txt)
# data<-read.csv("c:/Users/Dominique/Downloads/Sentiment(1).csv",sep=",")
# txt0<-txt<-data$text%>%as.character
#' @export verbatim_dedouble
#'
#'
#'
verbatim_dedouble<-function(txt,exact=FALSE,mc.cores = 4L, n_minhashes= 30L, bands = 5L , threshold = 1-cos(pi/3) ,progress=FALSE, use_duplicated = TRUE){
   # exact=FALSE;mc.cores = 4L; n_minhashes= 30L; bands = 5L ; threshold = 1-cos(pi/3) ;progress=TRUE; use_duplicated = TRUE
    library(textreuse)
  library(dplyr)
  library(stringr)
  library(Matrix)
  library(igraph)
  library(data.table)
  names(txt)<-NULL
  if(use_duplicated){
  txt<-data.table(txt=txt,id=seq_along(txt),key="txt")
  txt2<-unique(txt)
  setnames(txt2,"id","id2")
  txt2<-txt[txt2]
  txt2<-txt2[order(txt2$id),]
  id2<-txt2$id2
  id<-which(!duplicated(txt2$id2))
  txt<-txt2$txt[id]
  }
  # duplicated(txt)%>%table

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
    e2<-subset(e2,score>=lsh_threshold(n_minhashes,bands)/2)


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
    e2$a<-substr(e2$a,5,1000)%>%as.numeric
    e2$b<-substr(e2$b,5,1000)%>%as.numeric
    e2
  },error=function(err){print(err);NULL})
  a0$a<-a0$a%>%as.character%>%substr(5,1000)%>%as.numeric
  a0$b<-a0$b%>%as.character%>%substr(5,1000)%>%as.numeric
  d<-stringdist::stringdist(txt[a0$a],txt[a0$b],method="cosine",nthread= mc.cores)
  a0$score<-0
  a0$d<-d
  a0<-a0[a0$d<=(if(exact) 1e-6 else 0.05),]
  e2<-rbind(e2,a0)

  a<-e2$a
  b<-e2$b
  M<-sparseMatrix(i=c(a,b),j=c(b,a),x=1,dims = rep(length(txt),2))
  g<-graph_from_adjacency_matrix(M,"undirected")
  cg<-components(g)
  id0<-cg$membership
  w<-cg$csize

if(use_duplicated){
    id0<-id0[fastmatch::fmatch(id2,id)]
    w<-(data.frame(id=id0)%>%group_by(id)%>%summarise(n=n())%>%arrange(id))$n
   return(list(id=id0,weight=w))
} else {

  w<-(data.frame(id=id0)%>%group_by(id)%>%summarise(n=n())%>%arrange(id))$n
  return(list(id=id0,weight=w))
}
}
# verbatim_dedouble<-function(txt,exact=FALSE,mc.cores = 4L, n_minhashes= 200L, bands = 50L , threshold = 1-cos(pi/3) ,progress=FALSE){
#   library(text2vec)
#   library(LSHR)
#   library(dplyr)
#   library(stringr)
#   library(Matrix)
#   library(igraph)
#   names(txt)<-NULL
#   txt<-tolower(txt)
#   np<-options("mc.cores")$mc.cores
#   on.exit(options("mc.cores" = np))
#   if(.Platform[[1]]== "windows") mc.cores<-1
#   options("mc.cores" = mc.cores)
#
#   it <- itoken(txt, preprocess_function = tolower,tokenizer = word_tokenizer)
#   vocab <- create_vocabulary(it,ngram = c(ngram_min = 5L, ngram_max = 5L)) %>%
#     prune_vocabulary(term_count_min = 2,max_number_of_terms=600000)
#   vocab$vocab<-vocab$vocab%>%subset(doc_counts>=2)
#   vocab$vocab<-head(vocab$vocab,500000)
#   vectorizer <- vocab_vectorizer(vocab)
#   it <- itoken(txt, preprocess_function = tolower,tokenizer = word_tokenizer)
#   is_err<- tryCatch({
#     dtm <- create_dtm(it, vectorizer)
#     FALSE
#   },error=function(err)TRUE)
#   if(is_err)return(list(id=seq_along(txt),weight=rep(1,length(txt))))
#
#   gm<- get_hash_matrix(dtm, hashfun_number = n_minhashes, measure = "jaccard",seed=123L)
#   sign_mat <- get_signature_matrix(dtm, hashfun_number = n_minhashes, measure = 'jaccard',seed=123L, mc.cores =  mc.cores)
#   candidate_indices <-get_similar_pairs(sign_mat, bands_number = bands)
#   head(candidate_indices)
#
#   d<-sapply(seq_along(candidate_indices$id1)[1:1000],function(t)jaccard_atomic(dtm[candidate_indices[t,]$id1,],dtm[candidate_indices[t,]$id2,]))
#
#   e2<-tryCatch({
#     buckets <- lsh(e20, bands = bands, progress = progress)
#     candidates <- lsh_candidates(buckets)
#     e2<-lsh_compare(candidates, e20, jaccard_similarity, progress = progress)
#     e2<-as.data.frame(e2)
#
#
#
#     a<-substr(e2$a,5,1000)%>%as.numeric
#     b<-substr(e2$b,5,1000)%>%as.numeric
#     # e2$ta<-txt[a]
#     # e2$tb<-txt[b]
#     # d<-stringdist::stringdist(e2$ta,e2$tb,method="cosine",nthread= mc.cores)
#     a2<-paste0("doc-",a)
#     b2<-paste0("doc-",b)
#     d2<-unlist(parallel::mclapply(seq_along(a),function(t)jaccard_dissimilarity(e20[[a2[t]]]$tokens,e20[[b2[t]]]$tokens),mc.cores = if(.Platform[[1]]=="windows") 1 else mc.cores))
#     e2$d<-d2
#     e2<-subset(e2,d<=if(exact) 1e-6 else threshold)
#     a<-substr(e2$a,5,1000)%>%as.numeric
#     b<-substr(e2$b,5,1000)%>%as.numeric
#   },error=function(err){print(err);NULL})
#   a0$a<-a0$a%>%as.character%>%substr(5,1000)%>%as.numeric
#   a0$b<-a0$b%>%as.character%>%substr(5,1000)%>%as.numeric
#   d<-stringdist::stringdist(txt[a0$a],txt[a0$b],method="cosine",nthread= mc.cores)
#   a0$d<-d
#   a0<-a0[d<=if(exact) 1e-6 else 0.05,]
#   e2<-rbind(e2,a0)
#
#   a<-e2$a
#   b<-e2$b
#   M<-sparseMatrix(i=c(a,b),j=c(b,a),x=1,dims = rep(length(txt),2))
#   g<-graph_from_adjacency_matrix(M,"undirected")
#   cg<-components(g)
#   return(list(id=cg$membership,weight=cg$csize))
# }
