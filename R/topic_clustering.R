# txt <- force_encoding(txt)
# ##  appliquer listing suppression/remplacement (avec ignore.case)
# txt <- verbatim_exclusion(txt,min_word = 0, treatment = "empty")
# txt <- idiomatic_correction(tolower(txt), lang = lang)
# txt <- word_exclusion(txt,min_letter=1)
# # txt <- spell_correction(txt, lang=lang)
# txt<-gsub("_CARACTÈRE_SPÉCIAL_"," ",txt,fixed=TRUE)
# txt<-gsub("_CARACTÈRE_SPÉCIAL_"%>%tolower," ",txt,fixed=TRUE)
# txt<-gsub("[[:space:]]+"," ",txt)

#' @export topic_clustering
topic_clustering <- function(txt=NULL,lang="en",nb_topic = 20,sep_phrase="_PONCTUATION_"){
library(text2vec)
library(dplyr)
library(Matrix)
library(skmeans)
library(arules)
library(arulesViz)
library(data.table)
  library(dplyr)

txtd<-str_split(txt,sep_phrase)
a<-lapply(seq_along(txtd),function(t)rep(t,length(txtd[[t]])))
txtd<-data.frame(id=unlist(a),txt=str_trim(unlist(txtd)),stringsAsFactors  =FALSE)

stem_tokenizer <- function(x) {
  word_tokenizer(x)%>% lapply(SnowballC::wordStem,language=lang)
   }

it <- itoken(txtd$txt, preprocess_function = identity,tokenizer = stem_tokenizer)
vocab <- create_vocabulary(it,
                           ngram = c(ngram_min = 1L, ngram_max = 1L)
                           ,stopwords = tm::stopwords(lang)
) %>% prune_vocabulary(term_count_min = 5,term_count_max = 0.4*length(txt),max_number_of_terms=8000)
vocab$vocab$terms<-force_encoding(vocab$vocab$terms)


q<-str_split(txtd$txt," ")
a<-lapply(seq_along(q),function(t)rep(t,length(q[[t]])))
q<-data.frame(id=unlist(a),terms=unlist(q),stringsAsFactors  =FALSE)
q<-inner_join(q,vocab$vocab%>%select(terms),by="terms")
q<-q%>%group_by(id)%>%summarise(txt=paste(terms,collapse=" "),n=length(terms))
skip_grams_window <- max(3,min(6,quantile(q$n,0.5)))

vocab_v <- vocab_vectorizer(vocab, grow_dtm = FALSE, skip_grams_window = skip_grams_window)

it <- itoken(txtd$txt, preprocess_function = identity,tokenizer = stem_tokenizer)
tcm <- create_tcm(it, vocab_v)

it <- itoken(txtd$txt, preprocess_function = identity,tokenizer = stem_tokenizer)
dtm <- create_dtm(it, vocab_v)

# RcppParallel::setThreadOptions(numThreads = 8)
set.seed(123)
fit <- glove(tcm = tcm, shuffle_seed = 1, word_vectors_size = min(ceiling(nrow(tcm)/2),max(2*nb_topic,10)),
             x_max = 10, learning_rate = 0.05,
             num_iters = 150, grain_size = 1e5,
             max_cost = 50, convergence_threshold = 0.005)
word_vectors <- fit$word_vectors[[1]] + fit$word_vectors[[2]]
rownames(word_vectors) <- force_encoding(rownames(tcm))





M2<-transform_tfidf(dtm)
e<-Matrix::rowSums(dtm>0)
e<-ifelse(e==0,1,e)
M2<-M2/e
M2<-M2%*%word_vectors
M2<-as.matrix(M2)
e<-Matrix::rowSums(M2^2)
id<-which(e>1e-6)

set.seed(123)
m<-skmeans(x=M2[id,],k = min(nb_topic,max(floor(nrow(word_vectors)/2),2)),method="pclust",m=1.05,control=list(verbose=FALSE,start = "S",maxiter=75))
topic_matrix<-as(m$membership>0.9,"Matrix")
colnames(topic_matrix) <- paste0("cluster___",seq(ncol(topic_matrix)))
rule <- transform_topic_to_rule(dtm = dtm[id,],topic_matrix = topic_matrix)


word_distance_function <- word_distance(word_vectors)

# new_topic_matrix <-
# id,]%>%select(id)
txtd <- data.frame(id=txtd$id)
for(k in seq(ncol(rule$new_topic_matrix))){
  txtd[[colnames(rule$new_topic_matrix)[k]]]<-0
  txtd[[colnames(rule$new_topic_matrix)[k]]][id]<-rule$new_topic_matrix[,k]
}
topic_matrix<-melt(txtd,id.vars="id")
head(topic_matrix)
topic_matrix<-topic_matrix%>%group_by(id,variable)%>%summarise(value=max(value))
topic_matrix<-dcast(topic_matrix,id~variable,fun.aggregate=sum,value.var="value")
topic_matrix<-topic_matrix[order(topic_matrix$id),]%>%select(-id)

return(list(word_distance_function=word_distance_function,rule_table = rule$rule,topic_matrix=topic_matrix,vocab=vocab,word_vectors=word_vectors,txtd=txtd,dtm = dtm))
}

#' @export topic_clustering_divide
topic_clustering_divide <- function(object,id_topic,n=2){
  library(text2vec)
  library(dplyr)
  library(Matrix)
  library(skmeans)
  library(arules)
  library(arulesViz)
  library(data.table)
  library(dplyr)

  id1<-which(object$txtd[,1+id_topic]>0)
    set.seed(123)
    X<-object$dtm[id1,]
    X<-as(X,"dgTMatrix")
    X<-slam::simple_triplet_matrix(i=X@i+1,j=X@j+1,v=X@x,nrow = nrow(X),ncol=ncol(X))
    m<-skmeans(x=X,k = n,method="pclust",m=1.25,control=list(verbose=FALSE,start = "S",maxiter=75))
    topic_matrix<-as(m$membership>0.5,"Matrix")

    txtd<-object$txtd
    txtd<-txtd[,c(1,1+sapply(seq(ncol(txtd)-1),function(t)if(t==id_topic) rep(t,n) else t)%>%unlist)]
    txtd[,1+seq(id_topic,id_topic+n-1)]<-0
    txtd[id1,1+seq(id_topic,id_topic+n-1)] <- topic_matrix

      colnames(txtd)[-1] <- paste0("cluster___",seq(ncol(txtd)-1))


      rule <- transform_topic_to_rule(dtm = object$dtm,topic_matrix = txtd[,1+seq(id_topic,id_topic+n-1)]%>%as.matrix%>%as("Matrix"),ignore_rule_table = object$rule_table%>%subset(!(topic %in% id_topic)))

      rule$rule$topic<-rule$rule$topic+id_topic-1

  rule_table <- object$rule_table%>%subset(topic!=id_topic)
  rule_table$topic[rule_table$topic>id_topic]<-rule_table$topic[rule_table$topic>id_topic]+n-1
  rule_table <- rbind(rule_table, rule$rule)%>%arrange(topic,rule)



  topic_matrix <- txtd
  topic_matrix<-melt(topic_matrix,id.vars="id")
  head(topic_matrix)
  topic_matrix<-topic_matrix%>%group_by(id,variable)%>%summarise(value=max(value))
  topic_matrix<-dcast(topic_matrix,id~variable,fun.aggregate=sum,value.var="value")
  topic_matrix<-topic_matrix[order(topic_matrix$id),]%>%select(-id)

  return(list(word_distance_function=object$word_distance_function
              ,rule_table = rule_table
              ,topic_matrix=topic_matrix
              ,vocab=object$vocab
              ,word_vectors=object$word_vectors
              ,txtd=txtd
              ,dtm = object$dtm))

}
transform_topic_to_rule <- function(dtm, topic_matrix,ignore_rule_table=NULL){

  # if(is.null(colnames(topic_matrix)))
    colnames(topic_matrix) <- paste0("cluster___",seq(ncol(topic_matrix)))
  nb_topic <- ncol(topic_matrix)

  B<-as(cbind(topic_matrix,dtm>0),"nMatrix")
  colnames(B)<-force_encoding(colnames(B))
  base<-new("itemMatrix",
            data = Matrix::t(B)
            ,itemInfo=data.frame(labels=colnames(B),variables=colnames(B),levels=colnames(B))
            ,itemsetInfo=data.frame()
  )
  # colnames(base)
  set.seed(123)
  confidence <- min(5/nb_topic,1/2)
  support = max(0.0001,3/nrow(dtm))
  rules <- apriori(base,parameter = list(support = support, confidence = confidence,minlen=2,maxlen=4,smax=0.4))
  rules
  rules2<-arules::subset(rules,lift > 2)
  rules2
  rules2@lhs@itemInfo[,1]<-as.character(rules2@lhs@itemInfo[,1])%>%force_encoding
  rules2@rhs@itemInfo[,1]<-as.character(rules2@rhs@itemInfo[,1])%>%force_encoding
  q<-lapply(seq(ncol(topic_matrix)),function(tt){cat(".")
    s<-paste0("^",colnames(topic_matrix)[tt],"$")
if(is.null(ignore_rule_table)){
    id<-which(
      Matrix::colSums(rules2@lhs@data[which(grepl(s,rules2@lhs@itemInfo[,1],ignore.case=TRUE)),,drop=FALSE])==0
      & Matrix::colSums(rules2@rhs@data[which(grepl(s,rules2@rhs@itemInfo[,1],ignore.case=TRUE)),,drop=FALSE])>0
    )
} else {
  s2<-ignore_rule_table%>%arrange(topic,rule,terms)%>%group_by(topic,rule)%>%summarise(s2=paste0("(",paste0(paste0("(?=.*",terms,")"),collapse=""),")"))
  s2<-s2[!duplicated(s2$s2),]
  s2<-paste0(s2$s2,collapse="|")
  id<-which(
    Matrix::colSums(rules2@lhs@data[which(grepl(s,rules2@lhs@itemInfo[,1],ignore.case=TRUE)),,drop=FALSE])==0
    & Matrix::colSums(rules2@rhs@data[which(grepl(s,rules2@rhs@itemInfo[,1],ignore.case=TRUE)),,drop=FALSE])>0
    & Matrix::colSums(rules2@lhs@data[which(grepl(s2,rules2@lhs@itemInfo[,1],perl=TRUE)),,drop=FALSE])==0
  )
}
    length(id)
    id<-id[order(rules2@quality[id,]$support,decreasing=TRUE)]
    id<-id[seq_along(id)<=100]
    rules3<-rules2[id]
    rules4<-rules3




    tryCatch({
      # rules4@quality$lift<-1
      # rules4@quality$support<-1
      # rules4@quality$confidence<-1
      subset.matrix <- is.subset(rules4, rules4)
      # subset.matrix <- subset.matrix[c("{cluster___1,natur,air}","{cluster___1,natur}"),c("{cluster___1,natur,air}","{cluster___1,natur}"   )]
      # subset.matrix
      diag(subset.matrix)<-NA
      redundant <- which( Matrix::colSums(subset.matrix, na.rm=TRUE)>0)
      #  subset.matrix[lower.tri(subset.matrix, diag=TRUE)] <- NA
      #  redundant <- colSums(subset.matrix, na.rm=TRUE) > 0
      # table(redundant)
      if(length(redundant)>0){
        # remove redundant rules
        rules4 <- rules4[-redundant]
      }},error=function(e)NULL)

    e<-rules4@quality
    n<-nrow(dtm)
    n_a_b<-e$support*nrow(dtm)
    n_a<-n_a_b/e$confidence
    n_b<-n_a_b/(e$lift*n_a)*n
    x<-sapply(seq_along(e[,1]),function(i){
      x<-prop.test(c(n_a_b[i]/1,n_b[i]),c(n_a[i],n),alternative="greater")
      x$p.value
    })
    rules4@quality$p.value<-x
    id<-rules4@quality$p.value<=0.05
    rules4<-rules4[id]
    rules4
    if(length(rules4)>0){
    qq<-Matrix::t(Matrix::t(as(B,"dgCMatrix")%*%rules4@lhs@data)==Matrix::colSums(rules4@lhs@data))
    qq2<-(Matrix::t(qq[topic_matrix[,tt]==1,,drop=FALSE])%*%(1-qq[topic_matrix[,tt]==1,,drop=FALSE]))/
      (Matrix::t(qq)%*%(1-qq))
    qq2@x<-ifelse(is.na(qq2@x),0,qq2@x)
    diag(qq2)<-1
    qq3<-(Matrix::t(qq[topic_matrix[,tt]==1,,drop=FALSE])%*%(1-qq[topic_matrix[,tt]==1,,drop=FALSE]))/nrow(qq)
    qq2@x<-ifelse(is.na(qq2@x),1,qq2@x)
    qq2<-as.matrix((qq2<confidence) | (qq3<support))
    diag(qq2)<-0
    library(igraph)
    id<-seq(ncol(qq2))
    set.seed(1234)
    qq3<-t(qq2)
    id<-which(Matrix::colSums(qq3)==0)
    while(sum(qq3==1)>0){
      t<-which(qq3==1,arr.ind=TRUE)
      m<-which.min(t[,2]-1e-4*t[,1])
       # if(t[m,1]==1)stop("xx")
      if(sum(qq3[-t[m,1],t[m,2]])>0){
        qq3[t[m,1],t[m,2]]<-0
      } else {
        qq3[t[m,1],t[m,2]]<-2
        id<-c(id,t[m,1])
      }

    }
    id<-unique(id)
    # which(rowSums(qq2[-id,id])==0)
    # g<-graph_from_adjacency_matrix(t(qq2),mode="directed")
    # V(g)$color<-ifelse(seq(ncol(qq2)) %in% id,"green","red")
    #
    #   visNetwork::visIgraph(g)%>%visNetwork::visOptions(highlightNearest=TRUE,nodesIdSelection=TRUE)
    #   length(id)

    rules4<-rules4[id]
    }
    rules4
  })
  id<-which(sapply(q,length)>0)
  q<-q[id]

  qq<-sapply(q,function(qq){cat(".")
    qq<-sapply(seq(ncol(qq@lhs@data)),function(t)qq@lhs@itemInfo[which(qq@lhs@data[,t]>0),1])
    qq<-sapply(qq,function(t){Matrix::rowSums(B[,t,drop=FALSE]>0)==length(t) })
    1*(Matrix::rowSums(matrix(qq,nrow=nrow(B)))>0)
  })
  colnames(qq)<-colnames(topic_matrix)[which(sapply(q,length)>0)]

  r<-lapply(seq_along(q),function(i){
    r<-lapply(seq(ncol(q[[i]]@lhs@data)),function(tt){
      e<-colnames(B)[which(q[[i]]@lhs@data[,tt]>0)]
      data.frame(topic=i,rule=tt,terms=e,stringsAsFactors = FALSE)
    })%>%do.call(rbind,.)
    r
  })%>%do.call(rbind,.)
  return(list(rule=r,new_topic_matrix=qq,q=q))
}






#' @export topic_clustering_remove
topic_clustering_remove <- function(object,id_topic){
  library(text2vec)
  library(dplyr)
  library(Matrix)
  library(skmeans)
  library(arules)
  library(arulesViz)
  library(data.table)
  library(dplyr)
  topic_matrix <- object$topic_matrix
  topic_matrix <- topic_matrix[,which(!(seq(ncol(topic_matrix)) %in% id_topic)),drop=FALSE]

  txtd<-object$txtd
  txtd<-txtd[,c(1,1+sapply(seq(ncol(txtd)-1),function(t)if(t %in% id_topic) NULL else t)%>%unlist)]
  colnames(txtd)[-1] <- paste0("cluster___",seq(ncol(txtd)-1))



  rule_table <- object$rule_table%>%subset(!(topic %in% id_topic))
  rule_table$topic<-dplyr::dense_rank(rule_table$topic)
  rule_table <- rule_table%>%arrange(topic,rule)



  return(list(word_distance_function=object$word_distance_function
              ,rule_table = rule_table
              ,topic_matrix=topic_matrix
              ,vocab=object$vocab
              ,word_vectors=object$word_vectors
              ,txtd=txtd
              ,dtm = object$dtm))

}


#' @export topic_clustering_merge
topic_clustering_merge <- function(object,id_topic){
  library(text2vec)
  library(dplyr)
  library(Matrix)
  library(skmeans)
  library(arules)
  library(arulesViz)
  library(data.table)
  library(dplyr)

  topic_matrix<-object$topic_matrix
  id_topic<-sort(id_topic)
  id_topic1<-id_topic[1]
  id_topic2<-id_topic[-1]

  topic_matrix[,id_topic1]<-apply(topic_matrix[,id_topic,drop=FALSE],1,max)
  topic_matrix <- topic_matrix[,-id_topic2]

  txtd<-object$txtd
  txtd<-txtd[,c(1,1+sapply(seq(ncol(txtd)-1),function(t)if(t %in% id_topic2) NULL else t)%>%unlist)]
  colnames(txtd)[-1] <- paste0("cluster___",seq(ncol(txtd)-1))


  x<-object$rule_table%>%subset((topic %in% id_topic))
  x$rule<- dplyr::dense_rank(1000*x$rule+x$topic)
  x<-x%>%mutate(topic=ifelse(topic %in% id_topic2,id_topic1,topic))

  rule_table <- object$rule_table%>%subset(!(topic %in% id_topic))
  rule_table <- rbind(rule_table, x)
  rule_table$topic<-dplyr::dense_rank(rule_table$topic)
  rule_table<-rule_table%>%arrange(topic,rule)



  return(list(word_distance_function=object$word_distance_function
              ,rule_table = rule_table
              ,topic_matrix=topic_matrix
              ,vocab=object$vocab
              ,word_vectors=object$word_vectors
              ,txtd=txtd
              ,dtm = object$dtm))

}
