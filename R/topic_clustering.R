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
topic_clustering <- function(txt=NULL,lang="en",nb_topic = 20,sep_phrase="_PONCTUATION_",term_count_min = 5 , m=1.05, threshold = 0.9,word_vectors_size=50 , is_stem = FALSE){
  library(text2vec)
  library(dplyr)
  library(Matrix)
  library(skmeans)
  library(arules)
  library(arulesViz)
  library(data.table)
  library(dplyr)
  library(stringi)
  library(stringr)
    # save(file="do2",list=ls())
   # stop("xx")
   # load("C:/Users/Dominique/Desktop/Stat_Regie/data/application_data/do2")
txt<-stri_replace_all_fixed(txt,"'"," ")
  txtd<-stri_split_regex(txt,sep_phrase,case_insensitive=TRUE)
  a<-lapply(seq_along(txtd),function(t)rep(t,length(txtd[[t]])))
  txtd<-data.frame(id=unlist(a),txt=str_trim(unlist(txtd)),stringsAsFactors  =FALSE)

  if(is_stem){
  stem_tokenizer <- function(x) {
    word_tokenizer(x)%>% lapply(SnowballC::wordStem,language=lang)
  }
  } else {
    stem_tokenizer <- word_tokenizer
  }
  it <- itoken(txtd$txt, preprocess_function = identity,tokenizer = stem_tokenizer)
  vocab <- create_vocabulary(it,
                             ngram = c(ngram_min = 1L, ngram_max = 1L)
                             # ,stopwords = tm::stopwords(lang)
  ) %>% prune_vocabulary(term_count_min = term_count_min,term_count_max = 0.4*length(txt),max_number_of_terms=8000)
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
  colnames(dtm) <- force_encoding(colnames(dtm))
  txtp<-txtd$txt
  # RcppParallel::setThreadOptions(numThreads = 8)
  set.seed(123)
  word_vectors <- glove(tcm = tcm, shuffle_seed = 1, word_vectors_size = min(ceiling(nrow(tcm)/2),word_vectors_size),
               x_max = 10, learning_rate = 0.05,
               num_iters = 150, grain_size = 1e5,
               max_cost = 50, convergence_threshold = 0.005)
  rownames(word_vectors) <- force_encoding(rownames(tcm))





  # M2<-transform_tfidf(dtm)
  # e<-Matrix::rowSums(dtm>0)
  # e<-ifelse(e==0,1,e)
  # M2<-M2/e
  # M2<-M2%*%word_vectors
  # M2<-as.matrix(M2)
  # e<-Matrix::rowSums(M2^2)
  # id<-which(e>1e-6)

  # set.seed(123)
  # m<-skmeans(x=M2[id,],k = min(nb_topic,max(floor(nrow(word_vectors)/2),2)),method="pclust",m=m,control=list(verbose=FALSE,start = "S",maxiter=75))
  # X<-RSpectra::svds(M2[id,], k = ceiling(word_vectors_size*0.8), nv = 0, nu = ceiling(word_vectors_size*0.8))
  # X$u%>%dim
  # set.seed(123)
  # m<-skmeans(x=cBind(M2[id,],X$u),k = min(nb_topic,max(floor(nrow(word_vectors)/2),2)),method="pclust",m=m,control=list(verbose=TRUE,start = "S",maxiter=75))
set.seed(123)
  m0<-LDA(n_topics = nb_topic,vocabulary = vocab)
  a<-m0$fit_predict(dtm,n_iter = 300)
  topic_matrix<-as(a>threshold,"Matrix")

    # topic_matrix<-as(m$membership>threshold,"Matrix")
  colnames(topic_matrix) <- paste0("cluster___",seq(ncol(topic_matrix)))
  save(file="do2",list=ls())
  id<-seq(nrow(dtm))
  rule <- transform_topic_to_rule(dtm = dtm,topic_matrix = topic_matrix,word_vectors = word_vectors)


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

  return(list(word_distance_function=word_distance_function
              ,rule_table = rule$rule
              ,topic_matrix=topic_matrix
              ,vocab=vocab
              ,word_vectors=word_vectors
              ,txtd=txtd
              ,dtm = dtm
              ,txtp=txtp))
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



  rule <- transform_topic_to_rule(dtm = object$dtm,topic_matrix = txtd[,1+seq(id_topic,id_topic+n-1)]%>%as.matrix%>%as("Matrix"),ignore_rule_table = object$rule_table%>%subset(!(topic %in% id_topic)))

  for(k in seq(id_topic,id_topic+n-1)){

    txtd[id1,1+k]<-rule$new_topic_matrix[id1,1+k-id_topic]
  }

  colnames(txtd)[-1] <- paste0("cluster___",seq(ncol(txtd)-1))



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
              ,dtm = object$dtm
              ,txtp=object$txtp))

}


transform_topic_to_rule <- function(dtm, topic_matrix,ignore_rule_table=NULL,word_vectors=NULL){
  # save(file="doz",list=ls())
  # stop('xxx')
  # load("C:/Users/Dominique/Desktop/Stat_Regie/data/application_data/doz")
  # if(is.null(colnames(topic_matrix)))
  colnames(topic_matrix) <- paste0("cluster___",seq(ncol(topic_matrix)))
  nb_topic <- ncol(topic_matrix)
 if(!is.null(word_vectors)){
   y<-t(topic_matrix)%*%(dtm)%*%word_vectors
   y<-y%>%as.matrix
 }

  q<-lapply(seq(ncol(topic_matrix)),function(tt){cat(".")

    e<-tryCatch({
      library(glmnet)
    set.seed(123)
    m<-cv.glmnet(x=dtm,y=factor(topic_matrix[,tt]),family="binomial",alpha=0.01, lower.limits=0,pmax=30,maxit=500)
    e<-coef(m,s="lambda.min")
    e<-e[-1,]
    e<-e[which(e>0)]
    names(e[order(e,decreasing = TRUE)])
    },error=function(e)colnames(dtm))
    if(length(e)<=1)e<-colnames(dtm)

    B<-as(cbind(topic_matrix,dtm[,e,drop=FALSE]>0),"nMatrix")
    colnames(B)<-force_encoding(colnames(B))
    base<-new("itemMatrix",
              data = Matrix::t(B)
              ,itemInfo=data.frame(labels=colnames(B),variables=colnames(B),levels=colnames(B))
              ,itemsetInfo=data.frame()
    )
    # colnames(base)
    set.seed(123)
    confidence <- 0.15#min(5/nb_topic,1/4)
    support = max(0.00005,5/nrow(dtm))
    rules <- apriori(base,parameter = list(support = support, confidence = confidence,minlen=2,maxlen=4,smax=0.4,arem="info",minval=0,aval=TRUE))
    rules
    # print(paste0("Length (1) = ",length(rules)))
    rules2<-arules::subset(rules,lift > 2)
    # print(paste0("Length (2) = ",length(rules2)))
    rules2
    rules2@lhs@itemInfo[,1]<-as.character(rules2@lhs@itemInfo[,1])%>%force_encoding
    rules2@rhs@itemInfo[,1]<-as.character(rules2@rhs@itemInfo[,1])%>%force_encoding
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

    if(!is.null(word_vectors) & length(id)>=2){
      r<-rules[id]
      x<-r@lhs@data[-seq(ncol(topic_matrix)),]
      x<-lapply(seq(ncol(x)),function(t)which(x[,t]>0))
      d<-sapply(x,function(t){
        d<-proxy::dist(word_vectors[t,,drop=FALSE],y[tt,,drop=FALSE],method="cosine")
        max(d)
      })

      id<-id[order(d)]
      id<-id[seq_along(id)<=40]
      rules3<-rules2[id]

      id<-order(rules3@quality$info,decreasing=TRUE)
      id<-id[seq_along(id)<=20]
      rules4<-rules3[id]

      id<-order(rules4@quality$support,decreasing=TRUE)
      id<-id[seq_along(id)<=10]
      rules4<-rules4[id]
    } else {
    id<-id[order(rules2@quality[id,]$info,decreasing=TRUE)]
    id<-id[seq_along(id)<=30]
    rules3<-rules2[id]

    id<-order(rules3@quality$support,decreasing=TRUE)
    id<-id[seq_along(id)<=15]
    rules4<-rules3[id]
    }
    # rules4<-rules3
    # print(paste0("Length (3 -",tt,") = ",length(rules4)))

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
    # print(paste0("Length (4 -",tt,") = ",length(rules4)))
    rules4

    if(FALSE){
    if(length(rules4)>0){

      qq<-Matrix::t(Matrix::t(as(B,"dgCMatrix")%*%rules4@lhs@data)==Matrix::colSums(rules4@lhs@data))
      qq2<-(Matrix::t(qq[topic_matrix[,tt]==1,,drop=FALSE])%*%(1-qq[topic_matrix[,tt]==1,,drop=FALSE]))/
        (Matrix::t(qq)%*%(1-qq))
      qq2@x<-ifelse(is.na(qq2@x),0,qq2@x)

      diag(qq2)<-1
      qq3<-(Matrix::t(qq[topic_matrix[,tt]==1,,drop=FALSE])%*%(1-qq[topic_matrix[,tt]==1,,drop=FALSE]))/nrow(qq)
      qq2@x<-ifelse(is.na(qq2@x),1,qq2@x)
      qq2<-as.matrix((qq2<confidence) | (qq3<support))
      # qq2<-as.matrix((qq2<0.75*confidence))
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
      # print(paste0("Length (5 -",tt,") = ",length(rules4)))
    }
#
#     print(" ")
#     print(" ")
}
   list(r=rules4,B=B)
  })

  id<-which(sapply(q,function(t)length(t$r))>0)
  q<-q[id]

  qq<-sapply(q,function(qq){cat(".")
    qq0<-sapply(seq(ncol(qq$r@lhs@data)),function(t)qq$r@lhs@itemInfo[which(qq$r@lhs@data[,t]>0),1])
    qq0<-sapply(qq0,function(t){Matrix::rowSums(qq$B[,t,drop=FALSE]>0)==length(t) })
    1*(Matrix::rowSums(matrix(qq0,nrow=nrow(qq$B)))>0)
  })
  colnames(qq)<-colnames(topic_matrix)[which(sapply(q,length)>0)]

  r<-lapply(seq_along(q),function(i){
    r<-lapply(seq(ncol(q[[i]]$r@lhs@data)),function(tt){
      e<-colnames(q[[i]]$B)[which(q[[i]]$r@lhs@data[,tt]>0)]
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
              ,dtm = object$dtm
              ,txtp=object$txtp))

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
              ,dtm = object$dtm
              ,txtp=object$txtp))

}



transform_rule_to_topic <- function(dtm,rule_table){
  topic <- sort(unique(rule_table$topic))

  qq<-sapply(topic,function(qq){cat(".")
    q<-subset(rule_table,topic==qq)
    qq<-sapply(sort(unique(q$rule)),function(r){
      t<-subset(q,rule==r)$terms
      rowSums(dtm[,t,drop=FALSE]>0)==length(t)
    })
    1*(rowSums(qq)>0)
  })

  return(qq)


}



#' @export topic_clustering_modify
topic_clustering_modify <- function(object,rule_table){
  library(text2vec)
  library(dplyr)
  library(Matrix)
  library(skmeans)
  library(arules)
  library(arulesViz)
  library(data.table)
  library(dplyr)

  id_topic <- sort(unique(rule_table$topic))

  txtd<-object$txtd
  topic_matrix <- transform_rule_to_topic(object$dtm,rule_table)
  txtd[,1+id_topic] <- topic_matrix





  new_rule_table <- object$rule_table%>%subset(!c(topic %in% id_topic))
  new_rule_table <- rbind(new_rule_table, rule_table)%>%arrange(topic,rule)



  topic_matrix <- txtd
  topic_matrix<-melt(topic_matrix,id.vars="id")
  head(topic_matrix)
  topic_matrix<-topic_matrix%>%group_by(id,variable)%>%summarise(value=max(value))
  topic_matrix<-dcast(topic_matrix,id~variable,fun.aggregate=sum,value.var="value")
  topic_matrix<-topic_matrix[order(topic_matrix$id),]%>%select(-id)

  colnames(txtd)[-1] <- paste0("cluster___",seq(ncol(txtd)-1))
  return(list(word_distance_function=object$word_distance_function
              ,rule_table = new_rule_table
              ,topic_matrix=topic_matrix
              ,vocab=object$vocab
              ,word_vectors=object$word_vectors
              ,txtd=txtd
              ,dtm = object$dtm
              ,txtp=object$txtp))

}


#' @export topic_clustering_add
topic_clustering_add <- function(object,rule_table){
  library(text2vec)
  library(dplyr)
  library(Matrix)
  library(skmeans)
  library(arules)
  library(arulesViz)
  library(data.table)
  library(dplyr)

  id_topic <- max(object$rule_table$topic)+1
  rule_table$topic<-id_topic

  txtd<-object$txtd
  topic_matrix <- transform_rule_to_topic(object$dtm,rule_table)
  txtd <- cbind(txtd,topic_matrix)





  new_rule_table <- object$rule_table
  new_rule_table <- rbind(new_rule_table, rule_table)%>%arrange(topic,rule)



  topic_matrix <- txtd
  topic_matrix<-melt(topic_matrix,id.vars="id")
  head(topic_matrix)
  topic_matrix<-topic_matrix%>%group_by(id,variable)%>%summarise(value=max(value))
  topic_matrix<-dcast(topic_matrix,id~variable,fun.aggregate=sum,value.var="value")
  topic_matrix<-topic_matrix[order(topic_matrix$id),]%>%select(-id)
  colnames(txtd)[-1] <- paste0("cluster___",seq(ncol(txtd)-1))
  return(list(word_distance_function=object$word_distance_function
              ,rule_table = new_rule_table
              ,topic_matrix=topic_matrix
              ,vocab=object$vocab
              ,word_vectors=object$word_vectors
              ,txtd=txtd
              ,dtm = object$dtm
              ,txtp=object$txtp))

}
