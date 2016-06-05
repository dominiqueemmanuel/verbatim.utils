data<-readRDS("E:/Travail - Freelance/Givaudan/Global Benchmarking Men/DATAR")
txt <- data$`Impressions, images (Q2)`
nb_topic <- 20
lang <- "en"

txt <- force_encoding(txt)

##  appliquer listing suppression/remplacement (avec ignore.case)

txt <- verbatim_exclusion(txt,min_word = 0, treatment = "empty")

txt <- idiomatic_correction(tolower(txt), lang = lang)

txt <- word_exclusion(txt,min_letter=1)

txt<-gsub("_CARACTÈRE_SPÉCIAL_"," ",txt,fixed=TRUE)

txt<-gsub("[[:space:]]+"," ",txt)


# txt<-sapply(tolower(txt),function(t)tm::stemDocument(str_split(t," ")[[1]])%>%paste(collapse=" "))
library(text2vec)
library(dplyr)

txtd<-str_split(txt,"_PONCTUATION_")
a<-lapply(seq_along(txtd),function(t)rep(t,length(txtd[[t]])))
txtd<-data.frame(id=unlist(a),txt=str_trim(unlist(txtd)),stringsAsFactors  =FALSE)

stem_tokenizer <- function(x) {
  word_tokenizer(x)%>% lapply(SnowballC::wordStem,language=lang)
   }

it <- itoken(txtd$txt, preprocess_function = identity,tokenizer = stem_tokenizer)
vocab <- create_vocabulary(it,
                           ngram = c(ngram_min = 1L, ngram_max = 1L)
                           ,stopwords = tm::stopwords(lang)
) %>% prune_vocabulary(term_count_min = 5,term_count_max = 0.4*length(txt))
vocab$vocab$terms<-force_encoding(vocab$vocab$terms)

q<-str_split(txtd$txt," ")
a<-lapply(seq_along(q),function(t)rep(t,length(q[[t]])))
q<-data.frame(id=unlist(a),terms=unlist(q),stringsAsFactors  =FALSE)
q<-inner_join(q,vocab$vocab%>%select(terms),by="terms")
q<-q%>%group_by(id)%>%summarise(txt=paste(terms,collapse=" "),n=length(terms))
skip_grams_window <- max(3,min(8,quantile(q$n,0.5)))

vocab_v <- vocab_vectorizer(vocab, grow_dtm = FALSE, skip_grams_window = skip_grams_window)

it <- itoken(txtd$txt, preprocess_function = identity,tokenizer = stem_tokenizer)
tcm <- create_tcm(it, vocab_v)

it <- itoken(txtd$txt, preprocess_function = identity,tokenizer = stem_tokenizer)
dtm <- create_dtm(it, vocab_v)

# RcppParallel::setThreadOptions(numThreads = 8)
set.seed(123)
fit <- glove(tcm = tcm, shuffle_seed = 1, word_vectors_size = min(ceiling(nrow(tcm)/2),max(2*nb_topic,50)),
             x_max = 10, learning_rate = 0.05,
             num_iters = 150, grain_size = 1e5,
             max_cost = 50, convergence_threshold = 0.005)
word_vectors <- fit$word_vectors[[1]] + fit$word_vectors[[2]]
rownames(word_vectors) <- force_encoding(rownames(tcm))

d0<-proxy::dist(word_vectors,method="cosine")%>%as.matrix
d0<-d0[,]%>%as.matrix



M2<-transform_tfidf(dtm)
library(Matrix)
e<-rowSums(dtm>0)
e<-ifelse(e==0,1,e)
M2<-M2/e
M2<-M2%*%word_vectors
M2<-as.matrix(M2)
id<-which(rowSums(M2^2)>1e-6)
library(skmeans)
set.seed(123)
mincomponent<-min(floor(c(length(id)/nb_topic/20,length(id)/1000)))
m<-skmeans(x=M2[id,],k = min(nb_topic,max(floor(nrow(word_vectors)/2),2)),method="CLUTO",control=list(verbose=TRUE,maxiter=50,vcluster="E:\\software\\cluto-2.1.1\\Win32\\vcluster.exe",colmodel="none",control=paste0("-niter=100 -ntrials=20 -seed=123 -mincomponent=",mincomponent)))


library(arules)
library(arulesViz)

topic_matrix <- sapply(sort(unique(m$cluster)),function(tt)1*(m$cluster==tt))%>%as("Matrix")
colnames(topic_matrix) <- paste0("cluster___",seq(ncol(topic_matrix)))
# dtm <- dtm[id,]
rule <- transform_topic_to_rule(dtm = dtm[id,],topic_matrix = topic_matrix)


txtd
qq2<-data.frame(id=txtd$id[id],qq)
qq2<-rbind(qq2,data.frame(id=txtd$id[-id],qq[rep(1,length(txtd$id[-id])),,drop=FALSE]*0))
qq2<-melt(data=qq2,"id",variable.name = "cluster")%>%
  mutate(cluster=as.numeric(substr(cluster,2,100)))%>%
  dcast(id~cluster,fun.aggregate=sum,value.var="value")%>%arrange(id)
dim(qq2)
qq2<-data.frame(txt=txt,qq2,stringsAsFactors = FALSE)
View(qq2)


transform_topic_to_rule <- function(dtm, topic_matrix){
  if(is.null(colnames(topic_matrix))) colnames(topic_matrix) <- paste0("cluster___",seq(ncol(topic_matrix)))
  nb_topic <- ncol(topic_matrix)

  B<-as(cbind(topic_matrix,dtm>0),"nMatrix")
  colnames(B)<-force_encoding(colnames(B))
  base<-new("itemMatrix",
            data = t(B)
            ,itemInfo=data.frame(labels=colnames(B),variables=colnames(B),levels=colnames(B))
            ,itemsetInfo=data.frame()
  )
  # colnames(base)
  set.seed(123)
  rules <- apriori(base,parameter = list(support = max(0.0001,3/nrow(B)), confidence = min(10/nb_topic,1/4),minlen=2,maxlen=4,smax=0.4))
  rules
  rules2<-subset(rules,lift > 2)
  rules2
  rules2@lhs@itemInfo[,1]<-as.character(rules2@lhs@itemInfo[,1])%>%force_encoding
  rules2@rhs@itemInfo[,1]<-as.character(rules2@rhs@itemInfo[,1])%>%force_encoding
  q<-lapply(seq(ncol(topic_matrix)),function(tt){cat(".")
    s<-paste0("^",colnames(topic_matrix)[tt],"$")

    id<-which(
      colSums(rules2@lhs@data[which(grepl(s,rules2@lhs@itemInfo[,1],ignore.case=TRUE)),,drop=FALSE])==0
      & colSums(rules2@rhs@data[which(grepl(s,rules2@rhs@itemInfo[,1],ignore.case=TRUE)),,drop=FALSE])>0
    )
    id<-id[order(rules2@quality[id,3],decreasing=TRUE)]
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
      redundant <- which( colSums(subset.matrix, na.rm=TRUE)>0)
      #  subset.matrix[lower.tri(subset.matrix, diag=TRUE)] <- NA
      #  redundant <- colSums(subset.matrix, na.rm=TRUE) > 0
      # table(redundant)
      if(length(redundant)>0){
        # remove redundant rules
        rules4 <- rules4[-redundant]
      }},error=function(e)NULL)

    e<-rules4@quality
    n<-nrow(B)
    n_a_b<-e$support*nrow(B)
    n_a<-n_a_b/e$confidence
    n_b<-n_a_b/(e$lift*n_a)*n
    x<-sapply(seq_along(e[,1]),function(i){
      x<-prop.test(c(n_a_b[i]/1,n_b[i]),c(n_a[i],n),alternative="greater")
      x$p.value
    })
    rules4@quality$p.value<-x
    id<-rules4@quality$p.value<=0.05
    rules4<-rules4[id]
    rules4 <- rules4[order(rules4@quality$support,decreasing = TRUE)]
  })
  id<-which(sapply(q,length)>0)
  q<-q[id]

  qq<-sapply(q,function(qq){cat(".")
    qq<-sapply(seq(ncol(qq@lhs@data)),function(t)qq@lhs@itemInfo[which(qq@lhs@data[,t]>0),1])
    qq<-sapply(qq,function(t){;rowSums(B[,t,drop=FALSE]>0)==length(t) })
    1*(rowSums(matrix(qq,nrow=nrow(B)))>0)
  })
  colnames(qq)<-colnames(topic_matrix)[which(sapply(q,length)>0)]

  r<-lapply(seq_along(q),function(i){
    r<-lapply(seq(ncol(q[[i]]@lhs@data)),function(tt){
      e<-colnames(B)[which(q[[i]]@lhs@data[,tt]>0)]
      data.frame(topic=i,rule=tt,terms=e,stringsAsFactors = FALSE)
    })%>%do.call(rbind,.)
    r
  })%>%do.call(rbind,.)
  return(list(rule=r,new_topic_matrix=qq))
}
