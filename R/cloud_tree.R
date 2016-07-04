#' @export cloud_tree
cloud_tree <-function(dtm,word_vectors=NULL,automatic_color=TRUE,default_color="steelblue",dtm_base=NULL,method="indice",min_tree=3,max_tree=13,min_cloud=3,max_cloud=22){
  library(igraph)
  library(shinythemes)
  library(ggplot2)
  library(RColorBrewer)
  library(Matrix)
  library(smacof)
  library(dplyr)
  library(ggthemes)
  library(ggrepel)
  library(Matrix)
  set.seed(123)

  if(!is.null(dtm_base)){
    # dtm_base<-dtm_base[,e]
    if(method=="indice"){
      a<-1/Matrix::colSums(dtm_base)
      a<-ifelse(is.na(a),1,a)
      dtm<-Matrix::t(Matrix::t(dtm)*a)


    } else {


    a<-log(nrow(dtm_base)/Matrix::colSums(dtm_base>0))
    a<-ifelse(is.na(a),1,a)
    dtm<-Matrix::t(Matrix::t(dtm)*a)
    }
  }
  e<-order(colSums(dtm),decreasing=TRUE)[seq(ncol(dtm))<=100]
  dtm<-dtm[,e]
  ## wordcloud sera appelé via des ::
  if(nrow(dtm)>1 & ncol(dtm)>1){
    dtm0<-cBind(dtm[,1,drop=FALSE],dtm)
    if(is.null(word_vectors)){
    db<-as.matrix(proxy::dist(as.matrix(dtm0>0),by_rows = FALSE,method="Phi"))
    } else {
      db<-as.matrix(proxy::dist(as.matrix(word_vectors),by_rows = TRUE,method="cosine"))
    }
    d<-round(db/max(db,na.rm=TRUE),8)
    d[is.na(d)]<-1
    d<-d[-1,-1,drop=FALSE]
    rownames(d)<-colnames(d)<-gsub(" ","_",colnames(d),fixed=TRUE)
    d0<-d
    d<-(max(d)-d)
    d<-d/max(d)
    diag(d)<-0
    maxd<-max(d)
    nd<-maxd-d
    nd<-(nd+1e-3*(d>0))
    g<-graph.adjacency(adjmatrix=d, mode= "undirected",weighted=TRUE)
    C<-membership(multilevel.community(g))
    ND<-10+nd

    for(t in sort(unique(C))){
      idc<-which(C==t)
      nd2<-nd*0
      nd2[idc,idc]<-nd[idc,idc]
      ng<-graph.adjacency(adjmatrix=nd2, mode= "undirected",weighted=TRUE)
      mst<-minimum.spanning.tree(ng)
      amst<-get.adjacency(mst,edges=TRUE)
      if(length(amst@x)>0)amst@x<-E(mst)$weight[amst@x]
      ND[idc,idc]<-as.matrix(amst[idc,idc])
    }
    NG<-graph.adjacency(adjmatrix=ND, mode= "undirected",weighted=TRUE)
    mst<-minimum.spanning.tree(NG)
    E(mst)$weight<-ifelse(E(mst)$weight>=10,E(mst)$weight-10,E(mst)$weight)
    E(mst)$weight<- max(nd)-E(mst)$weight
    g<-mst
    g<-igraph::delete.edges(g,which(E(g)$weight==0))
    gg<-g
    cg0<-clusters(gg)
    # colC<-rainbow(length(unique(C)))
    colC<-gdocs_pal()(min(20,length(unique(C))))
    colC<-colC[(seq_along(unique(C))-1)%%20+1]
    colC<-apply(col2rgb(colC)*0.8,2,function(t)rgb(t[1]/255,t[2]/255,t[3]/255))
    colC<-colC[C]
    if(!automatic_color)colC<-rep(default_color,length(colC))
    if(cg0$no>1){
      if(min(cg0$csize)==1){
        cg<-which(cg0$membership %in% which(cg0$csize==1))
        gg<-igraph::add.vertices(gg,1)
        for(kk in cg){

          gg<-igraph::add.edges(gg, edges=c(cg[1],kk),attr=list(weight=0))
        }
        cg1<-which(cg0$membership==which(cg0$csize==max(cg0$csize)))
        cg1<-cg1[which.max(igraph::degree(g)[cg1])]
        gg<-igraph::add.edges(gg,c(vcount(gg),cg1),attr=list(weight=1))
        gg<-igraph::add.edges(gg,c(vcount(gg),cg[1]),attr=list(weight=1))
      }

      if(sum(cg0$csize>1 & cg0$csize<max(cg0$csize))>1){
        icg<-which(cg0$csize<max(cg0$csize) & cg0$csize>1 )
        for(j in icg){
          gg<-igraph::add.vertices(gg,1)
          cg<-which(cg0$membership==j)
          for(kk in cg){
            gg<-igraph::add.edges(gg, edges=c(cg[1],kk),attr=list(weight=0))
          }
          cg1<-which(cg0$membership==which(cg0$csize==max(cg0$csize)))
          cg1<-cg1[which.max(igraph::degree(g)[cg1])]
          gg<-igraph::add.edges(gg,c(vcount(gg),cg1),attr=list(weight=1))
          gg<-igraph::add.edges(gg,c(vcount(gg),cg[1]),attr=list(weight=1))
        }
      }
    }

    if(nrow(d0)>=3){
      set.seed(123)
      # la<-zg<-smacofSym(d0,ndim = 2,init=jitter(cbind(cos(C/max(C+1)*2*pi),sin(C/max(C+1)*2*pi))))$conf
      # la<-zg<-smacofSym(d0,ndim = 2,init=jitter(cbind(cos(C/max(C+1)*2*pi),sin(C/max(C+1)*2*pi))),itmax=2)$conf
      set.seed(123)
      # la<-layout.kamada.kawai(gg,params=list(sart=rbind(zg,c(0,0)),niter=2000))
      tryCatch({la<-layout_with_fr(gg, niter  =5000)},error=function(e)NULL)
      tryCatch({la<-layout_with_kk(gg,coords=la,maxiter  =5)},error=function(e)NULL)
      la<-la[seq(vcount(g)),,drop=FALSE]
    }
    if(nrow(d0)<3){
      # la<-layout.kamada.kawai(g,params=list(sart=seq_along(V(g))))
      la<-layout_with_fr(g,coords=seq_along(V(g)),niter  =30000)
    }

    V(g)$imp<-colSums(dtm)
    V(g)$imp<-V(g)$imp-min(V(g)$imp)
    V(g)$imp0<-V(g)$imp/max(V(g)$imp)
    if(all(is.na(V(g)$imp0)))V(g)$imp0<-1
    V(g)$imp<-5+(30-5)*V(g)$imp0
    V(g)$col1<-rgb(0,0,0,0)
    if(length(E(g)$weight)>0){
      E(g)$weight<-E(g)$weight-min(E(g)$weight)
      E(g)$weight<-E(g)$weight/max(E(g)$weight)
    }
    MYCOLOR=rgb(86/256,130/256,3/256,0.75)
    V(g)$col2<-colb(T=V(g)$imp0,alpha=0.8+0.2*V(g)$imp0,c1=MYCOLOR,c2=MYCOLOR)
    cbg<-cbg<-rgb(1,1,1,0)

    par(mar=c(0,0,1,0))
    r<-0.01
    rx<-range(la[,1]);rx<-c(rx[1]-6*r*diff(rx),rx[2]+6*r*diff(rx))
    ry<-range(la[,2]);ry<-c(ry[1]-r*diff(ry),ry[2]+r*diff(ry))
    # la<-wordcloud::wordlayout(x=la[,1], y = la[,2], words = rownames(d), cex = V(g)$imp/6,xlim=rx,ylim=ry)[,1:2]


    a<-lapply(seq_along(E(g)$weight),function(u)la[ends(g,u,names=FALSE),1])%>%do.call(rbind,.)%>%as.data.frame
    b<-lapply(seq_along(E(g)$weight),function(u)la[ends(g,u,names=FALSE),2])%>%do.call(rbind,.)%>%as.data.frame
    colnames(a)<-c("x_dep","x_arr")
    colnames(b)<-c("y_dep","y_arr")
    a<-data.frame(a,b)

    p<-ggplot(data=a)+geom_segment(aes(x=x_dep,y=y_dep,xend = x_arr, yend = y_arr),col="gray0",alpha=0.5,lwd=1)

    p<- p+theme_pander()
    p<-p+xlab("")+ylab("")
    p<-p+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                axis.text.y=element_blank(),axis.ticks=element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),legend.position="none",
                panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),plot.background=element_blank())
    la2<-data.frame(x=la[,1],y=la[,2],name= rownames(d), cex = V(g)$imp/3, color = colC,stringsAsFactors = FALSE)
    p<-p+geom_text_repel(data=la2,aes(x=x,y=y, label = name,size=10*cex),color=la2$color,max.iter=500, segment.color= add.alpha("grey0",0.5))
    p<-p+scale_size(range=c(min_tree,max_tree))
    .e <- new.env()
    .e$a<-a
    .e$la2<-la2
    p$plot_env<-.e



    noun<-gsub(" ","_",rownames(d),fixed=TRUE)
    count<-V(g)$imp#+runif(length(V(g)$imp))*0.05
    colors<-colC
    noun<-noun[order(count)]
    colors<-colors[order(count)]
    e<-la[order(count),]

    count<-count[order(count)]
    # wordcloud::textplot(x=e[,1], y = e[,2], words = noun ,cex =count/15,new=TRUE,show.lines=FALSE,col=colors)

    e<-my_wordcloud(noun,count,colors=colors,min.freq=0,random.order=FALSE,scale=c(2,0.25),rot.per=0.2,ordered.colors=TRUE)
    la3<-la2
    la3$x<-e$x
    la3$y<-e$y
    la3$cex<-e$size
    la3$R<-e$R
    p2<-ggplot(data=la3)+geom_text(aes(x=x,y=y,angle=R, label = name,size=20*cex),color=la3$color,check_overlap = FALSE)
    p2<- p2+theme_pander()
    p2<-p2+xlab("")+ylab("")
    p2<-p2+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                  axis.text.y=element_blank(),axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),legend.position="none",
                  panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(),plot.background=element_blank())
    p2<-p2+scale_size(range=c(min_cloud,max_cloud))
    p2
    .e <- new.env()
    .e$la3<-la3
    p2$plot_env<-.e


  } else{
    p<-NULL
    p2<-NULL
  }
  return(list(p_cloud=p2,p_tree=p))
}


colb<-function(T,c1=brewer.pal(9, "Blues")[9],c2=brewer.pal(9, "Blues")[1],alpha){
  if(length(alpha)==1)alpha<-rep(alpha,length(T))
  sapply(seq_along(T),function(t){
    x<-as.vector(col2rgb(c1))/256*T[t]+(1-T[t])*as.vector(col2rgb(c2))/256
    rgb(x[1], x[2], x[3], alpha=alpha[t])
  })
}

add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2,
        function(x)
          rgb(x[1], x[2], x[3], alpha=alpha))
}

add.blanc <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2,
        function(x)
          rgb(x[1]*alpha+(1-alpha), x[2]*alpha+(1-alpha), x[3]*alpha+(1-alpha), alpha=1))
}



my_wordcloud<- function (words, freq, scale = c(4, 0.5), min.freq = 3, max.words = Inf,
                         random.order = TRUE, random.color = FALSE, rot.per = 0.1,
                         colors = "black", ordered.colors = FALSE, use.r.layout = FALSE,
                         fixed.asp = TRUE, ...)
{
  if (!fixed.asp && rot.per > 0)
    stop("Variable aspect ratio not supported for rotated words. Set rot.per=0.")
  tails <- "g|j|p|q|y"
  last <- 1
  nc <- length(colors)
  if (missing(freq)) {
    if (!require("tm"))
      stop("freq must either be non-missing, or the tm package must be available")
    if (is.character(words) || is.factor(words)) {
      corpus <- Corpus(VectorSource(words))
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, function(x) removeWords(x,
                                                       stopwords()))
    }
    else corpus <- words
    tdm <- TermDocumentMatrix(corpus)
    freq <- slam::row_sums(tdm)
    words <- names(freq)
  }
  if (ordered.colors) {
    if (length(colors) != 1 && length(colors) != length(words)) {
      stop(paste("Length of colors does not match length of words",
                 "vector"))
    }
  }
  if (min.freq > max(freq))
    min.freq <- 0
  overlap <- function(x1, y1, sw1, sh1) {
    if (!use.r.layout)
      return(wordcloud:::.overlap(x1, y1, sw1, sh1, boxes))
    s <- 0
    if (length(boxes) == 0)
      return(FALSE)
    for (i in c(last, 1:length(boxes))) {
      bnds <- boxes[[i]]
      x2 <- bnds[1]
      y2 <- bnds[2]
      sw2 <- bnds[3]
      sh2 <- bnds[4]
      if (x1 < x2)
        overlap <- x1 + sw1 > x2 - s
      else overlap <- x2 + sw2 > x1 - s
      if (y1 < y2)
        overlap <- overlap && (y1 + sh1 > y2 - s)
      else overlap <- overlap && (y2 + sh2 > y1 - s)
      if (overlap) {
        last <<- i
        return(TRUE)
      }
    }
    FALSE
  }
  ord <- rank(-freq, ties.method = "random")
  words <- words[ord <= max.words]
  freq <- freq[ord <= max.words]
  if (ordered.colors) {
    colors <- colors[ord <= max.words]
  }
  if (random.order)
    ord <- sample.int(length(words))
  else ord <- order(freq, decreasing = TRUE)
  words <- words[ord]
  freq <- freq[ord]
  words <- words[freq >= min.freq]
  freq <- freq[freq >= min.freq]
  if (ordered.colors) {
    colors <- colors[ord][freq >= min.freq]
  }
  thetaStep <- 0.1
  rStep <- 0.05
  plot.new()
  par(mar = c(0, 0, 0, 0))
  if (fixed.asp)
    plot.window(c(0, 1), c(0, 1), asp = 1)
  else plot.window(c(0, 1), c(0, 1))

  normedFreq <- freq/max(freq)
  size <- (scale[1] - scale[2]) * normedFreq + scale[2]
  boxes <- list()
  R<-X<-Y<-1:length(words)
  for (i in 1:length(words)) {
    rotWord <- runif(1) < rot.per
    r <- 0
    theta <- runif(1, 0, 2 * pi)
    x1 <- 0.5
    y1 <- 0.5
    wid <- strwidth(words[i], cex = size[i], ...)
    ht <- strheight(words[i], cex = size[i], ...)
    if (grepl(tails, words[i]))
      ht <- ht + ht * 0.2
    if (rotWord) {
      tmp <- ht
      ht <- wid
      wid <- tmp
    }
    isOverlaped <- TRUE
    while (isOverlaped) {
      if (!overlap(x1 - 0.5 * wid, y1 - 0.5 * ht, wid,
                   ht) && x1 - 0.5 * wid > 0 && y1 - 0.5 * ht >
          0 && x1 + 0.5 * wid < 1 && y1 + 0.5 * ht < 1) {
        if (!random.color) {
          if (ordered.colors) {
            cc <- colors[i]
          }
          else {
            cc <- ceiling(nc * normedFreq[i])
            cc <- colors[cc]
          }
        }
        else {
          cc <- colors[sample(1:nc, 1)]
        }
        # text(x1, y1, words[i], cex = size[i], offset = 0,
        #      srt = rotWord * 90, col = cc, ...)
        X[i]<-x1
        Y[i]<-y1
        R[i]<-rotWord * 90
        boxes[[length(boxes) + 1]] <- c(x1 - 0.5 * wid,
                                        y1 - 0.5 * ht, wid, ht)
        isOverlaped <- FALSE
      }
      else {
        if (r > sqrt(0.5)) {
          warning(paste(words[i], "could not be fit on page. It will not be plotted."))
          isOverlaped <- FALSE
        }
        theta <- theta + thetaStep
        r <- r + rStep * thetaStep/(2 * pi)
        x1 <- 0.5 + r * cos(theta)
        y1 <- 0.5 + r * sin(theta)
      }
    }
  }

  return(list(x=X,y=Y,R=R,size=size))
}
