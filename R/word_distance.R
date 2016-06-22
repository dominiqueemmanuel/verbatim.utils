#' @export word_distance
word_distance <- function(word_vectors) {
  library(Matrix)
library(data.table)
library(stringdist)
library(magrittr)
  library(stringi)
terms <- rownames(word_vectors)

d0 <- proxy::dist(word_vectors,method="cosine")%>%as.matrix%>%as("dgTMatrix")
out<-data.table(terms1 = terms[1+d0@i],terms2 = terms[1+d0@j], dist_cosine = d0@x)
# out <- out[order(terms1,dist_cosine),]
setorder(out, terms1, dist_cosine)
out[, count := 1:.N, by = terms1]
out <- subset(out , count <= 20)
out[, count := NULL]

d0<-stringdistmatrix(terms,method="osa")%>%as.matrix
# d000<-stringdistmatrix(terms,method="lcs")%>%as.matrix
# d00<-nchar(terms)
# d00<-1-1*(d000==abs(outer(d00,d00,FUN = "-")))
# d0<-d0*d00+0.01*(1-d00)
d000<-stringdistmatrix(terms,method="qgram")%>%as.matrix
 d0<-d0 + d000/100
diag(d0)<-0

d0<-as(d0,"dgTMatrix")
out2<-data.table(terms1 = terms[1+d0@i],terms2 = terms[1+d0@j], dist_osa = d0@x)
# out2 <- out2[order(terms1,dist_osa),]
setorder(out2, terms1, dist_osa)
out2[, count := 1:.N, by = terms1]
out2 <- subset(out2 , count <= 8)


out<-merge(out,out2,by=c("terms1","terms2"),all=TRUE)
out[, dist_cosine := ifelse(is.na(dist_cosine),2,dist_cosine)]
out[, dist_osa := ifelse(is.na(dist_osa),100,dist_osa)]
out[, count := ifelse(is.na(count),100,count)]
# out[, dist := (dist_osa>0.02)*((dist_osa<=4 & count<=2)*(dist_osa/100+dist_cosine/10+count/10) + ((dist_osa<=4 & count>2) | (dist_osa>4))*(1+dist_cosine) )]
out[, dist := (count<=3)*(dist_osa/1000+dist_cosine/100+count/50) + (count>3)*(1+dist_cosine)  ]


out <- out[order(terms1,dist),]
setkeyv(out,"terms1")


out[, count := NULL]
out[, dist_cosine := NULL]
out[, dist_osa := NULL]





f<-function(x,n=30){

  a<-out[J(unique(x)),]
  a<-na.omit(a[order(a$dist),]$terms2)
  a<-setdiff(a,x)
  a[seq_along(a)<=n]
}
ev<-new.env()
ev$out<-out
environment(f)<-ev
return(f)
}


#  f <- word_distance(word_vectors)
# f(c("like","perfum","smell","eau","good","friend","red"))
