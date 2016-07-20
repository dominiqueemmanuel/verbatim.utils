# load("C:/Users/Dominique/Desktop/Stat_Regie/data/application_data/dom")

# intermediary_report_simple_analysis(file="test.pdf",dtm=object$dtm,table_ts=object$txtd[,2:3])
#' @export intermediary_report_simple_analysis
intermediary_report_simple_analysis <- function(file,dtm,table_ts = NULL,id_concat=NULL,names_concat="TOTAL"){
    # save(file="doo",list=ls())
  # load("C:/Users/Dominique/Desktop/Stat_Regie/data/application_data/doo")
if(is.null(id_concat))id_concat<-rep(1,nrow(dtm))
if(max(id_concat)==1)names_concat<-paste0(names_concat,collapse=", ")
    dtm<-1*(dtm>0)
  print(file)
  library(stringr)
  library(grid)
  library(gridExtra)
  library(gtable)
  library(gridBase)

  print("Begin intermediary report...")
  library(dplyr)
  library(Matrix)
pdf(file,width=20,height=ceiling(20/(2)))
for(kkk in sort(unique(id_concat))){
  title <- names_concat[kkk]
  idd<-which(id_concat==kkk)
intermediary_report_simple_analysis_affiche(dtm[idd,,drop=FALSE],title=title)
if(!is.null(table_ts)){
  for(k1 in seq(ncol(table_ts[idd,,drop=FALSE]))){
    for(k2 in sort(unique(table_ts[idd,,drop=FALSE][,k1]))){
      id<-which(table_ts[idd,,drop=FALSE][,k1]==k2)
      if(length(id)>=10){
        intermediary_report_simple_analysis_affiche(dtm[idd,,drop=FALSE][id,,drop=FALSE],dtm_ref = dtm[idd,,drop=FALSE],title=paste0(colnames(table_ts)[k1]," = ",k2))
      }

    }

  }
}
}
dev.off()

print("... End intermediary report")
}


intermediary_report_simple_analysis_affiche <- function(dtm,dtm_ref=NULL,title,only_result=FALSE,...) {
  cat(".")
  library(stringr)
  library(grid)
  library(gridExtra)
  library(gtable)
  library(gridBase)
  ft <- function(tab1,title,u,id=NULL,cols = colnames(tab1)){
    g1<-tableGrob(tab1,rows=NULL,cols = cols
                  ,heights=rep(unit(0.5, "cm"),nrow(tab1))
                  ,widths=unit(u,"cm")
    )
    find_cell <- function(g, row, col, name="core-fg"){
      l <- g$layout
      which(l$t==row & l$l==col & l$name==name)
    }
    for(k in id){
      ind2 <- find_cell(g1, k+1, 1, "core-fg")
      g1$grobs[ind2][[1]][["gp"]] <- gpar(fontface ="bold")
    }

    title <-  textGrob(title,gp=gpar(fontsize=14))
    g1 <- gtable_add_rows(g1,
                          heights = grobHeight(title) + unit(5,"mm"),
                          pos = 0)
    g1 <- gtable_add_grob(g1, title, 1, 1, 1, ncol(tab1))
    g1}
tab2<-tab3<-NULL

  a<-colMeans(dtm)
  id1<-order(a,decreasing = TRUE)[seq_along(a)<=30]
  b<-a
  if(!is.null(dtm_ref)){
    b<-colMeans(dtm_ref)
    id2<-order(a/ifelse(b==0,1,b),a,decreasing = TRUE)[seq_along(a)<=30]

    b1<-data.frame(`Mot` = colnames(dtm)
                   ,`Fréquence`=a#paste0(round(a*100,2),"%")
                  # ,`Fréquence\n globale`=paste0(round(b*100,2),"%")
                  # ,`Indice\n (base 100)`=round(100*a/b,0)
                  ,stringsAsFactors = FALSE,check.names=FALSE)
    b2<-data.frame(`Mot` = colnames(dtm_ref)
                   # ,`Fréquence`=paste0(round(a*100,2),"%")
                   ,`Fréquence\n globale`=b#paste0(round(b*100,2),"%")
                   # ,`Indice\n (base 100)`=round(100*a/b,0)
                   ,stringsAsFactors = FALSE,check.names=FALSE)
    b<-inner_join(b1,b2,by="Mot")
    b<-b%>%mutate(`Indice\n (base 100)`=round(100*`Fréquence`/`Fréquence\n globale`,0))%>%
      mutate(`Fréquence`=paste0(round(`Fréquence`*100,2),"%"))%>%
      mutate(`Fréquence\n globale`=paste0(round(`Fréquence\n globale`*100,2),"%"))
    tab2<-b[id1,]
    tab3<-b[id2,]
if(!only_result){
    g2<-ft(tab2,str_wrap("Top 30 en FREQUENCE de mots les plus représentés",30),c(5,3,3,3))
    g3<-ft(tab3,str_wrap("Top 30 en INDICE de mots les plus représentés",30),c(5,3,3,3))
    # grid.newpage()
    grid.arrange(g2,g3, ncol=2
                 ,top =textGrob(paste0(title," \n(",nrow(dtm)," occurences)"),gp=gpar(fontsize=20,font=3))
    )
}
  } else {
    b<-data.frame(`Mot` = colnames(dtm),`Fréquence`=paste0(round(a*100,2),"%")
                  ,stringsAsFactors = FALSE,check.names=FALSE)
    tab2<-b[id1,]
    if(!only_result){
    g2<-ft(tab2,str_wrap("Top 30 en FREQUENCE de mots les plus représentés",30),c(5,3))
    # grid.newpage()
    grid.arrange(g2, ncol=1
                 ,top =textGrob(paste0(title," \n(",nrow(dtm)," occurences)"),gp=gpar(fontsize=20,font=3))
    )
    }

  }


    x<-cloud_tree(dtm,...)#,dtm_base=object$dtm,method="indice")
    if(!only_result & !is.null(x$p_cloud)){ # grid.newpage()
  g<-arrangeGrob(x$p_cloud
                 ,x$p_tree
                 , ncol=2,top =textGrob(paste0(title," \n(",nrow(dtm)," occurences)"),gp=gpar(fontsize=20,font=3))
  )
  vps <- baseViewports()
  pushViewport(vps$inner, vps$figure, vps$plot)
  grid.draw(g)
    }
    if(only_result){
      return(list(tab2=tab2,tab3=tab3,x=x))
    }



}

