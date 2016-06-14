#' @export intermediary_report
intermediary_report <- function(file,object,global_table){
pdf(file,width=20,height=ceiling(20/(2)))
for(kk in seq_along(verbatim_rv$global_table[,1])){
  cat(".")
  tab1<-object$rule_table%>%subset(topic==kk)
  z<-tab1$terms

  tab1<-tab1%>%group_by(rule)%>%summarise(`Règle`=paste0(terms,collapse=" ET "))%>%as.data.frame%>%select(Règle)
  # tab1$Règle<-ifelse(grepl(" ET ",tab1$Règle,fixed=TRUE),paste0("(",tab1$Règle,")"),tab1$Règle)
  # tab1$Règle[seq_along(tab1$Règle)>=2] <- paste0("OU   ",tab1$Règle[seq_along(tab1$Règle)>=2])
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


  a<-colMeans(object$dtm[object$txtd[,1+kk]==1,,drop=FALSE])
  b<-colMeans(object$dtm)
  id1<-order(a,decreasing = TRUE)[seq_along(a)<=30]
  id2<-order(a/ifelse(b==0,1,b),a,decreasing = TRUE)[seq_along(a)<=30]
  b<-data.frame(`Mot` = colnames(object$dtm),`Fréquence\n au sein du\nthème`=paste0(round(a*100,2),"%")
                ,`Fréquence\n globale`=paste0(round(b*100,2),"%")
                ,`Indice\n (base 100)`=round(100*a/b,0)
                ,stringsAsFactors = FALSE,check.names=FALSE)
  tab2<-b[id1,]
  tab3<-b[id2,]
  e<-matrix(tab1[,1],nrow=min(nrow(tab1),30))
  e[seq_along(e)>nrow(tab1)]<-""
  e<-as.data.frame(e)
  g1<-ft(e,str_wrap("Liste des règles définissant le thème",30),u=rep(c(6),ncol(e)),cols=NULL)
  g2<-ft(tab2,str_wrap("Top 30 en FREQUENCE de mots les plus représentés",30),c(3,2,2,2),which(tab2$Mot %in% z))
  g3<-ft(tab3,str_wrap("Top 30 en INDICE de mots les plus représentés",30),c(3,2,2,2),which(tab3$Mot %in% z))
  # grid.newpage()
  grid.arrange(g1,g2,g3, ncol=3
               ,top =textGrob(paste0(global_table$`Libellé thème`[kk]," \n(",global_table$Occurences[kk]," occurences)"),gp=gpar(fontsize=20,font=3))
               ,bottom =textGrob("Les mots en gras dans les deux tableaux de droite sont les mots apparaissant dans les règles définissant le thème. Vous pouvez ENRICHIR ce thème en identifiant les MOTS ABSENTS DES RÈGLES mais COHÉRENTS AVEC LE SENS GLOBAL que vous identifiez => ajoutez ensuite ces mots dans les règles du thème (au sein l'application). Vous pouvez également NETTOYER ce thème en identifiant les MOTS EN GRAS mais INCOHÉRENTS AVEC LE SENS GLOBAL => retirez ensuite ces mots dans les règles du thème (au sein de l'application). Vous pouvez également SUPPRIMER ce thème si AUCUN SENS GLOBAL ne ressort"%>%str_wrap(115),gp=gpar(fontsize=10,font=3),just="left")
  )

  x<-cloud_tree(object$dtm[object$txtd[,1+kk]==1,,drop=FALSE])#,dtm_base=object$dtm,method="indice")
  # grid.newpage()
  g<-arrangeGrob(x$p_cloud
                 ,x$p_tree
                 , ncol=2,top =textGrob(paste0(global_table$`Libellé thème`[kk]," \n(",global_table$Occurences[kk]," occurences)"),gp=gpar(fontsize=20,font=3))
  )
  vps <- baseViewports()
  pushViewport(vps$inner, vps$figure, vps$plot)
  grid.draw(g)
}
dev.off()


}

