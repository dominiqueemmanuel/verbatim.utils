# name <- "nom d'une table"
# final_report_verbatim <- function(file,dtm,table_ts = NULL){
#' @export final_report_verbatim
final_report_verbatim <- function( file, name,object,global_table,txt0,f,dtm_origine,table_ts_origine = NULL,id_concat=NULL,names_concat="TOTAL",...){
  if(is.null(id_concat))id_concat<-rep(1,nrow(dtm))
   # save(file="dozz",list=ls())
   # load("C:/Users/Dominique/Desktop/Stat_Regie/data/application_data/dozz")
  if(!is.null(object)){
    object$dtm<-1*(object$dtm>0)
  }
  dtm_origine<-1*(dtm_origine>0)
   # save(file="dozz",list=ls())
  # load("C:/Users/Dominique/Desktop/Stat_Regie/data/application_data/dozz")
  # dtm<-object$dtm
  # set.seed(123)
  # x<-sample(250:850,20)
  # global_table <- data.frame(`ID thème`=seq_along(x)
  #                            ,`Libellé thème`=paste0("Thème ",seq_along(x))
  #                            ,`Occurences`=as.integer(x)
  #                            ,stringsAsFactors = FALSE
  #                            ,check.names = FALSE
  #                            ,row.names = NULL)
  # set.seed(123)
  # table_ts<-data.frame(Country = sample(c("France","Espagne","USA"),nrow(dtm),replace=TRUE),stringsAsFactors = FALSE)
  # table_ts$Country[1:2]<-NA
  # name <- "la_table"
  # txt0 <- object$txtp
  # file <- "dom.pptx"
  #

  model_file <- system.file("data/model.pptx",package = "verbatim.utils")
  # model_file<-'data/model.pptx'
  library(ReporteRs)
  mydoc <- pptx(  title = "Verbatims"
                  ,template = model_file
  )
  date<-format(Sys.Date(),"%d/%m/%Y")

  ## titre
  mydoc <- addSlide(mydoc,slide.layout = 'titre_global')
  mydoc <- addParagraph( mydoc,  paste0("Analyse de verbatims\n",name))
  mydoc <- addParagraph( mydoc, date)


  ###################################
  ## Analyse(s) simple(s)
  ###################################
  if(max(id_concat)==1)names_concat<-paste0(names_concat,collapse=", ")
  for(kkk in sort(unique(id_concat))){
    nom_var <- names_concat[kkk]
    idd<-which(id_concat==kkk)
    dtm<-dtm_origine[idd,,drop=FALSE]
    if(!is.null(table_ts_origine))table_ts<-table_ts_origine[idd,,drop=FALSE]
   ## analyse global
  mydoc <- addSlide(mydoc,slide.layout = 'titre_section')
  mydoc <- addParagraph( mydoc,  paste0("Analyse simple globale - ",nom_var))


  res<-intermediary_report_simple_analysis_affiche(dtm,title="TOTAL",only_result = TRUE,min_tree=2,max_tree=8,min_cloud=3,max_cloud=12)
  if(!is.null(res$x$p_cloud)){
  mydoc <- addSlide(mydoc,slide.layout = 'rendu2')

  mydoc <- addParagraph( mydoc,  paste0("Analyse simple globale - Nuage et arbre"))
  mydoc = addPlot( doc = mydoc, fun = print, x = res$x$p_cloud)
  mydoc = addPlot( doc = mydoc, fun = print, x = res$x$p_tree)
  }
  if(!is.null(res$tab2) && length(res$tab2) && NROW(res$tab2)>0 && nrow(res$tab2)>0){
  mydoc <- addSlide(mydoc,slide.layout = 'rendu1')

  mydoc <- addParagraph( mydoc,  paste0("Top 30 en FREQUENCE des mots les plus représentés"))
  mydoc <- addParagraph( mydoc,  paste0("Analyse simple globale - Mots les plus fréquents"))
  MyFTable <- vanilla.table(res$tab2)
  MyFTable = setZebraStyle( MyFTable, odd = '#eeeeee', even = 'white' )
  MyFTable[,1:2]= textProperties( font.size = 10 )
  mydoc = addFlexTable( doc = mydoc,MyFTable ,height=4,offx=2,offy=1.5,width=6)
  }
  ## analyse pas cible
  if(!is.null(table_ts_origine)){
    ## analyse global
    mydoc <- addSlide(mydoc,slide.layout = 'titre_section')
    mydoc <- addParagraph( mydoc,  paste0("Analyses simples par cible - ",nom_var))

    for(k1 in seq(ncol(table_ts))){
      for(k2 in sort(unique(table_ts[,k1]))){
        id<-which(table_ts[,k1]==k2)
        if(length(id)>=10){



          mydoc <- addSlide(mydoc,slide.layout = 'rendu2')
          res <- intermediary_report_simple_analysis_affiche(dtm[id,],dtm_ref = dtm,title=paste0(colnames(table_ts)[k1]," = ",k2),only_result = TRUE,min_tree=2,max_tree=8,min_cloud=3,max_cloud=12)

          mydoc <- addParagraph( mydoc,  paste0("Analyses simples par cible - Nuage et arbre : ",colnames(table_ts)[k1]," = ",k2))
          mydoc = addPlot( doc = mydoc, fun = print, x = res$x$p_cloud)
          mydoc = addPlot( doc = mydoc, fun = print, x = res$x$p_tree)

          mydoc <- addSlide(mydoc,slide.layout = 'rendu2')
          mydoc <- addParagraph( mydoc,  paste0("Analyses simples par cible - Nuage et arbre : ",colnames(table_ts)[k1]," = ",k2))
          mydoc <- addParagraph( mydoc,  paste0("Top 30 en FREQUENCE des mots les plus représentés"))
          mydoc <- addParagraph( mydoc,  paste0("Top 30 en INDICE des mots les plus représentés"))

          MyFTable1 <- vanilla.table(res$tab2)
          MyFTable1 = setZebraStyle( MyFTable1, odd = '#eeeeee', even = 'white' )
          MyFTable1[,1:4]= textProperties( font.size = 9 )

          MyFTable2 <- vanilla.table(res$tab3)
          MyFTable2 = setZebraStyle( MyFTable2, odd = '#eeeeee', even = 'white' )
          MyFTable2[,1:4]= textProperties( font.size = 9 )


          mydoc = addFlexTable( doc = mydoc,MyFTable1 ,height=4,offx=0.76,offy=1.5,width=3.5)
          mydoc = addFlexTable( doc = mydoc,MyFTable2 ,height=4,offx=5.76,offy=1.5,width=3.5)
        }

      }
    }
  }

}

  ###################################
  ## Analyse des thèmes
  ###################################
  if(!is.null(object)){
    mydoc <- addSlide(mydoc,slide.layout = 'titre_section')
    mydoc <- addParagraph( mydoc,  paste0("Analyses des thèmes"))
    X <-  object$dtm %*% object$word_vectors #2349 40
    Y<- t(object$txtd[,-1]) %*% X
    D<-(proxy::dist(Y%>%as.matrix,X%>%as.matrix,methode="cosine"))[,]%>%as.matrix
    if(isTRUE(length(global_table)>0 && nrow(global_table)>0)){
    x<-data.frame(Thème=global_table$Occurences,row.names = global_table$`Libellé thème`)
    x<-x[order(x$Thème),,drop=FALSE]
    library(graphpdd)
    p<-graphpdd(data = x,type_general = "Qualitatif",lib_var = "Thèmes")
    p<-p+ylab("")
    p<-p+coord_flip()
    p$layers[[1]]$aes_params$fill<-rgb(54/255,127/255,169/255)

    mydoc <- addSlide(mydoc,slide.layout = 'rendu1')
    mydoc = addPlot( doc = mydoc, fun = print, x =p)
    mydoc <- addParagraph( mydoc,  paste0("Fréquences (nombres d'occurences) des thèmes"))
    x<-object$topic_matrix
    colnames(x)<-global_table$`Libellé thème`
    res<-intermediary_report_simple_analysis_affiche( x,title="Thèmes",only_result = TRUE,min_tree=2,max_tree=8,min_cloud=3,max_cloud=12)
    x<-as.matrix(x)
    y<-t(x)%*%x
    y<-100*t(t(y/colSums(x))/colSums(x))*nrow(x)
    diag(y)<-100
    # print("a")
    colnames(y)<-rownames(y)<-colnames(y)%>%str_wrap(5)
    # print("b")
    max(y)
    library(seriation)
    s<-seriate(y,method="BEA_TSP")
    y<-y[get_order(s,dim=1),get_order(s,dim=2)]
    p<-graphpdd(data = y,is_mono = FALSE,is_indice=TRUE,is_heatmap = TRUE,type_general = c("Qualitatif","Qualitatif"),lib_var=c("Thèmes","Thèmes"),angle = 45 , nr1 = 40)
    # p<-p+theme(axis.text.x = element_text(angle = 90, hjust = 1))
    mydoc <- addSlide(mydoc,slide.layout = 'rendu1')
    mydoc = addPlot( doc = mydoc, fun = print, x =p,vector.graphic=FALSE)
    mydoc <- addParagraph( mydoc,  paste0("Analyses des liens entre thèmes : Indices croisés (base 100)"))

    mydoc <- addSlide(mydoc,slide.layout = 'rendu1')
    mydoc = addPlot( doc = mydoc, fun = print, x =res$x$p_tree)
    mydoc <- addParagraph( mydoc,  paste0("Analyses des liens entre thèmes : arbre de thèmes"))
}
    if(!is.null(table_ts_origine) && ncol(table_ts_origine)>0){

      Z<-list()
      for(k1 in seq(ncol(table_ts_origine))){

        e<-table(table_ts_origine[,k1])
        e<-sort(names(e[e>=10]))
        z<-lapply(e,function(k2){
          1*(table_ts_origine[,k1]==k2)
        })%>%do.call(cbind,.)
        # print(is(z))
        # print("+")
        # print(length(z))
        # print("+")
        #
        # print(str(z))
         print("a1")
        if(length(z)>0){
        colnames(z)<-e
        # print("a2")
        z

        z[is.na(z)]<-0
        Z[[k1]]<-z
        y<-t(x)%*%z
        y<-t(100*t(t(y/colSums(x))/colSums(z))*nrow(x))

        library(seriation)
        s<-seriate(y,method="BEA_TSP")
        y<-y[get_order(s,dim=1),get_order(s,dim=2)]

        p<-graphpdd(data = y,is_indice=TRUE,is_mono = FALSE,is_heatmap = TRUE,type_general = c("Qualitatif","Qualitatif"),lib_var=c(colnames(table_ts_origine)[k1],"Thèmes"),angle = 45 , nr1 = 40)
        # p<-p+theme(axis.text.x = element_text(angle = 90, hjust = 1))
        mydoc <- addSlide(mydoc,slide.layout = 'rendu1')
        mydoc = addPlot( doc = mydoc, fun = print, x =p,vector.graphic=FALSE)
        mydoc <- addParagraph( mydoc,  paste0("Analyses des liens entre thèmes et cibles (",colnames(table_ts_origine)[k1],") : Indices croisés (base 100)"))
        }
        print("a2")
      }
    }

    for(kk in seq_along(global_table[,1])){
      cat(".")
      occ<-global_table$Occurences[kk]%>%prettyNum(" ")%>%paste0(" (",.," occurences)")
      tab1<-object$rule_table%>%subset(topic==kk)
      # z<-tab1$terms

      tab1<-tab1%>%group_by(rule)%>%summarise(`Règle`=paste0(terms,collapse=" ET "))%>%as.data.frame%>%select(Règle)
      res <- intermediary_report_simple_analysis_affiche(object$dtm[which(object$txtd[,1+kk]==1),],dtm_ref = object$dtm,title=paste0(colnames(table_ts_origine)[k1]," = ",k2),only_result = TRUE,min_tree=2,max_tree=8,min_cloud=3,max_cloud=12)


      mydoc <- addSlide(mydoc,slide.layout = 'rendu1')
      mydoc <- addParagraph( mydoc,  paste0("Rappel de la définition du thème"))
      mydoc <- addParagraph( mydoc,  paste0("Analyse du thème ",global_table[kk,]$`Libellé thème`,occ," - Définition"))

      MyFTable<-vanilla.table(tab1)
      MyFTable = setZebraStyle( MyFTable, odd = '#eeeeee', even = 'white' )
      MyFTable[,1]= textProperties( font.size = 10 )
      MyFTable[,1]= parLeft()
      MyFTable[,1,to = 'header']= parLeft()
      mydoc = addFlexTable( doc = mydoc,MyFTable ,height=5.7,offx=3.8,offy=1.6,width=2.5)

      mydoc <- addSlide(mydoc,slide.layout = 'rendu2')
      mydoc <- addParagraph( mydoc,  paste0("Analyse du thème ",global_table[kk,]$`Libellé thème`,occ," - Nuage et arbre"))
      mydoc = addPlot( doc = mydoc, fun = print, x = res$x$p_cloud)
      mydoc = addPlot( doc = mydoc, fun = print, x = res$x$p_tree)

      mydoc <- addSlide(mydoc,slide.layout = 'rendu2')
      mydoc <- addParagraph( mydoc,  paste0("Analyse du thème ",global_table[kk,]$`Libellé thème`,occ," - Mots les plus fréquents"))
      mydoc <- addParagraph( mydoc,  paste0("Top 30 en FREQUENCE des mots les plus représentés"))
      mydoc <- addParagraph( mydoc,  paste0("Top 30 en INDICE des mots les plus représentés"))

      MyFTable1 <- vanilla.table(res$tab2)
      MyFTable1 = setZebraStyle( MyFTable1, odd = '#eeeeee', even = 'white' )
      MyFTable1[,1:4]= textProperties( font.size = 9 )

      MyFTable2 <- vanilla.table(res$tab3)
      MyFTable2 = setZebraStyle( MyFTable2, odd = '#eeeeee', even = 'white' )
      MyFTable2[,1:4]= textProperties( font.size = 9 )


      mydoc = addFlexTable( doc = mydoc,MyFTable1 ,height=4,offx=0.76,offy=1.5,width=3.5)
      mydoc = addFlexTable( doc = mydoc,MyFTable2 ,height=4,offx=5.76,offy=1.5,width=3.5)

      mydoc <- addSlide(mydoc,slide.layout = 'rendu2')
      mydoc <- addParagraph( mydoc,  paste0("Analyse du thème ",global_table[kk,]$`Libellé thème`,occ," - Verbatims représentatifs"))
      mydoc <- addParagraph( mydoc,  paste0("Top 30 des verbatims les plus représentatifs du thème"))
      e<-intersect(order(D[kk,]), which(object$txtd[,1+kk]==1))
      e<-e[seq_along(e)<=30]

      t<-txt0[e]
      t<-ifelse(nchar(t)>=270*2,paste0(substr(str_wrap(t,270),1,2*270-10)," [...]"),str_wrap(t,270))
      tab4<-data.frame(`Top 30 des verbatims les plus représentatifs du thème`=t,stringsAsFactors = FALSE,check.names = FALSE)

      MyFTable<-vanilla.table(tab4)
      MyFTable = setZebraStyle( MyFTable, odd = '#eeeeee', even = 'white' )
      MyFTable[,1]= textProperties( font.size = 8 )
      MyFTable[,1]= parLeft()
      MyFTable[,1,to = 'header']= parLeft()
      mydoc = addFlexTable( doc = mydoc,MyFTable ,height=3,offx=0.4,offy=1.5,width=8.9)

      if(!is.null(table_ts_origine) && length(Z)>0){

        for(k1 in seq(length(Z))){
          # save(file="dom",list=ls())
          # print("i1")
          a<-data.frame(Global = colSums(Z[[k1]]),Thème=colSums(Z[[k1]][x[,kk]==1,,drop=FALSE]))
          # print("i2")
          a$r<-rownames(a)
          a$q<-substr(rownames(a),1,rownames(a)%>%regexpr(" = ",.)-1)
          aa<-a%>%group_by(q)%>%summarise(s1=sum(Global),s2=sum(Thème))
          a<-left_join(a,aa,by="q")%>%mutate(Global=Global/s1,Thème=Thème/s2)%>%dplyr::select(r,Global,Thème)
          rownames(a)<-a$r
          a<-dplyr::select(a,-r)
          # a[,1]<-a[,1]/sum(a[,1])
          # a[,2]<-a[,2]/sum(a[,2])
          print("x1")
          colnames(a)[2]<-global_table$`Libellé thème`[kk]
          print("x2")
          # p<-graphpdd(data=t(a),is_mono = FALSE,type_general =c("Qualitatif","Qualitatif"),lib_var = c("",""))+

          p<-graphpdd(data=a,is_mono = TRUE,type_general =c("Qualitatif","Qualitatif"),lib_var = colnames(a),angle = 45 , nr1 = 40)+  scale_y_continuous(labels = scales::percent)
          p<-p+ylab("")
          a<-p$data$x
          levels(a)<-str_wrap(levels(a),10)
          p$data$x<-a
          p<-p+ theme(legend.title=element_blank())
          mydoc <- addSlide(mydoc,slide.layout = 'rendu1')
          mydoc = addPlot( doc = mydoc, fun = print, x =p)

          mydoc <- addParagraph( mydoc,  paste0("Analyse du thème ",global_table[kk,]$`Libellé thème`,occ," - Croisement avec ",colnames(table_ts_origine)[k1]))
        }
      }

    }
  }

  writeDoc( mydoc, file )

}
