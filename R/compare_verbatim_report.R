#####zzz
# options(java.parameters = "-Xmx4096m")
# load("C:/Users/Dominique/Desktop/Stat_Regie/data/application_data/params2")
# freq_global_topic_old<-params$model$freq_global_topic
# table_var<-params$table_var
# table_cible<-params$table_cible
# str(freq_global_topic_old,max.level = 2)
# str(freq_global_topic,max.level = 2)
# concat_old <- params$model$concat
# file<-"test.xlsx"
# names_topic <- params$global_table$`Libellé thème`
# global_table_old<-params$model$global_table
# global_table<-params$global_table
#' @export compare_verbatim_report
compare_verbatim_report <- function(freq_global_topic_old,freq_global_topic,table_var,table_cible,concat_old,file=test,names_topic ,global_table_old,global_table){
  # save(file="domm",list=ls())
  # load("C:/Users/Dominique/Desktop/Stat_Regie/data/application_data/domm")
  N_old<-sum(sapply(freq_global_topic_old$stat_freq_global,function(t)attr(t,"N")))
  N_new<-sum(sapply(freq_global_topic$stat_freq_global,function(t)attr(t,"N")))

  table_var<-table_var[which(!is.na(table_var[,1]) & table_var[,1]!="" & !is.na(table_var[,2]) & table_var[,2]!=""),,drop=FALSE]
  if(nrow(table_var)>0){
    model_file <- system.file("data/model.xlsx",package = "verbatim.utils")
    # model_file <-"./data/model.xlsx"

    file.copy(from = model_file,to=file,overwrite = TRUE)
    tryCatch(detach("package:xlsx", unload=TRUE),error=function(e)NULL)
    tryCatch(detach("package:xlsxjars", unload=TRUE),error=function(e)NULL)

    library(dplyr)
    library(XLConnect)
    wb <- loadWorkbook(file, create=FALSE)
    # attr(wb,"n")<-3
    xx<-c()

    ## comparaison des thèmes eux-mêmes
    b<-global_table[,2:3]
    b[,2]<-b[,2]/N_new
    colnames(b)<-c("Thème","Fréquence APRÈS")

    a<-global_table_old[,2:3]
    a[,2]<-a[,2]/N_old
    colnames(a)<-c("Thème","Fréquence AVANT")

    c<-full_join(a,b,by="Thème")
    c[is.na(c)]<-0
    c$`Volume AVANT`<-c$`Fréquence AVANT`*N_old
    c$`Volume APRÈS`<-c$`Fréquence APRÈS`*N_new
    c$`Fréquence APRÈS - AVANT`<-c[,3]-c[,2]
    c<-c[order(-abs(c$`Fréquence APRÈS - AVANT`)),]
    head(c)
    c$`P-Value`<-abs(c$`Fréquence APRÈS`-c$`Fréquence AVANT`)/sqrt((c$`Fréquence APRÈS`)*(1-c$`Fréquence APRÈS`)/N_new+(c$`Fréquence AVANT`)*(1-c$`Fréquence AVANT`)/N_old)
    c$`P-Value`<-2*(1-pnorm(c$`P-Value`))
    c1<-subset(c,is.na(`Fréquence APRÈS - AVANT`) | `Fréquence APRÈS - AVANT`>=0)%>%head(100)
    c2<-subset(c,is.na(`Fréquence APRÈS - AVANT`) | `Fréquence APRÈS - AVANT`<0)%>%head(100)
    x<-add_sheet_from_model(wb,c1,title=paste0("Thèmes en augmentation"),sheet_model = "model2")
    eval(parse(text=paste0("xx<-c(xx,`",x,"`=",nrow(c1),")")))
    x<-add_sheet_from_model(wb,c2,title=paste0("Thèmes en diminution"),sheet_model = "model2")
    eval(parse(text=paste0("xx<-c(xx,`",x,"`=",nrow(c2),")")))

    ## global et par cible
    for(k in names(freq_global_topic$stat_freq_global)){
      if(!concat_old){
        k0<-table_var[,1][table_var[,2]==k]
      } else {
        k0<-names(freq_global_topic_old$stat_freq_global)[1]
      }
      b<-freq_global_topic$stat_freq_global[[k]]

      condition_concat_avant_apres <- isTRUE(k0 %in% names(freq_global_topic_old$stat_freq_global) & (!concat_old | length(freq_global_topic$stat_freq_global)==1))
      if(condition_concat_avant_apres){
        a<-freq_global_topic_old$stat_freq_global[[k0]]
      } else {
        a<-b[,,drop=FALSE]
        a[,2]<-(-123)
        attr(a,"N")<-0
      }
      Na<-attr(a,"N")
      Nb<-attr(b,"N")
      if(is.null(Na))Na<-0
      if(is.null(Nb))Nb<-0
      colnames(a)[2]<-paste0(colnames(a)[2]," AVANT")
      colnames(b)[2]<-paste0(colnames(b)[2]," APRÈS")
      c<-full_join(a,b,by="Mot")
      c[is.na(c)]<-0
      c[c==-123]<-NA
      c$`Volume AVANT`<-c$`Fréquence AVANT`*Na
      c$`Volume APRÈS`<-c$`Fréquence APRÈS`*Nb
      c$`Fréquence APRÈS - AVANT`<-c[,3]-c[,2]
      c<-c[order(-abs(c$`Fréquence APRÈS - AVANT`)),]

      c$`P-Value`<-abs(c$`Fréquence APRÈS`-c$`Fréquence AVANT`)/sqrt((c$`Fréquence APRÈS`)*(1-c$`Fréquence APRÈS`)/Nb+(c$`Fréquence AVANT`)*(1-c$`Fréquence AVANT`)/Na)
      c$`P-Value`<-2*(1-pnorm(c$`P-Value`))
      c1<-subset(c,is.na(`Fréquence APRÈS - AVANT`) | `Fréquence APRÈS - AVANT`>=0)%>%head(100)
      c2<-subset(c,is.na(`Fréquence APRÈS - AVANT`) | `Fréquence APRÈS - AVANT`<0)%>%head(100)
      x<-add_sheet_from_model(wb,c1,title=paste0("Au global - Mots en augmentation : [",k,"]"))
      eval(parse(text=paste0("xx<-c(xx,`",x,"`=",nrow(c1),")")))
      x<-add_sheet_from_model(wb,c2,title=paste0("Au global - Mots en diminution : [",k,"]"))
      eval(parse(text=paste0("xx<-c(xx,`",x,"`=",nrow(c2),")")))

      #################################### par cible
      for(i in names(freq_global_topic$stat_freq_cible[[k]])){

        i0<-table_cible[,1][table_cible[,2]==i]

        for(j in freq_global_topic$stat_freq_cible[[k]][[i]]%>%names){
          b<-freq_global_topic$stat_freq_cible[[k]][[i]][[j]]
          N<-attr(b,"N")
          b<-b[,1:2]
          attr(b,"N")<-N

          if(isTRUE(length(k0)>0  && condition_concat_avant_apres && i0 %in% names(freq_global_topic_old$stat_freq_cible[[k0]]))){
            e<-names(freq_global_topic_old$stat_freq_cible[[k0]][[i0]])
            j0<-e[which.min(stringdist::stringdist(e,b))]
            if(isTRUE(tolower(stringr::str_trim(j0))==tolower(stringr::str_trim(j0)))){
              a<-freq_global_topic_old$stat_freq_cible[[k0]][[i0]][[j0]]
              N<-attr(a,"N")
              a<-a[,1:2]
              attr(a,"N")<-N
            } else {
              a<-b[,,drop=FALSE]
              a[,2]<-(-123)
              attr(a,"N")<-0
            }
          } else {
            a<-b[,,drop=FALSE]
            a[,2]<-(-123)
            attr(a,"N")<-0
          }


          Na<-attr(a,"N")
          Nb<-attr(b,"N")
          if(is.null(Na))Na<-0
          if(is.null(Nb))Nb<-0
          colnames(a)[2]<-paste0(colnames(a)[2]," AVANT")
          colnames(b)[2]<-paste0(colnames(b)[2]," APRÈS")
          c<-full_join(a,b,by="Mot")
          c[is.na(c)]<-0
          c[c==-123]<-NA
          c$`Volume AVANT`<-c$`Fréquence AVANT`*Na
          c$`Volume APRÈS`<-c$`Fréquence APRÈS`*Nb
          c$`Fréquence APRÈS - AVANT`<-c[,3]-c[,2]
          c<-c[order(-abs(c$`Fréquence APRÈS - AVANT`)),]
          head(c)
          c$`P-Value`<-abs(c$`Fréquence APRÈS`-c$`Fréquence AVANT`)/sqrt((c$`Fréquence APRÈS`)*(1-c$`Fréquence APRÈS`)/Nb+(c$`Fréquence AVANT`)*(1-c$`Fréquence AVANT`)/Na)
          c$`P-Value`<-2*(1-pnorm(c$`P-Value`))
          c1<-subset(c,is.na(`Fréquence APRÈS - AVANT`) | `Fréquence APRÈS - AVANT`>=0)%>%head(100)
          c2<-subset(c,is.na(`Fréquence APRÈS - AVANT`) | `Fréquence APRÈS - AVANT`<0)%>%head(100)
          x<-add_sheet_from_model(wb,c1,title=paste0("Cible ",i,"=",j," - Mots en augmentation : [",k,"]"))
          eval(parse(text=paste0("xx<-c(xx,`",x,"`=",nrow(c1),")")))
          x<-add_sheet_from_model(wb,c2,title=paste0("Cible ",i,"=",j," - Mots en diminution : [",k,"]"))
          eval(parse(text=paste0("xx<-c(xx,`",x,"`=",nrow(c2),")")))


        }





      }

    }

    ## par thèmes
    for(k in seq_along(freq_global_topic$stat_freq_topic)[seq_along(freq_global_topic$stat_freq_topic)<=length(names_topic)]){
      b<-freq_global_topic$stat_freq_topic[[k]]
      N<-attr(b,"N")
      b<-b[,1:2]
      attr(b,"N")<-N

      if(k<=length(freq_global_topic_old$stat_freq_topic)){
        a<-freq_global_topic_old$stat_freq_topic[[k]]
        N<-attr(a,"N")
        a<-a[,1:2]
        attr(a,"N")<-N
      }

      Na<-attr(a,"N")
      Nb<-attr(b,"N")
      if(is.null(Na))Na<-0
      if(is.null(Nb))Nb<-0
      colnames(a)[2]<-paste0(colnames(a)[2]," AVANT")
      colnames(b)[2]<-paste0(colnames(b)[2]," APRÈS")
      c<-full_join(a,b,by="Mot")
      c[is.na(c)]<-0
      c$`Volume AVANT`<-c$`Fréquence AVANT`*Na
      c$`Volume APRÈS`<-c$`Fréquence APRÈS`*Nb
      c$`Fréquence APRÈS - AVANT`<-c[,3]-c[,2]
      c<-c[order(-abs(c$`Fréquence APRÈS - AVANT`)),]
      head(c)
      c$`P-Value`<-abs(c$`Fréquence APRÈS`-c$`Fréquence AVANT`)/sqrt((c$`Fréquence APRÈS`)*(1-c$`Fréquence APRÈS`)/Nb+(c$`Fréquence AVANT`)*(1-c$`Fréquence AVANT`)/Na)
      c$`P-Value`<-2*(1-pnorm(c$`P-Value`))
      c1<-subset(c,is.na(`Fréquence APRÈS - AVANT`) | `Fréquence APRÈS - AVANT`>=0)%>%head(100)
      c2<-subset(c,is.na(`Fréquence APRÈS - AVANT`) | `Fréquence APRÈS - AVANT`<0)%>%head(100)
      x<-add_sheet_from_model(wb,c1,title=paste0("Mots en augmentation dans le thème [",names_topic[k],"]"))
      eval(parse(text=paste0("xx<-c(xx,`",x,"`=",nrow(c1),")")))
      x<-add_sheet_from_model(wb,c2,title=paste0("Mots en diminution dans le thème [",names_topic[k],"]"))
      eval(parse(text=paste0("xx<-c(xx,`",x,"`=",nrow(c2),")")))


    }




    saveWorkbook(wb)
    tryCatch(detach("package:XLConnect", unload=TRUE),error=function(e)NULL)
    tryCatch(detach("package:XLConnectJars", unload=TRUE),error=function(e)NULL)
    library(xlsx)
    wb<-loadWorkbook(file)
    removeSheet(wb, sheetName="model1")
    removeSheet(wb, sheetName="model2")
    e0<-getSheets(wb)
    e<-names(e0)
    e<-setdiff(e,"Sommaire")
    # e<-e[order(1-grepl("^Thèmes en"),seq_along(e))]
    e2<-sapply(e,function(t)readRows(e0[[t]],startRow=2,endRow=2,startColumn=1,endColumn=1)[1,1])
    e<-e%>%force_encoding
    names(e2)<-e
    names(e0)<-names(e0)%>%force_encoding

    for(k in seq_along(e2)){cat(".")
      rows <- createRow(e0[["Sommaire"]], rowIndex = 3+k)
      sheetTitle <- createCell(rows, colIndex = 1)
      setCellValue(sheetTitle[[1, 1]], e2[k])
      addHyperlink(sheetTitle[[1, 1]], paste0("#'",names(e2)[k],"'!A1"), linkType = "DOCUMENT")
      if(xx[names(e2)[k]]+4<100){
        e<-getRows(e0[[names(e2)[k]]], rowIndex=(xx[names(e2)[k]]+4):max(100,(xx[names(e2)[k]]+4)))
        removeRow(e0[[names(e2)[k]]], rows=e)
      }

    }
    saveWorkbook(wb, file)
    tryCatch(detach("package:xlsx", unload=TRUE),error=function(e)NULL)
    tryCatch(detach("package:xlsxjars", unload=TRUE),error=function(e)NULL)
  }
}

add_sheet_from_model <- function(wb,data,title="",sheetName=title,sheet_model="model1",sheet_sommaire="Sommaire",header=FALSE){
  sheetName <- gsub("\\\\|/|\\*|\\[|\\]|\\:|\\?"," ",sheetName)
  sheetName <- gsub("[[:space:]]+"," ",sheetName)
  sheetName<-substr(sheetName,1,31)
  n<-1
  #Petite modification pour gérer les onglets doublonnés qui ont des accents
  name_all_sheet <- c("",getSheets(wb)%>%force_encoding)
  while(sheetName %in% name_all_sheet){
    sheetName <- paste0(substr(sheetName,1,29),n)
    n <- n+1
  }
  cloneSheet(wb,sheet_model,sheetName)
  setStyleAction(wb,XLC$"STYLE_ACTION.NONE")
  writeWorksheet(wb,data.frame(x=title),sheetName,startRow=2,startCol=1,header=FALSE)
  writeWorksheet(wb,data,sheetName,startRow=if(header) 3 else 4,startCol=1,header=header)
  # name<-make.names(Sys.time())
  # createName(wb, name = name, formula = paste0("'",sheetName,"'!$A$",nrow(data)+4,":G10000"))
  # removeName(wb, name = name)
  # writeWorksheet(wb,data.frame(x=title),sheet_sommaire,startRow=attr(wb,"n"),startCol=1,header=FALSE)
  # attr(wb,"n")<-attr(wb,"n")+1
  return(sheetName)
}
