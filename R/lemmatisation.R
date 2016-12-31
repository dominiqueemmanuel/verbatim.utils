## Cette fonction prend un vecteur de chaine de caractères en entrée (i.e. un vecteur de verbatims)
## Cette fonction renvoie un vecteur de chaine de caractères de même longueur

## Cette fonction utilisera Freeling
## lemmatisation(c("une Phrase avec une forme féminine et un verbe conjugué","Une phrase avec un mot inconnu xxsf"), lang="fr")
## doit renvoyer
## c("un Phrase avec un forme féminin et un verbe conjuguer","Un phrase avec un mot inconnu xxsf")

## > Penser au fait que la fonction doit être Cross Platform
## > Penser aux performance (la fonction doit être rapide sur de longs et nombreux verbatims)

## Taux de développement estimé : 1 %
## Next steps : Adapter le code échangé par mail avec Abdelhadi
## (car la fonction initial renvoyait un evecteur de longeur différentes du vecteur en entrée)
####################################################

################ Info pour le développement

#
chunk2 <- function(x,n){
  if(n>1)
    return(split(x, cut(seq_along(x), n, labels = FALSE)))
  else
    return(list(x))
}

transform_pos <- function(x , lang = "en"){
  out <- NULL
  L <- c("Adjectif","Conjonction","Déterminant","Nom","Pronom","Adverbe","Préposition","Verbe","Nombre","Date","Interjection","Ponctuation")
  if(lang=="fr"){
    out <- plyr::mapvalues(substr(x,1,1)
                           ,c("A","C","D","N","P","R","S","V","Z","W","I","F")
                           ,L
                           , warn_missing = FALSE
    )
  }

  if(lang=="en"){
    out <- ifelse(x %in% c("JJ","JJR","JJS"),L[1],
                  ifelse(x %in% c("CC"),L[2],
                         ifelse(x %in% c("DT","WDT","PDT"),L[3],
                                ifelse(x %in% c("NNS","NN","NNP","NP00000","NP","NP00G00","NP00O00","NP00V00","NP00SP0","NNPS"),L[4],
                                       ifelse(x %in% c("EX","WP","PRP","PRP$","WP$"),L[5],
                                              ifelse(x %in% c("RB","RBR","RBS","WRB"),L[6],
                                                     ifelse(x %in% c("POS","IN","RP","TO"),L[7],
                                                            ifelse(x %in% c("MD","VBG","VB","VBN","VBD","VBP","VBZ"),L[8],
                                                                   ifelse(substr(x,1,1) == "Z",L[9],
                                                                          ifelse(substr(x,1,1) == "W",L[10],
                                                                                 ifelse(substr(x,1,1) == "I" | x=="UH",L[11],
                                                                                        ifelse(substr(x,1,1) == "F" ,L[12],

                                                                                        x
                                                                                 ))))))))))))
  }
  if(lang=="ge"){
    out <- plyr::mapvalues(substr(x,1,1)
                           ,c("A","C","D","N","P","R","S","V","Z","W","I","F")
                           ,L
                           , warn_missing = FALSE
    )

  }

  if(lang=="sp"){
    out <- plyr::mapvalues(substr(x,1,1)
                           ,c("A","C","D","N","P","R","S","V","Z","W","I","F")
                           ,L
                           , warn_missing = FALSE
    )

  }

  if(lang=="it"){
    out <- plyr::mapvalues(substr(x,1,1)
                           ,c("A","C","D","N","P","R","S","V","Z","W","I","F")
                           ,L
                           , warn_missing = FALSE
    )

  }

  if(lang=="pt"){
    out <- plyr::mapvalues(substr(x,1,1)
                           ,c("A","C","D","N","P","R","S","V","Z","W","I","F")
                           ,L
                           , warn_missing = FALSE
    )

  }

  if(lang=="ru"){
    out <- plyr::mapvalues(substr(x,1,1)
                           ,c("A","C","T","N","E","D","B","V","Z","W","J","F",    "C","P","R","Q","Y","I")
                           ,c(L,"Nom","Adverbe","Adjectif","Verbe","Nombre","Interjection")
                           , warn_missing = FALSE
    )

  }
  return(out)


}
sub_lemmatisation <- function(txt, lang="en", path = NULL,remove = NULL){
  # cat(".")
library(stringi)
  ## Nom de fichier source
  testtxt_in_name <- tempfile()
  ## Le fichier source
  testtxt_in<-file(testtxt_in_name,encoding="UTF-8")
  writeLines(txt,testtxt_in)
  close(testtxt_in)

  ## Nom de fichier en sortie
  testtxt_out_name <- tempfile()
  ## Le fichier en sortie
  testtxt_out<-file(testtxt_out_name,encoding="UTF-8")

  ## Écriture de la commande
  if(is.null(path)){
    command <-   paste0("analyze  --nonumb --nodate --flush --ner --force retok --afx  --output freeling --input text --inplv text -f ",lang,".cfg <",testtxt_in_name," >",testtxt_out_name)
  } else {
    command <-   paste0(path ,  "  --nonumb --nodate --flush --ner --force retok  --afx  --output freeling --input text --inplv text -f ",paste0(dirname(dirname(path)),"/data/config/"),lang,".cfg <",testtxt_in_name," >",testtxt_out_name,"  --tlevel 0")
  }
  ## Exécution de la commande
  if(.Platform[[1]]=="windows") {

    res <- shell(command)
    cat(command)
  } else {
    res <- system(command)
  }
  close(testtxt_out)

  ## s'il y a eu un problème on renvoi NULL
  if(res!=0){
    print("ERREUR LEMMATISATION")
    txt<-str_split(txt,"MOT_SEPARATEUR_DE_VERBATIM")[[1]]%>%str_trim
    return(list(txt_lemme=txt,txt_categ=NULL))
  }

  ## On lit le fichier en sortie

  out<-readLines(testtxt_out_name,encoding="UTF-8")

    # save(file="dom",list=ls())
  # out <- out[-length(out)]
  out <- str_split(out," ")
  out <- out[lapply(out, length) == 4]
  out_in <- sapply(out,function(t)t[[1]])
  out_lemme <- sapply(out,function(t)t[[2]])
  out_categ <- sapply(out,function(t)t[[3]])


  if(!is.null(remove)){
    id<-which(grepl("_",out_lemme) | !(transform_pos(out_categ,lang=lang) %in% remove))
    out_in<-out_in[id]
    out_lemme<-out_lemme[id]
    out_categ<-out_categ[id]
  }
  a<-sapply(stri_match_all_regex(out_lemme,"_"),function(t)length(na.omit(t)))
  b<-sapply(stri_match_all_regex(out_in,"_"),function(t)length(na.omit(t)))
  # out_lemme<-ifelse(grepl("_",out_lemme) & !grepl("_",out_in), out_in,out_lemme)
  out_lemme<-ifelse(a>b, out_in,out_lemme)
  out_categ[out_lemme==tolower("MOT_SEPARATEUR_DE_VERBATIM")] <- tolower("MOT_SEPARATEUR_DE_VERBATIM")

  out_lemme <- str_trim(str_split(paste(out_lemme,collapse = " "), tolower("MOT_SEPARATEUR_DE_VERBATIM"))[[1]])
  # out_lemme <- out_lemme[-length(out_lemme)]
  out_lemme<-stri_replace_all_regex(out_lemme,"_SPECIAL_FORM"," SPECIAL_FORM",case_insensitive=TRUE)

  out_categ <- str_trim(str_split(paste(out_categ,collapse = " "), tolower("MOT_SEPARATEUR_DE_VERBATIM"))[[1]])
  # out_categ <- out_categ[-length(out_categ)]

  return(list(txt_lemme=out_lemme,txt_categ=out_categ,out_in=out_in))

}


#' @export lemmatisation
lemmatisation  <- function(txt, lang="en", mc.cores = 4, path = NULL, remove = NULL) {
  lang <- match.arg(tolower(lang), c("en", "fr","ge","sp","it","pt","ru"))

  if(.Platform[[1]]=="windows" & is.null(path)) {
    path <- "E:/freeling/bin/analyzer.exe"
  }



  library(stringr)
  library(magrittr)
  ## Need Freeling to be installed

  ##### pour la paréllisation, on restreint à 1 coeur pour Windows (limitation du package du package)
  library(parallel)
  if(.Platform[[1]]=="windows") {
    mc.cores <- 1
  }

  ## Méthod pour consever l'information de séparateur de verbatims
  ##### LE CODE FREELING #####################

  # Mapping
  # txt <- lapply(chunk2(txt, mc.cores), function(lis_sent) sapply(lis_sent, function(sent) paste0(sent," MOT_SEPARATEUR_DE_VERBATIM")))
  txt <- lapply(chunk2(txt, mc.cores), function(lis_sent) paste(lis_sent,collapse  = "\nMOT_SEPARATEUR_DE_VERBATIM\n"))


  txt <- mclapply(txt, function(x) sub_lemmatisation(x,lang=lang,path=path,remove=remove), mc.cores = mc.cores)

  # Reducing
  result = list()
  for(e in 1:length(txt)){
    result$txt_lemme <- c(result$txt_lemme, txt[[e]]$txt_lemme)
    result$txt_categ <- c(result$txt_categ, txt[[e]]$txt_categ)
  }

  return(result)
}

# system.time({
#   replicate(20,{
# a<-lemmatisation(rep(c("ceci est un test avec des mots au pluriel et des adjectifs genrées ou des verbes conjugés"),100)
#               ,lang="fr"
#  ,mc.cores = 1,path  = "E:/freeling/bin/analyzer.exe")
#   })
# })
# # 55.96
# # 56.39
# # 60

#
# a<-lemmatisation(rep(c("de la grammatologie et les grammatologiens et ceci est un test avec des mots au pluriel et des adjectifs genrées ou des verbes conjugés"),2)
#                  ,lang="fr"
#                  # ,mc.cores = 1,path  = "E:\\software\\freeling\\freeling-3.1-win\\bin\\mt_analyzer.exe"
#                  ,mc.cores = 1,path  = "E:/freeling/bin/analyzer.exe")
#

# b<-lemmatisation(rep(c("In 2009, yes ! from you to me It's about the greenest and the 2 grammatologie and the grammatologists and this is a test with plurial words and gendered adjectives  or  conjugated verbs")%>%tolower,2)
#                  ,lang="en"
#                  # ,mc.cores = 1,path  = "E:\\software\\freeling\\freeling-3.1-win\\bin\\mt_analyzer.exe"
#                  ,mc.cores = 1,path  = "E:/freeling/bin/analyzer.exe",remove=c("Conjonction", "Déterminant","Pronom","Préposition"))
#

#
# c<-lemmatisation(rep(c("En 2009 _LINK_OR_  www.lemonde.fr ;), oui, hé ! de toi à moi. et les deux maisons"),2)
#               ,lang="fr"
#               # ,mc.cores = 1,path  = "E:\\software\\freeling\\freeling-3.1-win\\bin\\mt_analyzer.exe"
#               ,mc.cores = 1,path  = "E:/freeling/bin/analyzer.exe",remove=c("Conjonction", "Déterminant","Pronom","Préposition"))
# c
