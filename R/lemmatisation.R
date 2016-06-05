################ Info pour le développement
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


#' @export lemmatisation
lemmatisation  <- function(txt, lang="en", mc.cores = 4) {
  lang <- match.arg(tolower(lang), c("en", "fr"))

  library(magrittr)
  ## Need Freeling to be installed

  ##### pour la paréllisation, on restreint à 1 coeur pour Windows (limitation du package du package)
  library(parallel)
  if(.Platform[[1]]=="windows") {
    mc.cores <- 1
  }



  ## Méthod pour consever l'information de séparateur de verbatims
  txt <- paste0(txt,collapse=" _MOT_SEPARATEUR_DE_VERBATIM_ ")



  ##### LE CODE FREELING #####################
  txt<-str_split(txt," ")%>%unlist
  txt <- paste(txt,ifelse(txt=="_MOT_SEPARATEUR_DE_VERBATIM_",txt,substr(txt,1,5)),"NCFS000",runif(length(txt)))
  ##### FIN LE CODE FREELING ###########################




  txt <- str_split(txt," ")
  txt_lemme <- sapply(txt,function(t)t[[2]])
  txt_categ <- sapply(txt,function(t)t[[3]])
  txt_categ[txt_lemme=="_MOT_SEPARATEUR_DE_VERBATIM_"] <- "_MOT_SEPARATEUR_DE_VERBATIM_"

  txt_lemme <- str_trim(str_split(paste(txt_lemme,collapse = " "),"_MOT_SEPARATEUR_DE_VERBATIM_")[[1]])
  txt_categ <- str_trim(str_split(paste(txt_categ,collapse = " "),"_MOT_SEPARATEUR_DE_VERBATIM_")[[1]])


  return(list(txt_lemme=txt_lemme,txt_categ=txt_categ))


}
