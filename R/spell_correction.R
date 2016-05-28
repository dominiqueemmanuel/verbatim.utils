################ Info pour le développement
## Cette fonction prend un vecteur de chaine de caractères en entrée (i.e. un vecteur de verbatims)
## Cette fonction renvoie un vecteur de chaine de caractères de même longueur
## Cette fonction utilisera le package hunspell

## spell_correction(c("une Phrase avec une fauute","Une phrase avec un mot inconnu xxsf"), lang="fr")
## doit renvoyer
## c("une Phrase avec une faute","Une phrase avec un mot inconnu xxsf")

## > Penser au fait que la fonction doit être Cross Platform
## > Penser aux performance (la fonction doit être rapide sur de longs et nombreux verbatims)

## Taux de développement estimé : 85 %
## Next steps : faire des tests dans tous les sens y compris test de performance et cross plat form
####################################################

#' @export spell_correction
spell_correction <- function(txt, lang="en", minimal_occurence = 3, excluded_word = NULL) {
  lang <- match.arg(tolower(lang), c("en", "fr"))
  dict <- paste0(lang,"_",if(lang=="en") "US" else toupper(lang))
  library(magrittr)
  library(stringr)
  library(hunspell)
  library(fastmatch)
  # https://github.com/dominiqueemmanuel/hunspell

  ## On s'assure que les apostrphe ne sont pas collées aux mots suivants pour éviter les problème de tokenisation
  # Cette partie devra être intégré dans une fonction à part (idiomatic_correction)
  if(lang=="fr")txt <- gsub("'|´|’"%>%force_encoding,"' ",txt)
  if(lang=="en"){
    txt <- gsub("'|´|’"%>%force_encoding," '",txt)
    txt <- gsub("'s\\b"," is",txt)
    txt <- gsub("'t\\b"," not",txt)
  }

  ## Pour les autres signes de ponctuation on rajoute un espace avant et après
  p<-'\\!|\\"|\\$|\\%|\\(|\\)|\\*|\\+|\\[+]|\\]|\\,|\\.|\\/|\\:|\\;|\\<|\\=|\\>|\\?|\\\\|\\`|\\{|\\||\\}'%>%force_encoding

  txt <- gsub(paste0("(",p,")")," \\1 ",txt, perl=TRUE)


  ## On créer un séparateur artificiel de verbatim pour pouvoir traiter les verbatims en une fois (=> optimisation des temps de calculs)
  txt <- paste(txt,collapse=" _SEPARATEUR_DE_VERBATIM_ ")
  txt <- gsub(" +"," ",txt)
  txt <- str_split(txt," ")[[1]]
  id<-which(txt!="_SEPARATEUR_DE_VERBATIM_" & nchar(txt)>1)
  x<-txt[id]
  x<-factor(x)
  l<-levels(x)
  n<-table(x)
  n<-names(n[n<=minimal_occurence])
  n<-setdiff(n,excluded_word%>%force_encoding)
  y<-hunspell_check(n,dict=dict)
  q<-!grepl("[^\\'\\-[:^punct:]]"%>%force_encoding,n,perl=TRUE)
  e<-hunspell_suggest(n[!y & q],dict = dict)%>%sapply(function(t)t[1])
  n2<-n
  n2[!y & q][!is.na(e)]<-e[!is.na(e)]
  levels(x)<-plyr::mapvalues(l,n,n2,warn_missing = FALSE)%>%force_encoding
  txt[id]<-as.character(x)
  txt <- paste(txt,collapse=" ")
  txt <- str_trim(str_split(txt,"_SEPARATEUR_DE_VERBATIM_")[[1]])
  return(txt)

}


# spell_correction(c("une Phrase avec une fauute","Une phrase avec un mot inconnu xxsf"), lang="fr")
