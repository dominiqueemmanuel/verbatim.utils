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
spell_correction <- function(txt, lang="en") {
  lang <- match.arg(tolower(lang), c("en", "fr"))
  dict <- paste0(lang,"_",if(lang=="en") "US" else oupper(lang))
  library(magrittr)
  library(stringr)
  library(hunspell)
  # https://github.com/dominiqueemmanuel/hunspell

  ## On s'assure que les apostrphe ne sont pas collées aux mots suivants pour éviter les problème de tokenisation
  txt <- gsub("'|´|’","' ",txt)

  ## Pour les autres signes de ponctuation on rajoute un espace avant et après
  p<-'\\!|\\"|\\$|\\%|\\(|\\)|\\*|\\+|\\,|\\.|\\/|\\:|\\;|\\<|\\=|\\>|\\?|\\[|\\\\|\\]|\\`|\\{|\\||\\}'

  txt <- gsub(paste0("(",p,")")," \\1 ",txt, perl=TRUE)


  ## On créer un séparateur artificiel de verbatim pour pouvoir traiter les verbatims en une fois (=> optimisation des temps de calculs)
  txt <- paste(txt,collapse=" _SEPARATEUR_DE_VERBATIM_ ")
  txt <- gsub(" +"," ",txt)
  txt <- str_split(txt," ")[[1]]
  id<-which(txt!="_SEPARATEUR_DE_VERBATIM_" & nchar(txt)>1)
  x<-txt[id]

  y<-hunspell_check(x,dict=dict)
  e<-hunspell_suggest(x[!y],dict = dict)%>%sapply(function(t)t[1])
  x[!y][!is.na(e)]<-e[!is.na(e)]
  txt[id]<-x
  txt <- paste(txt,collapse=" ")
  txt <- str_trim(str_split(txt,"_SEPARATEUR_DE_VERBATIM_")[[1]])
  return(txt)

}


# spell_correction(c("une Phrase avec une fauute","Une phrase avec un mot inconnu xxsf"), lang="fr")
