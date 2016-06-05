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
spell_correction <- function(txt, lang="en"
                             , minimal_occurence = 3
                             , excluded_word = NULL
                             , ingore_cap_word = TRUE) {
  lang <- match.arg(tolower(lang), c("en", "fr"))
  dict <- paste0(lang,"_",if(lang=="en") "US" else toupper(lang))
  library(magrittr)
  library(stringr)
  library(hunspell)
  library(fastmatch)
  # https://github.com/dominiqueemmanuel/hunspell

  ## Pour les autres signes de ponctuation on rajoute un espace avant et après
  p<-'\\!|\\"|\\$|\\%|\\(|\\)|\\*|\\+|\\[+]|\\]|\\,|\\.|\\/|\\:|\\;|\\<|\\=|\\>|\\?|\\\\|\\`|\\{|\\||\\}'%>%force_encoding
  txt <- gsub(paste0("(",p,")")," \\1 ",txt, perl=TRUE)

  # Les premiers mots de phrases sont mis en minuscules pour ne pas intégréer le cas des majsucules en début de phrases
  if(ingore_cap_word){
    txt<-gsubfn::gsubfn("(?:^|(?:[:.!?]\\s))+(\\w+)",replacement=function(x,y,z)paste0(tolower(x),z,collapse=""),backref=2,x=txt)
  }

  ## On créer un séparateur artificiel de verbatim pour pouvoir traiter les verbatims en une fois (=> optimisation des temps de calculs)
  txt <- paste(txt,collapse=" _SEPARATEUR_DE_VERBATIM_ ")
  txt <- gsub(" +"," ",txt)
  txt <- str_split(txt," ")[[1]]


  #on prend chaque mot hors séparateur artificiel
  id<-which(txt!="_SEPARATEUR_DE_VERBATIM_" & nchar(txt)>1)
  x<-txt[id]

  # le fait de travailler sur les levels des facteurs vas accélérer les traitementes (éviter les doublons d'analyses)
  x<-factor(x)
  l<-levels(x)

  # On ne travaille que sur les mots aillant au plus minimal_occurence occurence
  n<-table(x)
  n<-names(n[n<=minimal_occurence])

  # On retire les mots à ne pas analyser (excluded_word)
  # n<-setdiff(n,excluded_word%>%force_encoding)
  n2<-as.vector(sapply(excluded_word,function(t)which(grepl(paste0("\\b",t,"\\b"),n,ignore.case = TRUE))))
  if(length(n2)>0)n<-n[-n2]

  # On retire les mots qui ont une majuscule
  if(ingore_cap_word){
    n <- n[!grepl("[A-Z]",n)]
  }

  y<-hunspell_check(n,dict=dict)
  q<-!grepl("[^\\'\\-[:^punct:]]"%>%force_encoding,n,perl=TRUE)

  # R2fléchir à une version parralélisée (mcapply)
  e<-hunspell_suggest(n[!y & q],dict = dict)%>%sapply(function(t)t[1])
  n2<-n
  n2[!y & q][!is.na(e)]<-e[!is.na(e)]
  levels(x)<-plyr::mapvalues(l,n,n2,warn_missing = FALSE)%>%force_encoding
  txt[id]<-as.character(x)
  txt <- paste(txt,collapse=" ")
  txt <- str_trim(str_split(txt,"_SEPARATEUR_DE_VERBATIM_")[[1]])
  return(txt)

}


# spell_correction(c("une Phrase avec une fauute et une autre Fauute","Une phrase avec un mot inconnu xxsf"), lang="fr")
