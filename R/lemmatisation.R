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
lemmatisation  <- function(txt, lang="en") {
  lang <- match.arg(tolower(lang), c("en", "fr"))

  library(magrittr)
  ## Need Freeling to be installed


}
