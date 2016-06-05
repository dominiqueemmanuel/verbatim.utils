################ Info pour le développement
## Cette fonction prend un vecteur de chaine de caractères en entrée (i.e. un vecteur de verbatims)
## Cette fonction renvoie un vecteur de chaine de caractères de même longueur (si treatment = "na") ou de longueure plus petite (si treatment ="remove")
## Cette fonction pourra utiliser le package stringr et/ou stringi

## verbatim_exclusion(c("une première phrase","Seconde phrase"),min_word=2)
## doit renvoyer
## c("une première phrase",NA)
## verbatim_exclusion(c("une première phrase","Seconde phrase"),min_word=2, treatment="remove")
## doit renvoyer
## c("une première phrase")

## > Penser au fait que la fonction doit être Cross Platform
## > Penser aux performance (la fonction doit être rapide sur de longs et nombreux verbatims)

## Taux de développement estimé : 85 %
## Next steps : faire des tests dans tous les sens y compris test de performance et cross plat form
####################################################

#' @export verbatim_exclusion
verbatim_exclusion <- function(txt, min_word = 0, treatment = "empty") {
  treatment <- match.arg(tolower(treatment), c("na", "remove","empty"))
  library(tokenizers)
  # https://github.com/dominiqueemmanuel/tokenizers

  l <- tokenize_words(txt)
  id <- which(sapply(l, function(x) length(x) <= min_word))
  txt[id] <- NA
  if (treatment == "remove")
    txt <- na.omit(txt)

  if(treatment == "empty")
    txt <- ifelse(is.na(txt), "", txt)

  return(txt)
}

# verbatim_exclusion(c("une première phrase","Seconde phrase"),min_word=2)
# verbatim_exclusion(c("une première phrase","Seconde phrase"),min_word=3)
# verbatim_exclusion(c("une première phrase","Seconde phrase"),min_word=2,treatment = "remove")
