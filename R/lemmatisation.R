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
    return(x)
}

sub_lemmatisation <- function(txt, lang="fr"){

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
  command <-   paste0("analyze  --output freeling --input text --inplv text -f ",lang,".cfg <",testtxt_in_name," >",testtxt_out_name)

  ## Exécution de la commande
  res <- system(command)

  ## s'il y a eu un problème on renvoi NULL
  if(res!=0)return(NULL)

  ## On lit le fichier en sortie
  out<-readLines(testtxt_out_name,encoding="UTF-8")
  out <- out[-length(out)]
  out <- str_split(out," ")
  out <- out[lapply(out, length) == 4]
  #print(head(out))
  #print(lapply(out, length))
  out_lemme <- sapply(out,function(t)t[[2]])
  out_categ <- sapply(out,function(t)t[[3]])
  out_categ[out_lemme==tolower("MOT_SEPARATEUR_DE_VERBATIM")] <- tolower("MOT_SEPARATEUR_DE_VERBATIM")

  out_lemme <- str_trim(str_split(paste(out_lemme,collapse = " "), tolower("MOT_SEPARATEUR_DE_VERBATIM"))[[1]])
  out_lemme <- out_lemme[-length(out_lemme)]

  out_categ <- str_trim(str_split(paste(out_categ,collapse = " "), tolower("MOT_SEPARATEUR_DE_VERBATIM"))[[1]])
  out_categ <- out_categ[-length(out_categ)]

  return(list(txt_lemme=out_lemme,txt_categ=out_categ))

}


#' @export lemmatisation
lemmatisation  <- function(txt, lang="en", mc.cores = 4) {
  lang <- match.arg(tolower(lang), c("en", "fr"))

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
  txt <- lapply(chunk2(txt, mc.cores), function(lis_sent) sapply(lis_sent, function(sent) paste0(sent," MOT_SEPARATEUR_DE_VERBATIM")))
  txt <- mclapply(txt, function(x) sub_lemmatisation(x,lang=lang), mc.cores = mc.cores)

  # Reducing
  result = list()
  for(e in 1:length(txt)){
    result$txt_lemme <- c(result$txt_lemme, txt[[e]]$txt_lemme)
    result$txt_categ <- c(result$txt_categ, txt[[e]]$txt_categ)
  }

  return(result)
}


#lemmatisation(c("kaeaj lamps kae","aleakk amloer al lamaiosn adar,","alors un deusieme text pour un exemple complet"))
