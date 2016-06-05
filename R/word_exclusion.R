################ Info pour le développement
## Cette fonction prend un vecteur de chaine de caractères en entrée (i.e. un vecteur de verbatims)
## Cette fonction renvoie un vecteur de chaine de caractères de même longueur (si treatment = "NA") ou de longueure plus petite (si treatment ="REMOVE")
## Cette fonction pourra utiliser les packages stringr, qdapRegex

## doit renvoyer
## c("un Phrase avec un forme féminin et un verbe conjuguer","Un phrase avec un mot inconnu xxsf")

## special_form peut prendre une ou plusieurs valeurs parmis : c("email","link","special_character","number","emoticon","punctuation")
## > Penser au fait que la fonction doit être Cross Platform
## > Penser aux performance (la fonction doit être rapide sur de longs et nombreux verbatims)
## > le paramètre excluded_word n'est pas encore utilisé.

## Taux de développement estimé : 75 %
## Next steps : faire des tests dans tous les sens y compris test de performance.


#' @export word_exclusion
word_exclusion <- function(txt
                           ,excluded_form= c("email","link","hashtag","date","heure","pourcentage","monnaie","number","emoticon","special_caracter","ponctuation")
                           ,replace_excluded_form = TRUE
                           ,excluded_word= NULL
                           ,min_letter = 0)
{
  excluded_form <- sapply(tolower(excluded_form),function(x)match.arg(x,c("email","link","hashtag","date","heure","pourcentage","monnaie","number","emoticon","special_caracter","ponctuation")))
  nom <- names(txt)
  txt <- force_encoding(txt)
  library(magrittr)
  library(stringr)
  library(qdapRegex)

  if("email" %in% excluded_form){
    txt <- rm_email(txt, replacement = if(replace_excluded_form) " _EMAIL_ " else "")
  }

  if("link" %in% excluded_form){
    txt <- rm_url(txt, replacement = if(replace_excluded_form) " _LIEN_ " else "")
    txt <- rm_twitter_url(txt, replacement = if(replace_excluded_form) " _LIEN_ " else "")
  }

  if("hashtag" %in% excluded_form){
    txt <- rm_hash(txt, replacement = if(replace_excluded_form) " _HASHTAG_ " else "")
  }

  if("date" %in% excluded_form){
    txt <- rm_date(txt, replacement = if(replace_excluded_form) " _DATE_ " else "")
  }

  if("heure" %in% excluded_form){
    txt <- gsub("\\d{1,2}(( ?)(\\:|h|H)( ?))(\\d{1,2}(?:[:.]\\d+)?)?", if(!replace_excluded_form)  "" else " _HEURE_ ", txt ,perl=TRUE)
    # txt <- rm_time(txt, replacement = if(replace_excluded_form) " _HEURE_ " else "")
  }

  if("pourcentage" %in% excluded_form){
    txt <- rm_percent(txt, replacement = if(replace_excluded_form) " _POURCENTAGE_ " else "")
  }

  if("monnaie" %in% excluded_form){
    txt <- gsub("\\(?[0-9\\.\\,]+\\)?(( ?)(euros|euro|dollars|dollar|pounds|pound|\\€|\\$|£))"%>%force_encoding, if(!replace_excluded_form)  "" else " _MONNAIE_ ", txt ,perl=TRUE)
  }

  if("number" %in% excluded_form){
    txt <- gsub("([[:punct:]]|[[:space:]])([[:digit:]]+)([[:punct:]]|[[:space:]])","\\1 \\2 \\3", txt ,perl=TRUE)
    txt <- rm_number(txt, replacement = if(replace_excluded_form) " _NOMBRE_ " else "")
  }

  if("emoticon" %in% excluded_form){
    txt <- gsub("(^|[[:blank:]])((>?[:;=8XB]{1}[-~+o^]?[|\")(&gt;DO>{pP3/]+|</?3|XD+|D:<|x[-~+o^]?[|\")(&gt;DO>{pP3/]+))+([[:blank:]]|$)", if(replace_excluded_form) " _EMOTICON_ " else " ",txt,perl=TRUE)
    # txt <- rm_emoticon(txt, replacement = if(replace_excluded_form) " _EMOTICON_ " else " ")
  }

  if("special_caracter" %in% excluded_form){
    txt <- gsub("[^\\,\\;\\?\\;\\:\\!\\'\\\"\\-\\_´’[:^punct:]]"%>%force_encoding,if(!replace_excluded_form)  "" else " _CARACTÈRE_SPÉCIAL_ ", txt, perl=TRUE)
  }

  if("ponctuation" %in% excluded_form){
    txt <- gsub("[\\,\\;\\?\\;\\:\\!]+",if(!replace_excluded_form)  "" else " _PONCTUATION_ ", txt, perl=TRUE)
  }

  if(min_letter>0){
    txt <- rm_nchar_words(txt, n=min_letter)
  }
  txt <- gsub("[[:space:]]+"," ",txt)



  ## Les _ qui ne sont pas d'une forme créé dans cette fonction sont supprimé
  p1<-"\\b(\\_?[[:alnum:]]{0,})\\_([[:alnum:]]{0,}\\_?)\\b"
  p2<-"(\\b((\\_)(([[:upper:]]|\\_){1,})(\\_))\\b)"
  library(gsubfn)
  id<-which(!is.na(txt))
  txt[id]<-gsubfn(p1
                  ,function(x,...)if(grepl(p2,x)) x else " "
                  ,x=txt[id]
                  ,backref = 1
                  ,perl=FALSE
                  ,engine="R"
  )



names(txt) <- nom
return(txt)
}



# txt<-c("une première phrase","Seconde phrase"
#        ,"une phrase avec un email adresse@email invalide et un email valide adresse@email.com"
#        ,"une phrase avec un hashtag #bonjour"
#        ,"une phrase avec un lien https://lien.internet.com"
#        ,"une phrase avec deux nombres un en lettre et un en chiffres : 31"
#        ,"une phrase avec des caractère spéciaux µ & ² # < et pas spéciaux 10$, 10 €, 10 dollars, 10 euros, 10 pound, 10%, 10£,10H10 10,50 €"
#        ,"lundi 26/03/2015 entre 10:10 et 10H20"
#        ,"haha ;) et :) et :("
#        ,"une phrase avec un mot i de moins de une lettre !! et !"
# )
#
# word_exclusion(txt,min_letter = 1)
