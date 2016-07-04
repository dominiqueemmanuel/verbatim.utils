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
word_exclusion <- make_parallel_func(word_exclusion0)


# word_exclusion <- function(txt,...,mc.cores = 4){
#   if(.Platform[[1]]=="windows") {
#     mc.cores <- 1
#   }
#   txt <- chunk2(txt, mc.cores)
#   txt <- unlist(mclapply(txt, function(x) word_exclusion0(x,...), mc.cores = mc.cores))
#   return(txt)
#
# }
word_exclusion0 <- function(txt
                           ,excluded_form= c("email","link",'@user',"hashtag","date","heure","pourcentage","monnaie","number","emoticon","special_caracter","ponctuation")
                           ,replace_excluded_form = TRUE
                           ,excluded_word= NULL
                           ,min_letter = 0)
{
  excluded_form <- sapply(tolower(excluded_form),function(x)match.arg(x,c("email","link","@user","hashtag","date","heure","pourcentage","monnaie","number","emoticon","special_caracter","ponctuation")))
  nom <- names(txt)
  txt <- force_encoding(txt)
  library(magrittr)
  library(stringr)
  library(qdapRegex)
  library(stringi)

  if("email" %in% excluded_form){
    txt <- rm_email(txt, replacement = if(replace_excluded_form) " SPECIAL_FORM_EMAIL " else "")
  }
  if("@user" %in% excluded_form){
    p<-"(?<![@\\w])(@)((([[:alnum:]]|\\.|/|-|_)+)\\b)"
    txt <- stri_replace_all_regex(txt,p,if(replace_excluded_form) " SPECIAL_FORM_USER " else "")

  }
  if("link" %in% excluded_form){
    # txt <- rm_url(txt, replacement = if(replace_excluded_form) " SPECIAL_FORM_LIEN " else "")
    # txt <- rm_twitter_url(txt, replacement = if(replace_excluded_form) " SPECIAL_FORM_LIEN " else "")
    # p<-"(?:(\\b[a-z\\d.-]+://)?[^<>\\s]+|\\b(?:(?:(?:[^\\s!@#$%^&*()_=+[\\\\]{}\\|;:'\",.<>/?]+)\\.)+(?:ac|ad|aero|ae|af|ag|ai|al|am|an|ao|aq|arpa|ar|asia|as|at|au|aw|ax|az|ba|bb|bd|be|bf|bg|bh|biz|bi|bj|bm|bn|bo|br|bs|bt|bv|bw|by|bz|cat|ca|cc|cd|cf|cg|ch|ci|ck|cl|cm|cn|coop|com|co|cr|cu|cv|cx|cy|cz|de|dj|dk|dm|do|dz|ec|edu|ee|eg|er|es|et|eu|fi|fj|fk|fm|fo|fr|ga|gb|gd|ge|gf|gg|gh|gi|gl|gm|gn|gov|gp|gq|gr|gs|gt|gu|gw|gy|hk|hm|hn|hr|ht|hu|id|ie|il|im|info|int|in|io|iq|ir|is|it|je|jm|jobs|jo|jp|ke|kg|kh|ki|km|kn|kp|kr|kw|ky|kz|la|lb|lc|li|lk|lr|ls|lt|lu|lv|ly|ma|mc|md|me|mg|mh|mil|mk|ml|mm|mn|mobi|mo|mp|mq|mr|ms|mt|museum|mu|mv|mw|mx|my|mz|name|na|nc|net|ne|nf|ng|ni|nl|no|np|nr|nu|nz|om|org|pa|pe|pf|pg|ph|pk|pl|pm|pn|pro|pr|ps|pt|pw|py|qa|re|ro|rs|ru|rw|sa|sb|sc|sd|se|sg|sh|si|sj|sk|sl|sm|sn|so|sr|st|su|sv|sy|sz|tc|td|tel|tf|tg|th|tj|tk|tl|tm|tn|to|tp|travel|tr|tt|tv|tw|tz|ua|ug|uk|um|us|uy|uz|va|vc|ve|vg|vi|vn|vu|wf|ws|xn--0zwm56d|xn--11b5bs3a9aj6g|xn--80akhbyknj4f|xn--9t4b11yi5a|xn--deba0ad|xn--g6w251d|xn--hgbk6aj7f53bba|xn--hlcj6aya9esc7a|xn--jxalpdlp|xn--kgbechtv|xn--zckzah|ye|yt|yu|za|zm|zw)|(?:(?:[0-9]|[1-9]\\d|1\\d{2}|2[0-4]\\d|25[0-5])\\.){3}(?:[0-9]|[1-9]\\d|1\\d{2}|2[0-4]\\d|25[0-5]))(?:[;/][^#?<>\\s]*)?(?:\\?[^#<>\\s]*)?(?:#[^<>\\s]*)?(?!\\w))"
    p1<-"(https?://t\\.co[^ ]*)|(t\\.co[^ ]*)|(http[^ ]*)|(ftp[^ ]*)|(www\\.[^ ]*)|(((https?|ftps?)://)|(www\\.))(-\\.)?([^\\s/?\\.#-]+\\.?)+(/[^\\s]*)?|(https?|ftps?)://(-\\.)?([^\\s/?\\.#-]+\\.?)+(/[^\\s]*)?"
    p2<-"(((([[:alnum:]]|\\.|/|-|_){1,}\\.[a-zA-Z]{2,3}(/([[:alnum:]]|\\.|/-|_){1,})?))|(/(([[:alnum:]]|\\.|/-|_){1,}\\.[a-zA-Z]{2,3}(/([[:alnum:]]|\\.|/-|_){1,})?)))"
    p<-paste0(p1,"|",p2)
     txt <- stri_replace_all_regex(txt,p,if(replace_excluded_form) " SPECIAL_FORM_LIEN " else "")

  }

  if("hashtag" %in% excluded_form){
    txt <- rm_hash(txt, replacement = if(replace_excluded_form) " SPECIAL_FORM_HASHTAG " else "")
  }

  if("date" %in% excluded_form){
    txt <- rm_date(txt, replacement = if(replace_excluded_form) " SPECIAL_FORM_DATE " else "")
  }

  if("heure" %in% excluded_form){
    txt <- gsub("\\d{1,2}(( ?)(\\:|h|H)( ?))(\\d{1,2}(?:[:.]\\d+)?)?", if(!replace_excluded_form)  "" else " SPECIAL_FORM_HEURE ", txt ,perl=TRUE)
    # txt <- rm_time(txt, replacement = if(replace_excluded_form) " SPECIAL_FORM_HEURE " else "")
  }

  if("pourcentage" %in% excluded_form){
    txt <- rm_percent(txt, replacement = if(replace_excluded_form) " SPECIAL_FORM_POURCENTAGE " else "")
  }

  if("monnaie" %in% excluded_form){
    txt <- gsub("\\(?[0-9\\.\\,]+\\)?(( ?)(euros|euro|dollars|dollar|pounds|pound|\\€|\\$|£))"%>%force_encoding, if(!replace_excluded_form)  "" else " SPECIAL_FORM_MONNAIE ", txt ,perl=TRUE)
  }

  if("number" %in% excluded_form){
    txt <- gsub("([[:punct:]]|[[:space:]])([[:digit:]]+)([[:punct:]]|[[:space:]])","\\1 \\2 \\3", txt ,perl=TRUE)
    txt <- rm_number(txt, replacement = if(replace_excluded_form) " SPECIAL_FORM_NOMBRE " else "")
  }

  if("emoticon" %in% excluded_form){
    txt <- gsub("(^|[[:blank:]])((>?[:;=8XB]{1}[-~+o^]?[|\")(&gt;DO>{pP3/]+|</?3|XD+|D:<|x[-~+o^]?[|\")(&gt;DO>{pP3/]+))+([[:blank:]]|$)", if(replace_excluded_form) " SPECIAL_FORM_EMOTICON " else " ",txt,perl=TRUE)
    # txt <- rm_emoticon(txt, replacement = if(replace_excluded_form) " SPECIAL_FORM_EMOTICON " else " ")
  }

  # if("special_caracter" %in% excluded_form){
  #   txt <- gsub("[^/\\.\\,\\;\\?\\;\\:\\!\\{\\}<>\\[\\]\\(\\)\\–\\'\\\"\\-\\_´’[:^punct:]]\\\\"%>%force_encoding,if(!replace_excluded_form)  "" else " SPECIAL_FORM_CARACTÈRE_SPÉCIAL ", txt, perl=TRUE)
  # }
  #
  #
  # if("ponctuation" %in% excluded_form){
  #   txt <- gsub("[\\.\\,\\;\\?\\;\\:\\!]+"%>%force_encoding,if(!replace_excluded_form)  "" else " SPECIAL_FORM_PONCTUATION ", txt, perl=TRUE)
  #   txt <- gsub("[/\\\\\\{\\}<>\\[\\]\\(\\)\\–]+"%>%force_encoding,if(!replace_excluded_form)  "" else " SPECIAL_FORM_PONCTUATION_SIMPLE ", txt, perl=TRUE)
  # }
  if("special_caracter" %in% excluded_form){
    txt <- gsub("[^\\.\\,\\;\\?\\;\\:\\!\\<\\>\\[\\]\\{\\}\\(\\)\\–\\'\\\"\\-\\_´’[:^punct:]]"%>%force_encoding,if(!replace_excluded_form)  "" else " SPECIAL_FORM_CARACTÈRE_SPÉCIAL ", txt, perl=TRUE)
  }

  if("ponctuation" %in% excluded_form){
      txt <- gsub("[\\?\\:\\!\\.]+"%>%force_encoding,if(!replace_excluded_form)  "" else " SPECIAL_FORM_PONCTUATION_PHRASE ", txt, perl=TRUE)
      txt <- gsub("[\\,\\;\\;\\<\\>\\[\\]\\{\\}\\(\\)\\–]+"%>%force_encoding,if(!replace_excluded_form)  "" else " SPECIAL_FORM_PONCTUATION_SIMPLE ", txt, perl=TRUE)
  }

  # txt <-stri_replace_all_regex(txt,"\\\\|/"," ")
    txt <- remove_small_word(txt,min_letter)
  txt <- stri_replace_all_regex(txt,"[[:space:]]+"," ")



  ## Les _ qui ne sont pas d'une forme créé dans cette fonction sont supprimé
  id<-which(!is.na(txt))
  txt[id]<-remove_underscore(txt[id])



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
#        ,"une phrase aza_ | aa / bb et \ cc avec un (et une parenthèse) et <br>  et [jj] et {kkj} mot i de moins de une lettre !! et !"
# )
# word_exclusion(txt,min_letter=1)

# txt <- force_encoding(movie_review$review)
# ##  appliquer listing suppression/remplacement (avec ignore.case)
# txt <- verbatim_exclusion(txt,min_word = 0, treatment = "empty")
# txt <- idiomatic_correction(tolower(txt), lang = lang)
# txt <- word_exclusion(txt,min_letter=1)
# sw<-tm::stopwords(kind=lang)
#
# tab<-data.frame(
#   # ID=as.integer(seq_along(sw)),
#   `Mot/N-gram`=sw
#   # ,`Action`="Suppression"
#   ,`Mot/N-gram de remplacement`=""
#   ,stringsAsFactors = FALSE
#   ,check.names = FALSE)
#
# for(k in seq_along(tab[,1])){cat(".")
#   txt <- stri_replace_all_regex(txt,paste0("\\b",str_trim(tab[k,"Mot/N-gram"]),"\\b")
#                                 ,str_trim(tab[k,"Mot/N-gram de remplacement"])
#                                 ,case_insensitive=TRUE)
# }
# #
# txt<-gsub("_CARACTÈRE_SPÉCIAL_"," ",txt,fixed=TRUE)
# txt<-gsub("[[:space:]]+"," ",txt)
#



#' @export remove_underscore
remove_underscore <- function(txt,f = function(x)x){
  p1<-"\\b(\\_?([[:alnum:]]|\\_){0,})\\_(([[:alnum:]]|\\_){0,}\\_?)\\b"
  p2<-"^special_form_([[:alnum:]]|\\_)"
  library(gsubfn)

  gsubfn(p1
                  ,function(x,...){if(grepl(p2,x,ignore.case=TRUE)) f(x) else stri_replace_all_fixed(x,"_"," ")}
                  ,x=txt
                  ,backref = 1
                  ,perl=FALSE
                  ,engine="R"
  )

}

#' @export remove_small_word
remove_small_word <- function(txt,min_letter=0){
  library(magrittr)
  library(stringr)
  library(qdapRegex)
  library(stringi)
  if(min_letter>0){
    p<-"(?<![\\w'])(?:'?\\w'?){1}(?![\\w'])"
    txt <- stri_replace_all_regex(txt,p,"")
  }
  return(txt)
}
