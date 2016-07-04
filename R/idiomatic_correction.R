#' @export idiomatic_correction
idiomatic_correction <- make_parallel_func(idiomatic_correction0)
idiomatic_correction0 <- function(txt, lang="en"){
  library(stringi)
  #On corrige les appostrphes
  txt <- stri_replace_all_regex(txt,"'|´|’"%>%force_encoding,"'")

  # on corrige les guillements
  txt <- stri_replace_all_regex(txt,force_encoding('"“|„')," ")

  # on corrige les ...
  txt <- stri_replace_all_regex(txt,force_encoding('…')," ... ")

  # on corrige les espacces vides
  txt <- stri_replace_all_regex(txt,force_encoding('\u200B')," ")

  # on corrige les espacces e dans l'o
  txt <-stri_replace_all_regex(txt,force_encoding("œ"),"oe")

  if(lang=="fr"){
  txt <-stri_replace_all_regex(txt,"\\bj ?'"%>%force_encoding,"je ")
  txt <- stri_replace_all_regex(txt,"\\bl ?'"%>%force_encoding,"le ")
  txt <- stri_replace_all_regex(txt,"\\bc ?'"%>%force_encoding,"ce ")
  txt <- stri_replace_all_regex(txt,"\\bm ?'"%>%force_encoding,"me ")
  txt <- stri_replace_all_regex(txt,"qu ?'"%>%force_encoding,"que ")
  txt <- stri_replace_all_regex(txt,"\\bd ?'"%>%force_encoding,"de ")
  txt <- stri_replace_all_regex(txt,"\\bn ?'"%>%force_encoding,"ne ")
  txt <- stri_replace_all_regex(txt,"\\bs ?' ?il\\b"%>%force_encoding,"si il")
  txt <- stri_replace_all_regex(txt,"\\bs ?'"%>%force_encoding,"se  ")
  txt <- stri_replace_all_regex(txt,"\\bt ?'"%>%force_encoding,"te  ")
  txt <- stri_replace_all_regex(txt,"- ?vous\\b"%>%force_encoding," vous ")
  txt <- stri_replace_all_regex(txt,"- ?tu\\b"%>%force_encoding," tu ")

}

if(lang=="en"){

  txt <-stri_replace_all_regex(txt,"\\bi ?' ?m\\b"%>%force_encoding,"i am")
  txt <-stri_replace_all_regex(txt,"\\bdon ?'? ?t"%>%force_encoding,"do not")
  txt <-stri_replace_all_regex(txt,"\\bdoesn ?'? ?t\\b"%>%force_encoding,"do not")
  txt <-stri_replace_all_regex(txt,"\\bdidn ?'? ?t\\b"%>%force_encoding,"did not")
  txt <-stri_replace_all_regex(txt,"\\bwouldn ?'? ?t\\b"%>%force_encoding,"would not")
  txt <-stri_replace_all_regex(txt,"\\bhaven ?'? ?t\\b"%>%force_encoding,"have not")
  txt <-stri_replace_all_regex(txt,"\\bwont ?'? ?t\\b"%>%force_encoding,"will not")
  txt <-stri_replace_all_regex(txt,"\\bisn ?'? ?t\\b"%>%force_encoding,"is not")
  txt <-stri_replace_all_regex(txt,"\\baren ?'? ?t\\b"%>%force_encoding,"are not")
  txt <-stri_replace_all_regex(txt,"\\bmustn ?'? ?t\\b"%>%force_encoding,"must not")
  txt <-stri_replace_all_regex(txt,"\\bcloudn ?'? ?t\\b"%>%force_encoding,"could not")
  txt <-stri_replace_all_regex(txt,"\\bcan ?'? ?t\\b"%>%force_encoding,"can not")
  txt <-stri_replace_all_regex(txt,"\\bcannot\\b"%>%force_encoding,"can not")
  txt <-stri_replace_all_regex(txt,"\\bshouldn ?'? ?t\\b"%>%force_encoding,"should not")

  txt <- stri_replace_all_regex(txt,"' ?s\\b"," is")
  txt <-stri_replace_all_regex(txt,"' ?ve\\b"%>%force_encoding," have")
  txt <-stri_replace_all_regex(txt,"' ?re\\b"%>%force_encoding,"are")



  txt <-stri_replace_all_regex(txt,"' ?ll\\b"%>%force_encoding," will ")
  txt <-stri_replace_all_regex(txt,"' ?d\\b"%>%force_encoding," had ")
  txt <- stri_replace_all_regex(txt,"' ?t\\b"," not")

}

  p<-'\\!|\\"|\\$|\\%|\\(|\\)|\\*|\\+|\\[+]|\\]|\\,|\\.|\\/|\\:|\\;|\\<|\\=|\\>|\\?|\\\\|\\`|\\{|\\||\\}'%>%force_encoding
  txt <- gsub(paste0("(",p,")")," \\1 ",txt, perl=TRUE)

  return(txt)
}
