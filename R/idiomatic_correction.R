#' @export idiomatic_correction
idiomatic_correction <- function(txt, lang="en"){
  #On corrige les appostrphes
  txt <- gsub("'|´|’"%>%force_encoding,"'",txt)

  # on corrige les guillements
  txt <- gsub(force_encoding('“|„')," ",txt)

  # on corrige les ...
  txt <- gsub(force_encoding('…')," ... ",txt)

  # on corrige les espacces vides
  txt <- gsub(force_encoding('\u200B')," ",txt)

  # on corrige les espacces e dans l'o
  txt <-gsub(force_encoding("œ"),"oe",txt)

  if(lang=="fr"){
  txt <-gsub("\\bj ?'"%>%force_encoding,"je ",txt)
  txt <- gsub("\\bl ?'"%>%force_encoding,"le ",txt)
  txt <- gsub("\\bc ?'"%>%force_encoding,"ce ",txt)
  txt <- gsub("\\bm ?'"%>%force_encoding,"me ",txt)
  txt <- gsub("qu ?'"%>%force_encoding,"que ",txt)
  txt <- gsub("\\bd ?'"%>%force_encoding,"de ",txt)
  txt <- gsub("\\bn ?'"%>%force_encoding,"ne ",txt)
  txt <- gsub("\\bs ?' ?il\\b"%>%force_encoding,"si il",txt)
  txt <- gsub("\\bs ?'"%>%force_encoding,"se  ",txt)
  txt <- gsub("\\bt ?'"%>%force_encoding,"te  ",txt)
  txt <- gsub("- ?vous\\b"%>%force_encoding," vous ",txt)
  txt <- gsub("- ?tu\\b"%>%force_encoding," tu ",txt)

}

if(lang=="en"){

  txt <-gsub("\\bi ?' ?m\\b"%>%force_encoding,"i am",txt)
  txt <-gsub("\\bdon ?'? ?t"%>%force_encoding,"do not",txt)
  txt <-gsub("\\bdoesn ?'? ?t\\b"%>%force_encoding,"do not",txt)
  txt <-gsub("\\bdidn ?'? ?t\\b"%>%force_encoding,"did not",txt)
  txt <-gsub("\\bwouldn ?'? ?t\\b"%>%force_encoding,"would not",txt)
  txt <-gsub("\\bhaven ?'? ?t\\b"%>%force_encoding,"have not",txt)
  txt <-gsub("\\bwont ?'? ?t\\b"%>%force_encoding,"will not",txt)
  txt <-gsub("\\bisn ?'? ?t\\b"%>%force_encoding,"is not",txt)
  txt <-gsub("\\baren ?'? ?t\\b"%>%force_encoding,"are not",txt)
  txt <-gsub("\\bmustn ?'? ?t\\b"%>%force_encoding,"must not",txt)
  txt <-gsub("\\bcloudn ?'? ?t\\b"%>%force_encoding,"could not",txt)
  txt <-gsub("\\bcan ?'? ?t\\b"%>%force_encoding,"can not",txt)
  txt <-gsub("\\bcannot\\b"%>%force_encoding,"can not",txt)
  txt <-gsub("\\bshouldn ?'? ?t\\b"%>%force_encoding,"should not",txt)

  txt <- gsub("' ?s\\b"," is",txt)
  txt <-gsub("' ?ve\\b"%>%force_encoding," have",txt)
  txt <-gsub("' ?re\\b"%>%force_encoding,"are",txt)



  txt <-gsub("' ?ll\\b"%>%force_encoding," will ",txt)
  txt <-gsub("' ?d\\b"%>%force_encoding," had ",txt)
  txt <- gsub("' ?t\\b"," not",txt)

}

  p<-'\\!|\\"|\\$|\\%|\\(|\\)|\\*|\\+|\\[+]|\\]|\\,|\\.|\\/|\\:|\\;|\\<|\\=|\\>|\\?|\\\\|\\`|\\{|\\||\\}'%>%force_encoding
  txt <- gsub(paste0("(",p,")")," \\1 ",txt, perl=TRUE)

  return(txt)
}
