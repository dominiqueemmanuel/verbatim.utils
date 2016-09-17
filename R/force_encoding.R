
## Taux de développement estimé : 90 %
## Next steps : faire des tests dans tous les sens y compris test de performance.

#' @export force_encoding
force_encoding <- make_parallel_func(force_encoding0)





force_encoding0 <- function(txt,target_encoding = "UTF-8",tolower=FALSE,trace=FALSE)
{
  if(length(txt)==0)return(txt)
  if(!tolower){
    txt <- vapply(txt,function(x){tryCatch({x<-iconv(x,Ruchardet::detectEncoding(x),target_encoding);x},error=function(e){if(trace){print(e);print(x)};x}) }, FUN.VALUE = character(1),USE.NAMES = FALSE)
  } else {
    txt <- vapply(txt,function(x){tryCatch({x<-tolower(iconv(x,Ruchardet::detectEncoding(x),target_encoding));x},error=function(e){if(trace){print(e);print(x)};x})}, FUN.VALUE = character(1),USE.NAMES = FALSE)
  }
  Encoding(txt) <- target_encoding
  return(txt)
}



