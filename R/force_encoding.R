
## Taux de développement estimé : 90 %
## Next steps : faire des tests dans tous les sens y compris test de performance.

#' @export force_encoding
force_encoding <- function(txt,target_encoding = "UTF-8")
{
  txt <- sapply(txt,function(x){tryCatch({x<-iconv(x,Ruchardet::detectEncoding(x),target_encoding);x},error=function(e){warning(e);x})})
  Encoding(txt) <- target_encoding
  return(txt)
}

