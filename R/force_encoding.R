
## Taux de développement estimé : 90 %
## Next steps : faire des tests dans tous les sens y compris test de performance.

#' @export force_encoding
force_encoding <- make_parallel_func(force_encoding0)
force_encoding0 <- function(txt,target_encoding = "UTF-8")
{
  if(length(txt)==0)return(txt)
  txt <- sapply(txt,function(x){tryCatch({x<-iconv(x,Ruchardet::detectEncoding(x),target_encoding);x},error=function(e){print(e);print(x);x})})
  Encoding(txt) <- target_encoding
  return(txt)
}

