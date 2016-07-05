make_parallel_func <- function(fun){
  # eval(quote({
  if(.Platform[[1]]=="windows") {
    function(txt,...,mc.cores = 4){
      # n<-names(txt)
      # txt <- chunk2(txt, mc.cores)
      # txt <- unlist(parallelsugar::mclapply_socket(txt, function(x) fun(x,...), mc.cores = mc.cores))
      # names(txt) <- n
      # return(txt)
      return(fun(txt,...))

    }
  } else {

    function(txt,...,mc.cores = 4){
      n<-names(txt)
      txt <- chunk2(txt, mc.cores)
      txt <- unlist(parallel::mclapply(txt, function(x) fun(x,...), mc.cores = mc.cores))
      names(txt) <- n
      return(txt)

    }
  }
# }))
}

my_lapply <- function(x,fun, ... ,mc.cores = 4){
  # eval(quote({
  if(.Platform[[1]]=="windows") {
    n<-names(x)
    res <-lapply(x, fun, ... )
    names(res) <- n
    return(res)
  } else {
    n<-names(x)
    res <-parallel::mclapply(x, fun, ... , mc.cores = mc.cores)
    names(res) <- n
    return(res)
  }
  # }))
}
