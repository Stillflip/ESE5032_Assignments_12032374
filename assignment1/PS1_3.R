Pascal_triangle <- function(k){
  b=c()
  for (i in 1:k){
    if(i-1==0){
      b[i] <- 1
      next
    }else{
      numer <- 1
      deno <-1
      n = k-1
      value <-1
      for (j in 1:(i-1)) {
        numer <- numer*n
        n <- n-1
        deno <- deno*j
        value <-(numer/deno)
        
      }
      b[i] =value
    }
  }
  #b <-c(1,b)
  return(b)
}


Pascal_triangle(100)
Pascal_triangle(200)

