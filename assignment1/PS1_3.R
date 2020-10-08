jie_cheng <- function(n){
  a <-1
  if (n == 0){
    return(a)
  }
  for(i in 1:n){
    a <- a*i
  }
  return(a)
}

jie_cheng(4)

jie_cheng(200)
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


Pascal_triangle(13)
Pascal_triangle(200)

j=2
for (s in 1:(j-1)){
    print('n')
}
