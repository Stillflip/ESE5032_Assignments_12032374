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
 
Pascal_triangle <- function(k){
  b=c()
  for (i in 0:k){
    b[i] =jie_cheng(k-1)/(jie_cheng(i) * jie_cheng(k-i-1))
  }
  b <-c(1,b)
 
  return(b)
}

Pascal_triangle(100)


