Pascal_triangle1 <- function(k){
  cc <- matrix(0, nrow = k, ncol = k)
  for (i in 1:k) {
    cc[i,i] = 1
    cc[i,1] = 1
  }
  if(k>=3){
    for (j in 3:k) {
      for (q in 2:(j-1)) {
        cc[j,q] = cc[j-1,q-1] + cc[j-1,q]
        
      }
      
    }
  }
  print(cc[k,])
}

Pascal_triangle1(100)
Pascal_triangle1(200)
