aa <- sample(0:50,50)
print(aa) 
M1 <- matrix(data=aa, nrow = 5, ncol = 10)
print(M1)
M2 <- matrix(data=aa, nrow = 10, ncol = 5)
print(M2)
print(nrow(M1))


Matrix_multip <- function(M1,M2){
  M=matrix(0,nrow = nrow(M1) , ncol = ncol(M2))
  for(j in 1:nrow(M1)){
    for (s in 1:nrow(M1)) {
      for (t in 1:ncol(M1)) {
         M[j,s] <- M[j,s] + M1[j,t]*M2[t,s] 
      }

    }
  }
  print(M)
}

Matrix_multip(M1,M2)
M1%*%M2
