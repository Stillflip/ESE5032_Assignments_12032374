Least_moves <- function(x){
  steps=0
  while (x !=1) {
    if(x%%2 == 1){
      x <- x-1
      steps <- steps + 1
      
    }else{
      x <- x/2
      steps <- steps + 1
    }
  }
  print(steps)
}

Least_moves(87)
