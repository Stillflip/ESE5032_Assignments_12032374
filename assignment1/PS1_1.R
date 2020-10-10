#判断语句和比较

Print_values <- function(a,b,c){
  if(a>b){
    if(b>c){
      print(c(a,b,c))
    }else{
      if(a>c){
        print(c(a,c,b))
        
      }else{
        print(c(c,a,b))
      }
    }
  }else{
    if(b>c){
      print('can not judge')
    }else{
      print(c(c,b,a))
    }
  }

print('Stop')
}

Print_values(2,4,1)
