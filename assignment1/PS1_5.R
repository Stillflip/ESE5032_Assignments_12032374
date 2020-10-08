Find_expression <- function(x){
  list(1,2,3,4,5,6,7,8,9,'+','-')
  
  
  
}

original=c(1,2,3,4,5,6,7,8,9)


print(original[1:9])
number_rank=permutations(9,9,original[1:9],set=TRUE, repeats.allowed=FALSE)
write.csv(number_rank,file = 'number.csv')

ori1=c(original,'+','-')
print(ori1)
all_rank=permutations(11,11,ori1[1:11],set=TRUE, repeats.allowed=FALSE)

a1= seq(1,9,by=1)


