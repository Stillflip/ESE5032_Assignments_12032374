"
在1-9之间增加空格，可有8个空格。然后将'+','-','',这三个元素进行可重复的排列组合，
每次组成一个1*8的向量，然后将该向量的元素分别带入8个空格当作。
再使用，paste函数将字符串合并为一个字符串，使用eval函数完成字符串
里面的数值运算，判断是否是想要的数，如成立则打印出并计数+1
"
Find_expression <- function(x){
  a <-c(1,'',2,'',3,'',4,'',5,'',6,'',7,'',8,'',9)
  b <- c('+','-','')
  c <-permutations(3,8,b,set=TRUE, repeats.allowed=TRUE)
  len_c <-length(c)/8
  num <-0
  for (i in 1:len_c) {
    for (j in 1:8) {
      a[2*j] <- c[i,j]
    }
    a1=paste(a,collapse = "")
    value <- eval(parse(text=a1))
    if(value == x){
      val <-sprintf('=%d',x)
      a2 <- c(a1,val)
      a2 <- paste(a2, collapse = "")
      print(a2)
      num <- num+1
    }
  }
  return(num)
}
Find_expression(10)
number1 <- Find_expression(10)
# 5.2
Total_solutions <-c()

for(x in 1:100){
  Total_solutions[x] <- Find_expression(x)
}
print(Total_solutions)

plot(c(1:100),Total_solutions,main='Find_expression(x)',type='h',xlab = 'number',ylab = 'quantity',col = 'blue')

max(Total_solutions)
num_max =which(Total_solutions == max(Total_solutions))
print(num_max)

min(Total_solutions)
num_min =which(Total_solutions == min(Total_solutions))
print(num_min)