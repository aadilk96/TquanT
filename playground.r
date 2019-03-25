# take input from the user
num = as.integer(readline(prompt="Enter a number: "))
factorial = 1
if (num < 0){
  print(paste(0))
}else if (num == 1){
  print(paste(1))
}else{
  for (i in 1:num){
    factorial = factorial * i 
  }
  print(paste(factorial))
}
