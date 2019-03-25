# Sheet 2

# a
vecX <- 0
for (i in 1:8){
    vecX[i] <- i * 2 + 1
}
# print(vecX)

# b 
myFunc <- function(num){
    if (num < 0){
        result <- -1
    }else if (num == 0){
        result <- 0 
    }
    else{
        result <- 1
    }
    return(result)
}
# print(myFunc(-10))

# c
x <- vector()
x <- c(x, 1:20)
y <- vector()
for (i in 1:20){
    if(i == 1){
        y[i] <- x[i]
    }
    else{
        y[i] <- x[i] + y[i - 1]
    }
}
# print(y)

(m2 <- matrix(1:20, 5))
lower.tri(m2)

# print(m2)

# d
Lin <- function(set){
    linearMod <- lm(dist ~ speed, data=set) 
    print(linearMod)
    print(summary(linearMod))
}

Lin(cars)
