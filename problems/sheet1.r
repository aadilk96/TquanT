# Sheet 1

# b
x = c(1, 5, 8, 3, 7, 2, 6)
y <- seq(1, 13, 2)
z = 4*x + 2*y
print("z:")
print(paste(z))

# c
A <- cbind(x, y, z)
B <- rbind(x, y, z)
C = B%*%A
print("Product B and A")
print(paste(C))

# d
id <- rep(1:60, 1)
type <- factor(rep(1:2, 30), labels=c("old", "new"))
condition <- factor(rep(1:3, 20), labels=c("A", "B", "C"))
dataFrame <- data.frame(id, type, condition)
print(dataFrame)

# e
rt <- rnorm(id, mean = 400, sd = sqrt(625))
dataFrame$ReactionTime = rt
print(dataFrame)
aggr <- aggregate(rt ~ condition, dataFrame, mean)
print(aggr)

