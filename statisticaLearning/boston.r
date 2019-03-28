library(MASS)
print(head(Boston))
print(summary(Boston))
attach(Boston)

crime1 = rep(0, length(crim))
crime1[crim > median(crim)] = 1
Boston = data.frame(Boston, crime1)

train = 1:(dim(Boston)[1]/2)
test = (dim(Boston)[1]/2 + 1):dim(Boston)[1]

# splitting dataset into training and test
Boston.train = Boston[train,]
Boston.test = Boston[test,]

crime1.test = crime1[test]

# logistic regression 
glm.fit = glm(crime1 ~ . - crime1 - crim, data = Boston, family = binomial, subset = train)

print(glm.fit)

# glm.probs = predict(glm.fit, Boston.test, type = "response")
# glm.pred = rep(0, length(glm.probs))
# glm.pred[glm.probs > 0.5] = 1
# print(mean(glm.pred != crime1.test))

