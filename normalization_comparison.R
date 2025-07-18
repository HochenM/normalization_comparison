library(MASS)

# Generate sample data
x = rchisq(1000, 5)

# Normalization Functions
myf1 = function(x){
  return( (x - mean(x)) / sd(x) )
}

myf2 = function(x){
  return(log(x))
}

myf3 = function(x){
  result = boxcox(lm(x ~ 1), plotit = FALSE)
  lambda = result$x[which.max(result$y)]
  y = (x^lambda - 1) / lambda
  return(y)
}

# Apply transformations
norm1 = myf1(x)
norm2 = myf2(x)
norm3 = myf3(x)

# Plot Histograms
par(mfrow=c(1,3))
hist(norm1, col = "red", main = "Standard Normalization")
hist(norm2, col = "blue", main = "Log Transformation")
hist(norm3, col = "black", main = "Box-Cox Transformation")

# Plot QQ-plots
par(mfrow = c(1, 3))
qqnorm(norm1, main = "Standard Normalization", col = "red")
qqline(norm1, col = "blue")

qqnorm(norm2, main = "Log Transformation", col = "darkgreen")
qqline(norm2, col = "blue")

qqnorm(norm3, main = "Box-Cox Transformation", col = "purple")
qqline(norm3, col = "blue")
