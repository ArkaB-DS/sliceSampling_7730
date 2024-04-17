source("slicer.R")
# Standard normal with unnormalized density
set.seed(6)
target = function(x) exp(-x^2/2) # The normalizing factor is 1/sqrt(2pi)
A = function(u,x) {
  x = sqrt(-2*log(u))
  return(c(-x,x))
}

pdf("./Plots/unnormalized_normal.pdf", width = 8, height = 8)
hist(slice(1e4, 0.1, target, A)$x, freq=F, 100,
     xlab= "x", main="Histogram of slice sampling from 
     an unnormalized standard normal sistribution.",
     col = "lightblue")
curve(dnorm, add=TRUE, col = "red")
dev.off()