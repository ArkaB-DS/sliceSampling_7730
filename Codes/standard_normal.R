source("slicer.R")
A = function(u,xx) {
  c(uniroot(function(x) dnorm(x)-u, c(-10^10,xx))$root, 
    uniroot(function(x) dnorm(x)-u, c(xx, 10^10))$root)
}

set.seed(6)
res = slice(10, 0.1, dnorm, A)
x = res$x
u = res$u

pdf("./Plots/standard_normal.pdf", width = 8, height = 8)
hist(slice(1e4, 0.1, dnorm, A)$x, freq=F, 100,
     xlab="x",
     main = "Histrogram of slice sampling from standard normal distribution.",
     col = "lightblue")
curve(dnorm, add=TRUE, col = "red")
dev.off()