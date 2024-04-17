source("slicer.R")

# Exponential example
set.seed(6)
A = function(u,x=NA) c(0,-log(u))
res = slice(10, 0.1, dexp, A)
x = res$x
u = res$u
pdf("./Plots/Exp(1).pdf", width = 8, height = 8)

hist(slice(1e4, 0.1, dexp, A)$x, freq=F, 100,
     main="Histogram of slice sampling from Exp(1).",
     xlab="x",
     col = "lightblue")
curve(dexp, add=TRUE, col = "magenta")

dev.off()