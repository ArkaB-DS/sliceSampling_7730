source("slicer_double.R")

target = function(x)  dnorm(x,-2)/2+dnorm(x,2)/2
res = slice3(1e4, 0, target,1, 10)

pdf("./Plots/bimodal_double.pdf", width = 8, height = 8)
hist(res$x,freq=F,100, xlab= "x", col = "lightblue",
     main = "Histogram of slice sampling from
     bimodal normal distribution.")
curve(target, add=TRUE, col ="red")
dev.off()