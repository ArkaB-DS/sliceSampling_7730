# Slice sampler
slice = function(n,init_x,target,A) {
  u = x = rep(NA,n)
  x[1] = init_x
  u[1] = runif(1,0,target(x[1])) # This never actually gets used
  
  for (i in 2:n) {
    u[i] = runif(1,0,target(x[i-1]))
    endpoints = A(u[i],x[i-1]) # The second argument is used in the second example
    x[i] = runif(1, endpoints[1],endpoints[2])
  }
  return(list(x=x,u=u))
}