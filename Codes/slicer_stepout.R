slice2 = function(n,init_x,target,w,max_steps) {
  u = x = rep(NA,n)
  x[1] = init_x
  
  for (i in 2:n) {
    u[i] = runif(1,0,target(x[i-1]))
    L = x[i-1] - runif(1,0,w)
    R = L+w
    
    # Step out
    J = floor(max_steps*runif(1))
    K = (max_steps-1)-J
    while((u[i]<target(L)) & J>0) {
      L = L-w
      J = J-1
    } 
    while((u[i]<target(R)) & K>0) {
      R = R+w
      K = K-1
    }
    
    # Sample and shrink
    repeat {
      x[i] = runif(1,L,R)
      if (u[i]<target(x[i])) break
      
      # shrink
      if (x[i]>x[i-1]) R = x[i]
      if (x[i]<x[i-1]) L = x[i]
    }
  }
  return(list(x=x,u=u))
}