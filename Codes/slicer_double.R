accept = function(x0, x1, L, R, u, w) {
  D = FALSE
  while(R-L > 1.1*w) {
    M = (L+R)/2
    if ((x0 < M & x1 >= M) | (x0 >= M & x1 < M)) D = TRUE
    if (x1 < M) { R = M } else { L = M }
    if (D & u>=target(L) & u>= target(R)) {
      return(FALSE) 
    } 
  }
  return(TRUE)
}

slice3 = function(n,init_x,target,w,max_doubling) {
  u = x = rep(NA,n)
  x[1] = init_x
  
  for (i in 2:n) {
    u[i] = runif(1,0,target(x[i-1]))
    L = x[i-1] - runif(1,0,w)
    R = L+w
    
    # Step out
    K = max_doubling
    while((u[i]<target(L) | u[i]<target(R)) & K>0) {
      if (runif(1) < 0.5) {
        L = L-(R-L)
      } else {
        R = R+(R-L)
      }
      K = K-1
    }
    
    # Sample and shrink
    repeat {
      x[i] = runif(1,L,R)
      if (u[i]<target(x[i]) & 
          accept(x[i-1], x[i], L, R, u[i], w)) break
      
      # shrink
      if (x[i]>x[i-1]) R = x[i]
      if (x[i]<x[i-1]) L = x[i]
    }
  }
  return(list(x=x,u=u))
}