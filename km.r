km <- function(x,k=2L,e=1L,niter=10L,nstart=1L){
    cent <- k
    obs <- length(x[,1])
    c. <- length(x[1,])
    centroids <- matrix(NA, cent, c.)
    edist <- array(NA, c(obs, cent, c.))
    cluster <- matrix(NA, obs, c.) 
    c.ss <- rep(NA, cent)
for(i in 1:c.){
  centroids[,i] <- sample(x[,i],cent,replace = FALSE)
  }
  for(n in 1:niter){ 
    for(j in 1:cent){
      for(k in 1:c.){
        edist[,j,k] <- 1/(sqrt((x[,k] - centroids[j,k])^2))^e
      }
    }
    for(i in 1:obs){
      for(k in 1:c.){
        cluster[i,k] <- which.min(edist[i,,k])
      }
    }
    for(t in 1:cent){
      for(p in 1:cent){
        for(m in 1:c.){
          centroids[t,m] <- mean(data[cluster[,t]==p,m])
        }
      }
    }
  }
  
  return(apply(cluster,1,function(x) names(which.max(table(x)))))
}
```
