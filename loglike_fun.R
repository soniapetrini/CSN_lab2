
## Poisson
minus_log_like_poisson <- function(lambda) {
  C <- 0;
  for(i in 1:N) {
    for(j in 2:x[i]) {
      C<-C+log(j)
    }
  }
  return(- M * log(lambda) + N*(lambda + log(1-(exp(-lambda)))) + C)
}


## Geometric
minus_log_like_geom <- function(q) {
  - (M-N)*log(1-q) - N*log(q)
}

## zeta
minus_log_like_zeta <- function(gamma) {
  N * log(zeta(gamma)) + gamma * M_1
}

## Trunc Zeta
minus_log_like_trunc_zeta <- function(gamma) {
  gamma*M_1 + N*log(H(max(x),gamma))
}
















