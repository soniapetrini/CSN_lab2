require("stats4")
require("VGAM")

source = read.table("list_in.txt", 
                    header = TRUE,               # this is to indicate the first line of the file contains the names of the columns instead of the real data
                    as.is = c("language","file") # this is need to have the cells treated as real strings and not as categorial data.
)

d<- data.frame()

for (i in 1:nrow(source)) {
  filepath <- source$file[i]
  degree_sequence <- read.table(filepath,header = FALSE)
  x <- degree_sequence$V1
  M_1 <- sum(log(x))
  M <- sum(x)
  N <- length(x)
  
  H <- function(kmax,gamma) {
    s <- 0
    for (i in 1:kmax) {
      s <- s + (1/(i^gamma))
    }
    return(s)
  }
  
  zeta_d <- function(x){
    return(1/(zeta(2.543344)*x^2.543344));
  }
  
  ## zeta
  minus_log_like_zeta <- function(gamma) {
    N * log(zeta(gamma)) + gamma * M_1
  }
  
  mle_zeta <- mle(minus_log_like_zeta,
                  start = list(gamma = 2),
                  method = "L-BFGS-B",
                  lower = c(1.0000001))
  ## Geometric
  minus_log_like_geom <- function(q) {
    - (M-N)*log(1-q) - N*log(q)
  }
  mle_geom <- mle(minus_log_like_geom,
                  start = list(q = N/M),
                  method = "L-BFGS-B",
                  lower = c(0.00001),
                  upper = c(0.99999))
  ## Trunc Zeta
  minus_log_like_trunc_zeta <- function(kmax,gamma) {
    gamma*M_1 + N*log(H(kmax,gamma))
  }
  mle_trunc_zeta <- mle(minus_log_like_trunc_zeta,
                        start = list(gamma = 2, kmax=max(x)),
                        method = "L-BFGS-B",
                        lower = list(kmax = 1),
                        upper = list(kmax = N))
  
  minus_log_likelihood_zeta2 <- function(){
    2*M_1 + N * log((pi^2)/6)
  }
  
  minus_log_like_poisson <- function(lambda) {
    C <- 0;
    for(i in 1:N)
      for(j in 2:x[i])
        C<-C+log(j)
    return(-M * log(lambda) + N*(lambda + log(1-(exp(-lambda)))) + C)
  }
  
  mle_poisson <- mle(minus_log_like_poisson,
                     start = list(lambda = M/N),
                     method = "L-BFGS-B",
                     lower = c(0.0000001))
  
  get_AIC <- function(m2logL,K,N) {
    m2logL + 2*K*N/(N-K-1) # AIC with a correction for sample size
  }
  
  aic_poisson <- get_AIC(attributes(summary(mle_poisson))$m2logL, 1, N)
  aic_geom <-get_AIC(attributes(summary(mle_geom))$m2logL, 1, N)
  aic_zeta2 <- get_AIC(2*minus_log_likelihood_zeta2(),0,N)
  aic_zeta <- get_AIC(attributes(summary(mle_zeta))$m2logL, 1, N)
  aic_zeta_trunc <- get_AIC(attributes(summary(mle_trunc_zeta))$m2logL, 2, N)
  best <- min(aic_poisson,aic_geom,aic_zeta2,aic_zeta,aic_zeta_trunc)
  d[i,1] <- aic_poisson - best
  d[i,2] <- aic_geom - best
  d[i,3] <- aic_zeta2 - best
  d[i,4] <- aic_zeta - best
  d[i,5] <- aic_zeta_trunc - best
}

colnames(d) <- c('Poisson','Geom','Zeta-2','Zeta','Zeta-trunc')




