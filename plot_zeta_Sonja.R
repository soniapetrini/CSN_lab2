
# REAL DISTR
# plot the real degree distribution of the data 
real_dist <- function(x){
  degree_spectrum <- table(x)
  N <- length(x)
  degree_dist <- data.frame(degree_spectrum/N)
  return(degree_dist)
}


# ZETA
# plot a zeta function with 'gamma'
estimated_dist_zeta <- function(xvals,gamma){
  return(1/(zeta(gamma)*xvals^gamma)); # definition of the zeta-function
}

# TRUNC ZETA
H <- function(kmax,gamma) {
  s <- 0
  for (i in 1:kmax) {
    s <- s + (1/(i^gamma))
  }
  return(s)
}

zeta_trunc_dist <- function(xvals,gamma,kmax){
  return(1/(H(kmax,gamma)*xvals^gamma)); # definition of the zeta-trunc-function
}






# example usage (for Hungarian):
x <- unlist(degree_distrib_ls[[10]])
xvals <- sort(as.integer(unique(x)))

y_zeta <- estimated_dist_zeta(gamma = params_table$gamma_1[params_table$lang == "Hungarian"],
                              xvals = xvals)
y_real <- real_dist(x)


dist_zeta <- data.frame("y_pred"= y_zeta, "y_real" = y_real$Freq, "degree" = as.integer(y_real$x))

ggplot(data=dist_zeta) +
  geom_bar(aes(x=degree, y=y_pred),stat="identity", fill = "magenta") +
  geom_bar(aes(x=degree, y=y_real),stat="identity", fill = "green") +
  geom_point(aes(x=degree, y=y_pred), colour = "red") +
  geom_point(aes(x=degree, y=y_real), colour = "green") +
  theme_minimal() +
  labs(y= "freq",x="k") +
  scale_x_log10() +
  scale_y_log10()
  

library(MLmetrics)
MSE(y_zeta*N,y_real$Freq*N)





y_zeta_trunc <- zeta_trunc_dist(xvals,
                                gamma = params_table$gamma_2[params_table$lang == "Hungarian"],
                                kmax = params_table$kmax[params_table$lang == "Hungarian"])
y_real <- real_dist(x)

dist_zeta_trunc <- data.frame("y_pred"= y_zeta_trunc, "y_real" = y_real$Freq, "degree" = as.integer(y_real$x))

ggplot(data=dist_zeta_trunc) +
  geom_bar(aes(x=degree, y=y_pred),stat="identity", fill = "magenta") +
  geom_bar(aes(x=degree, y=y_real),stat="identity", fill = "green") +
  geom_point(aes(x=degree, y=y_pred), colour = "red") +
  geom_point(aes(x=degree, y=y_real), colour = "green") +
  theme_minimal() +
  labs(y= "freq",x="k") +
  scale_x_log10() +
  scale_y_log10()

