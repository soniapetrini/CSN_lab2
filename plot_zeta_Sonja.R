zeta_d <- function(xvals,gamma){
  return(1/(zeta(gamma)*xvals^gamma)); # definition of the zeta-function
}

# plot a zeta function with 'gamma' up to a maximal x-value of 'maxdegree'
estimated_dist_zeta <- function(gamma,x){
  xvals <- sort(unique(x),decreasing = F)
  yvals <-zeta_d(xvals,gamma)
  return(yvals)
}

# plot the real degree distribution of the data 
real_dist <- function(x){
  degree_spectrum <- table(x)
  N <- length(x)
  degree_dist <- data.frame(degree_spectrum/N)
  return(degree_dist$Freq)
}

# example usage (for Hungarian):
x <- unlist(degree_distrib_ls[[8]])
xvals <- sort(as.integer(unique(x)))
y_zeta <- estimated_dist_zeta(gamma = params_table$gamma_1[params_table$lang == "Hungarian"],
                              x = x)
y_real <- real_dist(x)

plot(y_zeta)
plot(y_real)

dist_zeta <- data.frame("y_pred"= y_zeta, "y_real" = y_real, "degree" = xvals)

ggplot(data=dist_zeta) +
  geom_bar(aes(x=degree, y=y_pred),stat="identity", fill = "magenta") +
  geom_bar(aes(x=degree, y=y_real),stat="identity", fill = "magenta") +
  geom_point(aes(x=degree, y=y_pred),colour = "black", fill = "white") +
  geom_point(aes(x=degree, y=y_real),colour = "black", fill = "white") +
  theme_minimal() +
  labs(y= "freq",x="k") +
  scale_x_log10() +
  scale_y_log10()
  

library(MLmetrics)
MSE(y_zeta*N,y_real*N)
