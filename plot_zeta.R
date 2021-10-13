zeta_d <- function(xval,gamma){
  return(1/(zeta(gamma)*xval^gamma)); # definition of the zeta-function
}

# plot a zeta function with 'gamma' up to a maximal x-value of 'maxdegree'
estimated_dist_zeta <- function(gamma,x,language){
  N <- length(x)
  spectrum <- table(x)
  zeta_est <- c()
  i = 1
  for (degree in spectrum) {
    zeta_est[i] = zeta_d(xval = degree,gamma =params_table$gamma_1[params_table$lang == as.character(language)])
    i = i + 1
  }
  zeta_vals <- table(zeta_est/N)
  return(zeta_vals)
}


# plot the degree distribution of the data stored in 'filepath'
real_dist <- function(x){
  degree_spectrum = table(x)
  N <- length(x)
  degree_dist <- degree_spectrum
  return(degree_dist)
}

# example usage (for Turkish):
plot_estimated_dist_zeta(2.542,6704)
#plot_real_dist('./data/Turkish_in-degree_sequence.txt')


barplot(real_dist(x), log = "xy")
barplot(estimated_dist_zeta(params_table$gamma_1[params_table$lang == as.character(language)],x,"English"), log = "xy")
