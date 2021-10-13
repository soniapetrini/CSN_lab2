
# geom
#p(k) = (1 − q)^(k−1)q

geom <- function(x,q) {
  geom_degrees <- c()
  i = 1
  for (degree in x) {
    geom_degrees[i] = ((1 - q)^(degree - 1))*q
    i = i + 1
  }
  return(geom_degrees)
}


x <- unlist(degree_distrib_ls[[1]])
geom_arabic <- geom(x,params_table$q[1])

spectrum_geom <- table(geom_arabic)
spectrum <- table(x)
barplot(spectrum_geom, main = "English",
        xlab = "degree", ylab = "number of vertices", log = "xy")
barplot(spectrum, main = "English",
        xlab = "degree", ylab = "number of vertices", log = "xy")

MSE()











