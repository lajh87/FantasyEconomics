

variable <- c("Var 1", "Var 2", "Var 3")
ml <- c(45, 70, 15)
min <- c(20, 60, 10)
max <- c(70, 95, 25)

tpe <- dplyr::tibble(variable, min, ml, max)

corr <-matrix(c(1,   0.8, 0.3,
                0.8, 1,   0.5,
                0.3, 0.5, 1),
               nrow = 3)

# sigma <- https://modelassist.epixanalytics.com/display/EA/Triangular+distribution

#https://aaronschlegel.me/cholesky-decomposition-r-example.html
# https://towardsdatascience.com/behind-the-models-cholesky-decomposition-b61ef17a65fb
# https://www.mathworks.com/help/finance/corr2cov.html
# https://blogs.sas.com/content/iml/2010/12/10/converting-between-correlation-and-covariance-matrices.html
# https://en.wikipedia.org/wiki/Beta_distribution
#
cor2cov <- function(R, S) {
  sweep(sweep(R, 1, S, "*"), 2, S, "*")
}


