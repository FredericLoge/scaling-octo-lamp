# DESCRIPTION ------------------------------------------------------------------
#
# To recreate a famous fractal, we have to generate data under the following pattern
# - pick x0 in [0, 1]
# - pick lambda in [0, 4]
# - run, as many times as you wish:
#   x_t = lambda * x_{t-1} * (1 - x_{t-1})
#
# a proper resource on Rcpp is given by H. Wickham's book, Advanced R
# > https://adv-r.hadley.nz/rcpp.html

library(Rcpp)

# implementation ---------------------------------------------------------------

# R implementation
run_iteration <- function(n_iter, x0, lambda){
  for(i in 1:n_iter){
    x0 <- lambda * x0 * (1-x0)
  }
  return(x0)
}

# Rcpp implementation
cppFunction('
  double run_iteration_cpp(int n_iter, double x0, double lambda) {
    for (int i = 0; i < n_iter; i++) {
      x0 = lambda * x0 * (1.0 - x0);
    }
    return x0;
  }
')

# comparison
system.time({ run_iteration_cpp(n_iter=1e6, x0=0.5, lambda=3.8) })
system.time({ run_iteration(n_iter=1e6, x0=0.5, lambda=3.8) })

# full algorithm ---------------------------------------------------------------

# algorithm - test the two 
system.time({
  n_x0 = 10000
  n_lambda = 100
  mat = matrix(data=NA, nrow=n_x0*n_lambda, ncol=3)
  row_idx = 1
  for(x0 in runif(n=n_x0, min=0, max=1)){
    for(lambda in runif(n=n_lambda, min=2, max=4)){
      r <- run_iteration(n_iter=100, x0=x0, lambda=lambda)
      # r <- run_iteration_cpp(n_iter=100, x0=x0, lambda=lambda)
      mat[row_idx,] <- c(x0, lambda, r)
      row_idx <- row_idx+1
    }
  }
})

# plot result
plot(
  x=mat[,2], 
  y=mat[,3], 
  pch='.', 
  xlab=expression(lambda), 
  ylab=expression(x[t]),
  cex.axis=2,
  cex.lab=2,
  cex.main=2,
  main='Bifurcation Diagram'
)

# implement full loop in Rcpp --------------------------------------------------

cppFunction('
  NumericMatrix run_iteration_mat(int n_iter,
                                  NumericVector x0,
                                  NumericVector lambda) {

    int n = x0.size();
    int m = lambda.size();
    int out_size = n*m;

    NumericMatrix out(out_size, 3);  
    // col 0 = x0
    // col 1 = lambda
    // col 2 = final value

    int row_idx = 0;
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < m; j++) {
        double x = x0[i];
        double lam = lambda[j];
        for (int k = 0; k < n_iter; k++) {
          x = lam * x * (1.0 - x);
        }
        out(row_idx, 0) = x0[i];
        out(row_idx, 1) = lam;
        out(row_idx, 2) = x;
        row_idx += 1;
      }
    }

    return out;
  }
')

n_x0 = 100
n_lambda = 10000

# run de l'implementation Rcpp
system.time({
  mat <- run_iteration_mat(
    n_iter=100, 
    x0=runif(n=n_x0, min=0, max=1), 
    lambda=runif(n=n_lambda, min=3.4, max=4)
  )
})

# plot result
plot(
  x=mat[,2], 
  y=mat[,3], 
  pch='.', 
  xlab=expression(lambda), 
  ylab=expression(x[t]),
  cex.axis=2,
  cex.lab=2,
  cex.main=2,
  main='Bifurcation Diagram'
)

# important: being able to generate this computations+graph rapidly
# opens the door to new applications: more computations+interactive viz
