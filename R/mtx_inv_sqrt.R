mtx_inv_sqrt <- function(M) {
  s <- svd(M)
  ( s$v %*% diag( sqrt((1/s$d)) ) %*%  t(s$u) )

}
