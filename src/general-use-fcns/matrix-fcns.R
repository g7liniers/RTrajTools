symmetrize_upper_triangular <- function(matrix){
  M <- matrix
  M[lower.tri(M)] <- t(matrix)[lower.tri(t(matrix))]
  M
}

fill_diag <- function(matrix){
  diag(matrix) <- NA
  matrix
}

sym_fill <- function(matrix){
  M <- fill_diag(matrix)
 symmetrize_upper_triangular(M)
}
