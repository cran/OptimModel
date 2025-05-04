solve2 = function(H)
{
  H.inv = try( solve(H), silent=TRUE )
  if ( class(H.inv)[1] == "try-error" )
    H.inv = try( Matrix::solve(Matrix::Matrix(H)), silent=TRUE )
  if ( class(H.inv)[1] == "try-error" )
    H.inv = try( chol2inv( chol(H) ), silent=TRUE )
  if ( class(H.inv)[1] == "try-error" )
    H.inv = matrix(NA, nrow(H), ncol(H))

  return(H.inv)
}    