scaleRJ = function( X, medians = FALSE )
{
  if ( !is.matrix( X ) )
  {
    stop( "Input must be in matrix form" )
  }

  X = scale_c( X, medians )

  return( X )
}
