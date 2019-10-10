# /*
#  *  Global Supporting Functions
#  *  Quick place to put all the other useful, but not reaaaally important functions
#  */

# function to re-weight proportions of data set
# as we allow user to type what proportions they want, we need to ensure they
# do sum to 100%
fun.simData_ReWeightProportions <- function( simData ) {
  sum_weights <- sum( simData[,3] )
  base_simData[,3] <- simData[,3] / sum_weights
  return( base_simData )
}

# function which updates the sample size of data set
fun.simData_UpdateSampleSize <- function( simData, sampleSize ) {
  base_simData <- simData[,3] * as.numeric( sampleSize ) 
  new_Obs <- unlist( base_simData )
  base_simData <- round( ifelse( new_Obs < 2, 2, new_Obs ), digits = 0 )
  return( base_simData )
}

# function to check if there is any variance in means of data set
fun.findVariance <- function( coreData ){ return( var( coreData[,4]))}


# function to re-weight or update sample size in one
fun.base_simData <- function( simData , ID, sampleSize ){
  base_simData <- simData
  # re-weight proportions (in case user cannot add to 100...)
  if( ID != 5 ) { 
    sum_weights <- sum( simData[,3] )
    base_simData[,3] <- simData[,3] / sum_weights
  } 
  if( ID > 1 ){
    base_simData[ , 2 ] <- fun.simData_UpdateSampleSize( simData, sampleSize )
  }  
  return( base_simData )
}

# we are only interested in factors or character columns that contain more
# than 1 unique result
fun.x_ValidateOptions <- function( index, vector, data ) {
  valid_Option <- FALSE
  n_results <- nrow( as.data.frame( table( data[vector[ index ] ] ) ) )
  if ( n_results > 1 ){
    valid_Option <- TRUE
  } 
  return( valid_Option )
}
