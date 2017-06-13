#' Make Fixed Holdout Instances for a number of data splits
#' @param train.inds [list] \cr
#'   List of the training id vectors
#' @param test.inds [list] \cr
#'   List of the test id vectors  
#' @param size [integer] \cr
#'   Size of the data set to resample. 
#'   The function needs to know the largest possible index of the whole data set.
#' @export
makeFixedHoldoutInstances = function(train.inds, test.inds, size = NULL) {
  
  # Asser valid inputs
  assertList(train.inds, len = length(test.inds))
  assertList(test.inds, len = length(train.inds))
  assertInteger(size, len = 1L, null.ok = TRUE)
  # And convert to integers if required
  train.inds = lapply(train.inds, asInteger, any.missing = FALSE)
  test.inds = lapply(test.inds, asInteger, any.missing = FALSE)
  
  if(is.null(size)) {
    size = max(c(train.inds, test.inds))
  }
  
  # Length of train.inds list sets the number of iterations
  l = length(train.inds)
  rdesc = makeResampleDesc("Bootstrap", iters = l)
  rin = makeResampleInstance(rdesc, size = size)
  rin$train.inds = train.inds
  rin$test.inds = test.inds
  rin$desc$id = "Multiple Holdout Instances"
  
  return(rin)
}
