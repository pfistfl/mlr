#' @export
makeRLearner.oneclass.svdd = function() {
  makeRLearnerOneClass(
    cl = "oneclass.svdd",
    package = "kslad",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "kernel", default = "rbfdot",
        values = c("vanilladot", "polydot", "rbfdot", "tanhdot", "laplacedot")),
      makeNumericLearnerParam(id = "c.reg", lower = 0, default = 1),
      makeNumericLearnerParam(id = "sigma",
        lower = 0, requires = quote(kernel %in% c("rbfdot", "laplacedot"))),
      makeIntegerLearnerParam(id = "degree", default = 3L, lower = 1L,
        requires = quote(kernel %in% c("polydot"))),
      makeNumericLearnerParam(id = "scale", default = 1, lower = 0,
        requires = quote(kernel %in% c("polydot", "tanhdot"))),
      makeNumericLearnerParam(id = "offset", default = 1,
        requires = quote(kernel %in% c("polydot", "tanhdot")))
    ),
    properties =  c("oneclass", "numerics", "factors", "weights"),
    name = "one-class Support Vector Data Description",
    short.name = "one-class svdd",
    callees = "svdd"
  )
}

#' @export
trainLearner.oneclass.svdd = function(.learner, .task, .subset, .weights = NULL, degree, offset, scale, sigma, ...) {
  # Get Data
  x = getTaskFeatureNames(.task)
  d = getTaskData(.task, .subset)[, x]

  # Handle kernel parameters
  kpar = learnerArgsToControl(list, degree, offset, scale, sigma)
  if (base::length(kpar) == 0L) {
    kpar = list()
  }
  kslad::svdd(data = d, kpars = kpar, ...)
}

#' @export
predictLearner.oneclass.svdd = function(.learner, .model, .newdata, ...) {
  # svm currently can't predict probabilities only response
  p = predict(.model$learner.model, newdata = .newdata, ...)
  if (.learner$predict.type == "response") {
    p = as.factor(p)
    levels(p) = union(levels(p), .model$task.desc$class.levels)
  }
  return(p)
}


