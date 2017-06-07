#' @export
makeRLearner.fdaoneclass.slad = function() {
  makeRLearnerOneClass(
    cl = "fdaoneclass.slad",
    package = "kslad",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "c.reg", lower = 0, default = 1),
      makeNumericLearnerParam(id = "k", lower = 0, upper = 1, default = 0.03),
      makeNumericLearnerParam(id = "l", lower = 0, upper = 1, default = 0.2),
      makeDiscreteLearnerParam(id = "step", default = "pegasos", values = c("pegasos", "sqrt", "user")),
      makeUntypedLearnerParam("step.size", default = NULL, requires = quote(step == "user")),
      makeIntegerLearnerParam(id = "max.iter", lower = 0L, upper = Inf, default = 5L),
      makeDiscreteLearnerParam(id = "init", default = "kmeans", values = c("kmeans", "random", "user")),
      makeUntypedLearnerParam("init.shapes", default = NULL, requires = quote(init == "user")),
      makeDiscreteLearnerParam("loss", default = "linear", values = c("linear", "squared")),
      makeNumericLearnerParam("convergence.eps", default = 10^-3, upper = Inf, lower = 0),
      makeLogicalLearnerParam("show.info", default = FALSE) ,
      makeDiscreteLearnerParam("qpsolver", default = "ipop", values = c("ipop", "ipopCpp"))
    ),
    properties =  c("oneclass", "numerics"),
    name = "Linear Anomaly Detection and Shapelet Learning",
    short.name = "fdaoneclass slad",
    callees = "slad"
  )
}

#' @export
trainLearner.fdaoneclass.slad = function(.learner, .task, .subset, .weights = NULL, ...) {
  # Get data
  x = getTaskFeatureNames(.task)
  d = getTaskData(.task, .subset)[, x]
  # Train model
  kslad::learnLinearAnomalyModel(data = d, ...)
}

#' @export
predictLearner.fdaoneclass.slad = function(.learner, .model, .newdata, ...) {
  # kslad currently can't predict probabilities only response
  p = kslad::predict.slad(.model$learner.model, newdata = .newdata, ...)
  if (.learner$predict.type == "response") {
    p = as.factor(p) # as.logical(p)
    levels(p) = union(levels(p), .model$task.desc$class.levels)
  }
  return(p)
}
