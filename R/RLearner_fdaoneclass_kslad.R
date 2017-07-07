#' @export
makeRLearner.fdaoneclass.kslad = function() {
  makeRLearnerOneClass(
    cl = "fdaoneclass.kslad",
    package = "kslad",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "c.reg", lower = 0, upper = 1, default = NULL, special.vals = list(NULL)),
      makeNumericLearnerParam(id = "k", lower = 0, upper = 1, default = 0.02),
      makeNumericLearnerParam(id = "l", lower = 0, upper = 1, default = 0.2),
      makeDiscreteLearnerParam(id = "pegasos-momentum", default = "pegasos", values = c("pegasos", "pegasos-momentum", "sqrt", "user")),
      makeUntypedLearnerParam("step.size", default = NULL, requires = quote(step == "user")),
      makeIntegerLearnerParam(id = "max.iter", lower = 0L, upper = Inf, default = 500L),
      makeDiscreteLearnerParam(id = "init", default = "kmeans", values = c("kmeans", "random", "user")),
      makeUntypedLearnerParam("init.shapes", default = NULL, requires = quote(init == "user")),
      makeNumericLearnerParam("convergence.eps", default = 10^-3, upper = Inf, lower = 0),
      makeLogicalLearnerParam("show.info", default = FALSE) ,
      makeDiscreteLearnerParam("qpsolver", default = "ipop", values = c("ipop", "ipopCpp")),
      makeDiscreteLearnerParam(id = "kernel", default = "rbfdot",
        values = c("vanilladot", "polydot", "rbfdot", "tanhdot", "laplacedot")),
      makeNumericLearnerParam(id = "sigma", lower = 0, upper = Inf, default = NULL,
        special.vals = list(NULL), requires = quote(kernel %in% c("rbfdot", "laplacedot"))),
      makeIntegerLearnerParam(id = "degree", default = 3L, lower = 1L,
        requires = quote(kernel %in% c("polydot"))),
      makeNumericLearnerParam(id = "scale", default = 1, lower = 0,
        requires = quote(kernel %in% c("polydot", "tanhdot"))),
      makeNumericLearnerParam(id = "offset", default = 1,
        requires = quote(kernel %in% c("polydot", "tanhdot")))
    ),
    properties =  c("oneclass", "numerics"),
    name = "Kernelized Anomaly Detection and Shapelet Learning",
    short.name = "fdaoneclass kSlad",
    callees = "kslad"
  )
}

#' @export
trainLearner.fdaoneclass.kslad = function(.learner, .task, .subset, .weights = NULL, degree,
  offset, scale, sigma, ...) {
  # Get Data
  x = getTaskFeatureNames(.task)
  d = getTaskData(.task, .subset)[, x]
  # Handle kernel parameters
  kpar = learnerArgsToControl(list, degree, offset, scale, sigma)
  if (base::length(kpar) == 0L) {
    kpar = list()
  }
  kslad::learnKernelAnomalyModel(data = d, kpar = kpar, ...)
}

#' @export
predictLearner.fdaoneclass.kslad = function(.learner, .model, .newdata, ...) {
  # kslad currently can't predict probabilities only response
  # browser()
  p = kslad::predict.kslad(.model$learner.model, newdata = .newdata, ...)
  if (.learner$predict.type == "response") {
    p = as.factor(p) # as.logical(p)
    levels(p) = union(levels(p), .model$task.desc$class.levels)
  }
  return(p)
}
