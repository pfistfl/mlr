#' @export
makeRLearner.fdaoneclass.anomalous = function() {
  makeRLearnerOneClass(
    cl = "fdaoneclass.anomalous",
    package = "anomalous",
    par.set = makeParamSet(
      makeLogicalLearnerParam("normalise", default = TRUE),
      makeIntegerLearnerParam("width", lower = 0),
      makeIntegerLearnerParam("window", lower = 0),
      makeIntegerLearnerParam(id = "n", default = 10, lower = 0, upper = Inf),
      makeDiscreteLearnerParam(id = "method", default = "hdr", values = c("hdr")),
      makeLogicalLearnerParam("robust", default = TRUE),
      makeLogicalLearnerParam("plot", default = TRUE),
      makeLogicalLearnerParam("labels", default = TRUE),
      makeDiscreteVectorLearnerParam("col")
    ),
    par.vals = list(plot = FALSE),
    properties =  c("oneclass", "numerics"),
    name = "Anomalous Time-Series Detecton (Hyndman, 2015)",
    short.name = "fdaoneclass anomalous",
    callees = "anomalous",
    note = "Plot param is set to FALSE as default. For multiple time-series, features are used together"
  )
}

#' @export
trainLearner.fdaoneclass.anomalous = function(.learner, .task, .subset, .weights = NULL, ...) {
  # Get Data
  x = getTaskFeatureNames(.task)
  y = getTaskData(.task, .subset)[, x]

  # Iterate over all functional features, extract and cbind
  grd = getTaskDesc(.task)$fd.grids
  feats = getTaskDesc(.task)$fd.features
  z = sapply(names(grd), function(x) {
    y = ts(t(y), start = min(grd[[x]]), end = max(grd[[x]]))
    anomalous::tsmeasures(y, ...)
  }, simplify = FALSE)
  x = do.call("cbind", z)

  anomalous::anomaly(x = x, ...)
}

#' @export
predictLearner.fdaoneclass.slad = function(.learner, .model, .newdata, ...) {
 # FIXME
}
