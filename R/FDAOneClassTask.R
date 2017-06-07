#' @title Functional data OneClass Classification task.
#'
#' @description
#' Create a functional data oneclass-classification task. It is similar to traditional
#' oneclass-tasks, but for use with functional features. This means that some features
#' in the task will be so-called functional features, measured on a grid or time scale.
#' Different functional features might come from
#' different sensors for example.
#'
#' @inheritParams Task
#' @template arg_fdatask_pars
#' @return [\code{\link{FDAOneClassTask}}].
#' @export
#' @aliases FDAOneClassTask
makeFDAOneClassTask = function(id = deparse(substitute(data)), data, target,
  weights = NULL, blocking = NULL, positive = NA_character_, negative, fixup.data = "warn",
  check.data = TRUE, fd.features = NULL, fd.grids = NULL) {

  # positive needs to be a string, if it's a number convert it into string
  assert(
    checkString(positive),
    checkNumber(positive)
  )
  if (isScalarNumeric(positive))
    positive = as.character(positive)

  # negative needs to be a string, if it's a number convert it into string
  assert(
    checkString(negative),
    checkNumber(negative)
  )
  if (isScalarNumeric(negative))
    negative = as.character(negative)

  task = makeOneClassTask(id, data, target, weights, blocking, fixup.data, positive, negative, check.data)
  convertTaskToFDATask(task, "fdaoneclass", fd.features, fd.grids, "FDAOneClassTask",
    "FDAOneClassTaskDesc")
  }

makeFDAOneClassTaskDesc = function(id, data, target, weights, blocking, positive, fd.features, fd.grids) {
  new.td = makeOneClassTaskDesc(id, data, target, weights, blocking, positive)
  new.td$type = "fdaoneclass"
  # we cannot call getTaskFeatureNames here, task is not fully constructed
  feat.remain = setdiff(names(data), target)
  # Create new fields called fd.features and fd.grids for functional data
  updated.desc = updateFDATaskDesc(fd.features, fd.grids, feat.remain)
  new.td$fd.features = updated.desc$fd.features
  new.td$fd.grids = updated.desc$fd.grids
  addClasses(new.td, "FDAOneClassTaskDesc")
}

#' @export
print.FDAOneClassTask = function(x, ...) {
  di = printToChar(table(getTaskTargets(x)), collapse = NULL)[-1L]
  m = length(x$task.desc$class.levels)
  print.FDATask(x)
  catf("Classes: %i", m)
  catf(collapse(di, "\n"))
  catf("Positive/Normal class: %s", x$task.desc$positive)
  catf("Negative/Anomaly class: %s", x$task.desc$negative)
  catf("Note: As oneclass classification problem is an unsupervised learning problem,
    the label TRUE and FALSE aren't the ground truth, if the class column is automaticially created by mlR,
    but rather an assumption of the oneclass classification problem.")
}

