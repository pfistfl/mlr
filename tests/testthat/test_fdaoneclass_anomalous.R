context("FDA_fdaoneclass_anomalous")

test_that("FDA_fdaoneclass_anomalous behaves like original api", {

  requirePackagesOrSkip("anomalous", default.method = "load")
  data(dat0, package = "anomalous")

  set.seed(getOption("mlr.debug.seed"))

  x = anomalous::tsmeasures(t(dat0[1:100, ]), normalise = TRUE, width = 5, window = 7)
  # Jitter constant features, as they break the normalization
  cst = apply(x, 2, function(x) length(unique(x))) == 1
  x[, cst] = jitter(x[, cst], factor = 10^-10)
  a1 = anomalous::anomaly(x, plot = FALSE)

  lrn = makeLearner("fdaoneclass.anomalous", normalise = TRUE, width = 5, window = 7)
  task = makeFDAOneClassTask(data = data.frame(dat0[1:100, ]))
  set.seed(getOption("mlr.debug.seed"))
  m = train(lrn, task)

  cp = predict(m, makeFDAOneClassTask(data = as.data.frame(dat0[1:100, ])))
  all.equal(m$learner.model$index, a1$index)
})

