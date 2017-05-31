context("FDA_fdaoneclass_kslad")

test_that("FDA_fdaoneclass_kslad behaves like original api", {

  requirePackagesOrSkip("kslad", default.method = "load")
  data(sampleCurves, package = "kslad")

  # Use only 10 obs. for 5 classes, as knn training is really slow
  index = c(1:10, 30:40)
  train = sampleCurves$train

  set.seed(getOption("mlr.debug.seed"))
  a1 = kslad::learnKernelAnomalyModel(sampleCurves$train[index, ], l = 0.1, c.reg = 0.2, kernel = "rbfdot",
    kpars = list(sigma = 1), max.iter = 2L)
  p1 = kslad::predict.slad(a1, sampleCurves$test)

  lrn = makeLearner("fdaoneclass.slad", par.vals = list(l = 0.1, c.reg = 0.2))
  task = makeFDAOneClassTask(data = data.frame(sampleCurves$train[index, ]))
  set.seed(getOption("mlr.debug.seed"))
  m = train(lrn, task)

  p2 = kslad::predict.slad(m$learner.model, sampleCurves$test)
  expect_equal(p1, p2)

  cp = predict(m, makeFDAOneClassTask(data = as.data.frame(sampleCurves$test)))
  expect_equal(as.factor(as.logical(p1)), cp$data$response)
})
