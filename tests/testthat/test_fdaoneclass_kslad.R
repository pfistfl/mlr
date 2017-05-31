context("FDA_fdaoneclass_kslad")

test_that("FDA_fdaoneclass_kslad behaves like original api", {

  requirePackagesOrSkip("kslad", default.method = "load")
  data(sampleCurves, package = "kslad")

  # Use only 20 obs., training is really slow
  index = c(1:10, 30:40)

  set.seed(getOption("mlr.debug.seed"))
  a1 = kslad::learnKernelAnomalyModel(sampleCurves$train[index, ], l = 0.1, c.reg = 0.2,
    k = 0.1, kernel = "rbfdot", kpars = list(sigma = 1), max.iter = 2L)
  p1 = kslad::predict.kslad(a1, sampleCurves$test)

  ps = list(l = 0.1, c.reg = 0.2, k = 0.1, kernel = "rbfdot", sigma = 1, max.iter = 2L)
  lrn = makeLearner("fdaoneclass.kslad", par.vals = ps)
  task = makeFDAOneClassTask(data = data.frame(sampleCurves$train[index, ]))

  set.seed(getOption("mlr.debug.seed"))
  m = train(lrn, task)
  p2 = kslad::predict.kslad(m$learner.model, sampleCurves$test)
  expect_equal(p1, p2)

  ptask = makeFDAOneClassTask(data = as.data.frame(sampleCurves$test))
  cp = predict(m, ptask)
  expect_equal(as.factor(as.logical(p1)), cp$data$response)

  # Works for (almost) default
  set.seed(getOption("mlr.debug.seed"))
  a2 = kslad::learnKernelAnomalyModel(sampleCurves$train[index, ], max.iter = 1L)
  p21 = kslad::predict.kslad(a2, sampleCurves$test)
  lrn2 = makeLearner("fdaoneclass.kslad", par.vals = list(max.iter = 1L))
  m2 = train(lrn2, task)
  p22 = kslad::predict.kslad(m2$learner.model, sampleCurves$test)
  expect_equal(p21, p22)
})
