context("FDA_oneclass_svdd")

test_that("Oneclass svdd behaves like original api", {

  requirePackagesOrSkip("kslad", default.method = "load")
  data(sampleCurves, package = "kslad")

  # Use only 20 obs., training is really slow
  set.seed(getOption("mlr.debug.seed"))

  x = sampleCurves$train[1:50, 1:40]
  a1 = kslad::svdd(x, "rbfdot", kpars = list(sigma = 1L))
  p1 = kslad::predict.kSVDD(a1, x)
  pf = as.factor(as.logical(p1))
  levels(pf) = c(FALSE, TRUE)

  lrn = makeLearner("oneclass.svdd", kernel = "rbfdot", sigma = 1L)
  task = makeOneClassTask(data = data.frame(x))
  set.seed(getOption("mlr.debug.seed"))
  m = train(lrn, task)

  expect_equal(a1$R, m$learner.model$R)
  expect_equal(a1$kernel, m$learner.model$kernel)

  cp = predict(m, makeOneClassTask(data = data.frame(x)))

  expect_equal(cp$data$response, pf)
})

