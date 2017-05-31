context("FDA_fdaoneclass_slad")

test_that("FDA_fdaoneclass_slad behaves like original api", {

  requirePackagesOrSkip("kslad", default.method = "load")
  data(sampleCurves, package = "kslad")

  # Use only 20 obs., training is really slow
  index = c(1:10, 30:40)

  set.seed(getOption("mlr.debug.seed"))
  a1 = kslad::learnLinearAnomalyModel(sampleCurves$train[index, ], l = 0.1, c.reg = 0.2)
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

