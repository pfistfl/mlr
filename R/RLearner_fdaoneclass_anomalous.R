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
  feats = getTaskFeatureNames(.task)
  y = getTaskData(.task, .subset)[, feats]

  # Iterate over all functional features, extract and cbind
  grd = getTaskDesc(.task)$fd.grids
  feats = getTaskDesc(.task)$fd.features
  z = sapply(names(grd), function(x) {
    y = ts(t(y), start = min(grd[[x]]), end = max(grd[[x]]))
    anomalous::tsmeasures(y, ...)
  }, simplify = FALSE)
  x = do.call("cbind", z)

  # Jitter constant features, as it breaks the normalization
  cst = apply(x, 2, function(x) length(unique(x))) == 1
  x[, cst] = jitter(x[, cst], factor = 10^-2)

  anomaly_v2(x = x, ...)
}

#' @export
predictLearner.fdaoneclass.anomalous = function(.learner, .model, .newdata, ...) {
    testx = tsmeasures(.newdata)
    anomaly_v2_predict(.model$learner.model, newdata = testx)
  }


# Reimplementations of TRAIN / TEST from anomalous.
# The original implementations only predicts on training data.

anomaly_v2 = function(x, n = 10, method = "hdr", robust = TRUE,
  plot = TRUE, labels = TRUE, col) {

  # x: a matrix returned by `tsmeasures` function
  nc = nrow(x)
  if (nc < n) {
    stop("Your n is too large.")
  }
  x[is.infinite(x)] = NA # ignore inf values
  naomit.x = na.omit(x) # ignore missing values
  na.act = na.action(naomit.x)
  if (is.null(na.act)) {
    avl = 1:nc
  } else {
    avl = (1:nc)[-na.action(naomit.x)]
  }
  method = match.arg(method)
  # robust PCA space (scaling version)
  if (robust) {
    rbt.pca = pcaPP::PCAproj(naomit.x, k = 2, center = mean, scale = sd)
  } else {
    rbt.pca = princomp(scale(naomit.x, center = TRUE, scale = TRUE),
      cor = TRUE)
  }
  scores = rbt.pca$scores
  scoreswNA = matrix(NA, nrow = nc, ncol = 2)
  scoreswNA[avl, ] = scores
  tmp.idx = vector(length = n)
  if (method == "hdr") {
    hdrinfo = hdrcde::hdr.2d(x = scores[, 1], y = scores[, 2],
      kde.package = "ks", prob = c(0.001, 0.01, 0.05))
    tmp.idx = order(hdrinfo$fxy)[1:n]
    main = "Lowest densities on anomalies"
  }

  idx = avl[tmp.idx] # Put back with NA
  if (plot) {
    if (missing(col)) {
      col = c("grey", "darkblue")
    } else {
      lencol = length(col)
      if (lencol == 1L) {
        col = rep(col, 2)
      } else {
        col = unique(col)[1:2]
      }
    }
    xrange = range(scores[, 1], na.rm = TRUE)
    yrange = range(scores[, 2], na.rm = TRUE)
    plot(x = scores[-tmp.idx, 1], y = scores[-tmp.idx, 2],
      pch = 19, col = col[1L], xlab = "PC1", ylab = "PC2", main = main,
      xlim = xrange, ylim = yrange)
    points(scores[tmp.idx, 1], scores[tmp.idx, 2],
      col = col[2L], pch = 17)
    if (labels) {
      text(scores[tmp.idx, 1] + 0.3, scores[tmp.idx, 2],
        col = col[2L], label = 1:length(idx), cex = 1.2)
    }
  }
  return(structure(list(index = idx, scores = scoreswNA, hdr_results = hdrinfo, pca_results=rbt.pca)))
}



anomaly_v2_predict = function(anomaly_out_train, newdata, predicttype = "response"){

  pca_trafo = anomaly_out_train$pca_results$loadings
  center = anomaly_out_train$pca_results$center
  scale = anomaly_out_train$pca_results$scale
  x_vec = apply(test_measures, 1, function(x) -((x-center)/scale) %*% pca_trafo[,1] )
  y_vec = apply(test_measures, 1, function(x) -((x-center)/scale) %*% pca_trafo[,2] )
  test_scores = cbind(x_vec, y_vec)

  dens_value = rep(0,dim(test_measures)[1] )

  for( i in 1:dim(test_measures)[1]){

    min_x = min(which(anomaly_out_train$hdr_results$den$x >= test_scores[i,1]))
    min_y = min(which(anomaly_out_train$hdr_results$den$y >= test_scores[i,2]) )

    if(is.infinite(min_x)) {min_x = max(which(anomaly_out_train$hdr_results$den$x <= test_scores[i,1]))}
    if(is.infinite(min_y)) {min_y = max(which(anomaly_out_train$hdr_results$den$y <= test_scores[i,2]) )}

    dens_value[i] = anomaly_out_train$hdr_results$den$z[min_x,min_y]
  }

  names(dens_value) = seq(1:dim(test_measures)[1])
  return(structure(list(f_density=dens_value, test_scores=test_scores)))
}
