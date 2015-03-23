context("classif_rflda")
require('HiDimDA')
test_that("classif_rflda", {
  requirePackages("HiDimDA", default.method = "load")
  #set.seed(getOption("mlr.debug.seed"))
  SelectionRes=HiDimDA::SelectV(multiclass.train,grouping=multiclass.train[,multiclass.class.col])
  SelectedFeatures=multiclass.train[SelectionRes$vkpt]
  m <-HiDimDA::RFlda(SelectedFeatures, grouping=multiclass.train[,multiclass.class.col])
  #set.seed(getOption("mlr.debug.seed"))
  p = predict(m,newdata=multiclass.test[SelectionRes$vkpt],grpcodes=levels(multiclass.df[,multiclass.class.col]))
  testSimple("classif.rflda", multiclass.df, multiclass.target, multiclass.train.inds, p$class)
  #testProb  ("classif.lda", multiclass.df, multiclass.target, multiclass.train.inds, p$posterior)
  return
  tt = HiDimDA::RFlda
  tp = function(model, newdata) predict(model, newdata)$class

  #testCV("classif.rflda", multiclass.df, multiclass.target, tune.train = tt, tune.predict = tp)
})
