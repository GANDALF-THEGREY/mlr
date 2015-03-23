#'@export
makeRLearner.classif.rflda=function(){
  makeRLearnerClassif(
    cl ="classif.rflda",
    package="HiDimDA",
    par.set=makeParamSet(makeDiscreteLearnerParam(id="q",default = 3L,values=1:5)
    ),
    properties=c("twoclass","multiclass","numerics"),
    name="Factor-Based Linear Discriminant Analysis",
    short.name="rflda",
    note="f",

  )
}

#'@export
trainLearner.classif.rflda=function(.learner,.task,.subset,.weights=NULL,...){
  d=getTaskData(.task,.subset,target.extra = TRUE)
  SelectionRes=HiDimDA::SelectV(d$data,d$target)
  SelectedFeatures=d$data[SelectionRes$vkpt]
  HiDimDA::RFlda(SelectedFeatures,grouping=d$target)
}
#'export
predictLearner.classif.rflda=function(.learner,.model,.newdata,...){
   m = .model$learner.model
   predict(m,.newdata,grpcodes=.model$task.desc$class.levels)$class
}
