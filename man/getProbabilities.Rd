% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/Prediction_operators.R
\name{getProbabilities}
\alias{getProbabilities}
\title{Get probabilities for some classes.}
\usage{
getProbabilities(pred, cl)
}
\arguments{
\item{pred}{[\code{\link{Prediction}}]\cr
Prediction object.}

\item{cl}{[\code{character}]\cr
Names of classes.
Default is either all classes for multi-class problems or the positive class for binary classification.}
}
\value{
[\code{data.frame}] with numerical columns or a numerical vector if length of \code{cl} is 1.
  Order of columns is defined by \code{cl}.
}
\description{
Get probabilities for some classes.
}
\examples{
task = makeClassifTask(data = iris, target = "Species")
lrn = makeLearner("classif.lda", predict.type = "prob")
mod = train(lrn, task)
# predict probabilities
pred = predict(mod, newdata = iris)

# Get probabilities for all classes
head(getProbabilities(pred))

# Get probabilities for a subset of classes
head(getProbabilities(pred, c("setosa", "virginica")))
}
\seealso{
Other predict: \code{\link{asROCRPrediction}};
  \code{\link{plotROCRCurves}};
  \code{\link{plotViperCharts}};
  \code{\link{predict.WrappedModel}};
  \code{\link{setPredictThreshold}};
  \code{\link{setPredictType}}
}

