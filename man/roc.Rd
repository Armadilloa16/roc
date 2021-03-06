% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roc.R
\name{roc}
\alias{roc}
\title{Calculate a ROC curve.}
\usage{
roc(y, classes)
}
\arguments{
\item{y}{A numeric vector, high values of which are assumed to predict
class TRUE.}

\item{classes}{A boolean vector of the same length as \code{y},
corresponding to the known classes.}
}
\value{
A data.frame representing the ROC curve for the classifier
  \code{y >= t} for all possible values of the threshold \code{t}.
  Rows represent each unique point on this ROC curve, with columns
  for the respective FPR and TPR of each point. Each such point
  corresponds to an interval of threshold values, and a third
  column, t, returns the largest value of t on the
  closure of the corresponding interval. Note that:
  \itemize{
    \item This will include a value of \code{Inf} for the (0,0) point on
      the ROC curve.
    \item The largest value on the closure of each interval will simply
      be the largest value in each interval except for in the case of the
     (1,1) point on the ROC curve, which represents an open interval from
     \code{-Inf} (all others are half-open).
  }
}
\description{
Calculates a maximum-resolution Receiver Operating Characteristic
(ROC) curve (considering all possible thresholds). Note that tied
values are correctly handled, which is one point of pedantry many
other implementations fail to satisfy.
}
\examples{
roc(c(1, 2, 3, 4, 4, 5), c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE))
}

