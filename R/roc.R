# Copyright (C) 2016 Lyron Winderbaum
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Calculate a ROC curve.
#'
#' Calculates a maximum-resolution Receiver Operating Characteristic
#' (ROC) curve (considering all possible thresholds). Note that tied
#' values are correctly handled, which is one point of pedantry many
#' other implementations fail to satisfy.
#'
#' @param y A numeric vector, high values of which are assumed to predict
#'   class TRUE.
#' @param classes A boolean vector of the same length as \code{y},
#'   corresponding to the known classes.
#' @return A data.frame representing the ROC curve for the classifier
#'   \code{y >= t} for all possible values of the threshold \code{t}.
#'   Rows represent each unique point on this ROC curve, with columns
#'   for the respective FPR and TPR of each point. Each such point
#'   corresponds to an interval of threshold values, and a third
#'   column, t, returns the largest value of t on the
#'   closure of the corresponding interval. Note that:
#'   \itemize{
#'     \item This will include a value of \code{Inf} for the (0,0) point on
#'       the ROC curve.
#'     \item The largest value on the closure of each interval will simply
#'       be the largest value in each interval except for in the case of the
#'      (1,1) point on the ROC curve, which represents an open interval from
#'      \code{-Inf} (all others are half-open).
#'   }
#' @examples
#' roc(c(1, 2, 3, 4, 4, 5), c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE))
#' @export
roc = function(y, classes){
  # Check inputs
  if (!is.vector(y, mode="numeric")){
    stop(paste("roc::roc: invalid input: ", toString(y),
               "is not a numeric vector."))
  }
  if (!is.vector(classes,mode="logical")){
    stop(paste("roc::roc: invalid input: ", toString(classes),
               "is not a boolean vector."))
  }
  if (length(y) != length(classes)){
    stop(paste("roc::roc: inputs are of unequal length."))
  }
  if (length(y) == 0){
    warning("roc::roc: empty inputs.")
    return(data.frame())
  }

  # remove NA and NaN values.
  l = (is.na(y) | is.na(classes))
  nr = sum(l)
  if (nr == length(y)){
    warning("roc::roc: all entries of input are NA.")
    return(data.frame())
  } else if (nr > 0){
    y = y[!l]
    classes = classes[!l]
    warning(paste("roc::roc:", toString(nr), "invalid entries removed."))
  }

  # Sort
  l = order(y, decreasing=TRUE)
  y = y[l]
  classes = classes[l]

  # Calculate FPR and TPR
  np = sum(classes)
  nn = sum(!classes)

  return(data.frame(TPR = c(0,   cumsum(classes)[!duplicated(y,  fromLast=TRUE)]/np),
                    FPR = c(0,   cumsum(!classes)[!duplicated(y, fromLast=TRUE)]/nn),
                    t   = c(Inf, unique(y))))
}

#' Calculate the area under an ROC curve.
#'
#' Given a receiver operating characteristic (ROC) curve,
#' calculates the area under said curve (AUC).
#'
#' Arguments \code{fpr} and \code{tpr} must contain only values between
#' 0 and 1, without any NA or NaN values, and must be the same length.
#' Otherwise, \code{auc} will throw an appropriate error.
#'
#' @param fpr A numeric vector, representing the false positive rate (FPR)
#'   of points on a ROC curve.
#' @param tpr A numeric vector, representing the true positive rate (TPR) of
#'   points on a ROC curve, assumed to be corresponding to those represented
#'   in \code{fpr}.
#' @return The AUC.
#' @examples
#' roc.df = roc(c(1, 2, 3, 4, 4, 5), c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE))
#' auc(roc.df$FPR, roc.df$TPR)
#' @export
auc = function(fpr, tpr){
  if (!is.vector(fpr, mode="numeric")){
    stop(paste("roc::auc: invalid input: ", toString(fpr),
               "is not a numeric vector."))
  }
  if (sum(fpr > 1 | fpr < 0) > 0){
    stop(paste("roc::auc: invalid input: ", toString(fpr),
               "contains invalid values."))
  }
  if (!is.vector(tpr, mode="numeric")){
    stop(paste("roc::auc: invalid input: ", toString(tpr),
               "is not a numeric vector."))
  }
  if (sum(tpr > 1 | tpr < 0) > 0){
    stop(paste("roc::auc: invalid input: ", toString(tpr),
               "contains invalid values."))
  }
  if (length(fpr) != length(tpr)){
    stop(paste("roc::auc: inputs are of unequal length."))
  }
  # TODO: Could add checks for including (0,0) and (1,1), or add them if they
  #   are missing. Not sure, but that is probably appropriate here?

  o = order(fpr,tpr)
  fpr = fpr[o]
  tpr = tpr[o]
  n = length(fpr)
  return(sum(((tpr[1:(n - 1)]+tpr[2:n]) / 2)*(fpr[2:n] - fpr[1:(n - 1)])))
}

#' Calculate a cost function.
#'
#' Given a receiver operating characteristic (ROC) curve,
#' calculates the cost function with given weights on false positives
#' and false negatives respectively.
#'
#' Arguments \code{fpr} and \code{tpr} must contain only values between
#' 0 and 1, without any NA or NaN values, and must be the same length.
#' Otherwise, \code{cost} will throw an appropriate error.
#'
#' @inheritParams auc
#' @param fp.weight A weight (numeric value) representing the relative
#'   importance (cost) of a false positive, relative to a false negative.
#'   So a value of 2 would indicate a false positive is twice as `bad'
#'   as a false negative, a value of 0.5 would indicate it is half as bad.
#' @return A vector of costs.
#' @examples
#' roc.df = roc(c(1, 2, 3, 4, 4, 5), c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE))
#' cost(roc.df$FPR, roc.df$TPR)
#' cost(roc.df$FPR, roc.df$TPR, 2)
#' @export
cost = function(fpr, tpr, fp.weight = 1){
  if (!is.vector(fpr, mode="numeric")){
    stop(paste("roc::cost: invalid input: ", toString(fpr),
               "is not a numeric vector."))
  }
  if (sum(fpr > 1 | fpr < 0) > 0){
    stop(paste("roc::cost: invalid input: ", toString(fpr),
               "contains invalid values."))
  }
  if (!is.vector(tpr, mode="numeric")){
    stop(paste("roc::cost: invalid input: ", toString(tpr),
               "is not a numeric vector."))
  }
  if (sum(tpr > 1 | tpr < 0) > 0){
    stop(paste("roc::cost: invalid input: ", toString(tpr),
               "contains invalid values."))
  }
  if (length(fpr) != length(tpr)){
    stop(paste("roc::cost: inputs are of unequal length."))
  }
  # TODO: Add checks on fp.weight? Seems excessive.

  return((fp.weight*fpr + (1 - tpr)) / (fp.weight + 1))
}
