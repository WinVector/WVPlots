% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PRPlot.R
\name{PRPlot}
\alias{PRPlot}
\title{Plot Precision-Recall plot.}
\usage{
PRPlot(frame, xvar, truthVar, truthTarget, title, ..., estimate_sig = FALSE)
}
\arguments{
\item{frame}{data frame to get values from}

\item{xvar}{name of the independent (input or model) column in frame}

\item{truthVar}{name of the dependent (output or result to be modeled) column in frame}

\item{truthTarget}{value we consider to be positive}

\item{title}{title to place on plot}

\item{...}{no unnamed argument, added to force named binding of later arguments.}

\item{estimate_sig}{logical, if TRUE compute significance}
}
\description{
Plot Precision-Recall plot.
}
\details{
See https://www.nature.com/articles/nmeth.3945 for a discussion of precision and recall,
and how the precision/recall plot relates to the ROC plot.

In addition to plotting precision versus recall, \code{PRPlot} reports the best
achieved F1 score, and plots an isoline corresponding to that F1 score.
}
\examples{

if (requireNamespace('data.table', quietly = TRUE)) {
	# don't multi-thread during CRAN checks
		data.table::setDTthreads(1)
}

set.seed(34903490)
x = rnorm(50)
y = 0.5*x^2 + 2*x + rnorm(length(x))
frm = data.frame(x=x,y=y,yC=y>=as.numeric(quantile(y,probs=0.8)))
frm$absY <- abs(frm$y)
frm$posY = frm$y > 0
frm$costX = 1
WVPlots::PRPlot(frm, "x", "yC", TRUE, title="Example Precision-Recall plot")

}
\seealso{
\code{\link{ROCPlot}}
}
