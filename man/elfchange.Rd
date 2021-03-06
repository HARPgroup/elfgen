% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/elfchange.R
\name{elfchange}
\alias{elfchange}
\title{Calculate Net change in Richness From A Percent Reduction In Flow}
\usage{
elfchange(stats, yaxis_thresh, xlabel = FALSE, ylabel = FALSE)
}
\arguments{
\item{stats}{a dataframe of ELF model statistics}

\item{yaxis_thresh}{y-axis threshold used for plotting maximum y-axis limit}

\item{xlabel}{used to overwrite default x-axis label}

\item{ylabel}{used to overwrite default y-axis label}
}
\value{
richness.change
}
\description{
function for calculating change in richness from streamflow reduction
}
