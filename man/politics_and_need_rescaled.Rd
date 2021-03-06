% Generated by roxygen2 (4.0.2): do not edit by hand
\docType{data}
\name{politics_and_need_rescaled}
\alias{politics_and_need_rescaled}
\title{Governors' decisions to oppose the medicaid expansion under the PPACA.}
\format{A data frame with 50 observations of standardized version of the following
10 variables: \describe{
  \item{state}{the name of the state}
  \item{oppose_expansion}{equals one if
  the state's governor opposed the expansion and zero otherwise}
  \item{gop_governor}{equals one if the state's govenor is Republican and
  zero otherwise}
  \item{percent_favorable_aca}{an estimate of the percent of
  the state's population that views the PPACA favorably}
  \item{gop_leg}{equals one if Republicans control both houses of the state
  legislature and zero otherwise}
  \item{percent_uninsured}{the percent of the
  state's population without health insurance}
  \item{bal2012}{the state's
  year-end reserves as a percentage of total spending for 2012}
  \item{multiplier}{the state's current Medicaid multiplier}
  \item{percent_nonwhite}{the percent of the state that is non-white}
  \item{percent_metro}{the percent of the state that resides in a
  metropolitan area}}}
\usage{
politics_and_need_rescaled
}
\description{
A data set containing the \emph{rescaled} variables that Barrilleaux and Rainey (2014) use in
the their main statistical model of governors' decisions to oppose the
Medicaid expansion under the PPACA. Most importantly, no Democratic governors
opposed the expansion, leading to a problem of separation. Variables are rescaled following
Gelman (2008) and Gelman et al. (2008).
}
\keyword{datasets}

