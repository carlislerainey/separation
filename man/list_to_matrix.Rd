% Generated by roxygen2 (4.0.2): do not edit by hand
\name{list_to_matrix}
\alias{list_to_matrix}
\title{Convert a list to a matrix}
\usage{
list_to_matrix(list, formula)
}
\arguments{
\item{list}{A named list with scalars and up to one vector.}

\item{formula}{The model formula.}
}
\description{
\code{list_to_matrix} takes a list of scalars and possibly one
 vector and returns a matrix. If the list contains only scalars, then
 \code{list_to_matrix} returns a matrix with one row. If the list contains a
 vector, then \code{list_to_matrix} returns a matrix with the number of rows
 equal to the number of elements in the vector. This must be a named list and it
 must contain only one vector and the rest be scalars.
}
\details{
Convert a list to a matrix
}

