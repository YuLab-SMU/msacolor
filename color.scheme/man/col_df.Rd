\name{col_df}
\alias{col_df}
\docType{data}
\title{the data frame of color scheme}
\description{
The dataset gives a a color scheme to mapping sequences aligned.
}
\usage{data("col_df")}
\format{
  A data frame with 24 observations on the following 6 variables.
  \describe{
    \item{\code{category}}{a factor with levels \code{Aromatic} \code{Cysteines} \code{Glycines} \code{Hydrophobic} \code{Negative charge} \code{Polar} \code{Positive charge} \code{Prolines} \code{Unconserved}}
    \item{\code{colour}}{a character vector}
    \item{\code{re_position}}{a character vector}
    \item{\code{thred}}{a numeric vector}
    \item{\code{re_gp}}{a character vector}
    \item{\code{type}}{a character vector}
  }
}
\details{
the color scheme mainly depends on re_position, re_gp and thred
}
\source{a reference}
\references{a reference}
\examples{
data(col_df)
## maybe str(col_df) ; plot(col_df) ...
}
\keyword{datasets}
