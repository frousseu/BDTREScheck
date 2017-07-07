\name{checkBD}
\alias{checkBD}
\title{
A function for doing checks on the TRES latest DB
}
\description{
This function will do a series of checks on the latest TRES DB (adults, chicks and broods) and returns a list with the results.
}
\usage{
checkBD(dsn = ".", year, adultsNew = NULL, broodsNew = NULL, chicksNew = NULL, adultsOld = "Adultes_2004-2015.xlsx", broodsOld = "Couvee_2004-2015.xlsx", chicksOld = "Oisillons_2004-2015.xls", sheet = 1, stop = FALSE)
}
\arguments{
  \item{dsn}{Path to the location of the different databases}
  \item{year}{Year in which verfications are made}
  \item{adultsNew}{Excel file with the latest data}
  \item{broodsNew}{Excel file with the latest data}
  \item{chicksNew}{Excel file with the latest data}
  \item{adultsOld}{Excel file with the previous data}
  \item{broodsOld}{Excel file with the previous data}
  \item{chicksOld}{Excel file with the previous data}
  \item{sheet}{All data should be contained in the first sheet. The first sheet is read by default}
  \item{stop}{Logical. Whether the function should stop for some checks to prevent spurious further problems. Currently not ignored }
}
\details{
Returns a list with the different checks and problem lines. Elements in the list can be vectors, lists, data.frames, etc.
}
\value{
Returns a list of class \code{TREScheck}.
}
\author{
Francois Rousseu, Nghia Tran and Audrey Bourret
}
\seealso{
\code{\link{checkShow}}, \code{\link{print.TREScheck}}
}
\examples{}

