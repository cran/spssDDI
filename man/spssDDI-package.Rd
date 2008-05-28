\name{spssDDI-package}
\alias{spssDDI-package}
\docType{package}
\title{
A package to read SPSS files and produce valid DDI Version 3.0 XML documents
}
\description{
There are two functions included in this package:

\code{readSpssSav} reads the metadata inside a SPSS System file and optionally the actual data.
  
\code{writeDDI} produces a valid DDI Version 3.0 XML document from \code{readSpssSav} output.
}
\details{
\tabular{ll}{
Package: \tab spssDDI\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2008 - 6-15\cr
License: \tab GNU General Public Licence\cr
}

The SPSS System file format is described in \file{pspp-dev.pdf}  (\url{http://www.gnu.org/software/pspp/}). 
\code{PSSP} is a program for statistical analysis of sampled data. 

SPSS System files are also described in  \code{SPSSReader} (\url{http://forge.opendatafoundation.org/gf/project/ukda_dext/frs/}).

   
The Data Documentation Initiative (\url{http://www.ddialliance.org/}) is an international effort to establish a standard for technical 
documentation describing social science data.

\code{spssDDI} is a work in progress. The author welcomes questions, comments and code submissions.
}
\author{
Guido Gay

Maintainer:  <gay@irer.it>
}
\references{
\describe{
 \item{DDI}{\url{http://www.ddialliance.org/}}
 \item{PSPP}{\url{http://www.gnu.org/software/pspp/}}
 \item{SPSSReader}{\url{http://forge.opendatafoundation.org/gf/project/ukda_dext/frs/}}
}
}
\keyword{ package }
\examples{
\dontrun{
benchmark<-readSpssSav("benchmarkSpss16.sav")
writeDDI(benchmark,"benchmarkSpss16.sav")

essSample<-readSpssSav("essSample.sav")
sink("essSample.xml")
writeDDI(essSample,"essSample.sav")
sink()
}
}
