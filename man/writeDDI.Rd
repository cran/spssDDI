\name{writeDDI}
\alias{writeDDI}
\title{Write a valid DDI 3.0 XML document}
\description{
  \code{writeDDI} produces a valid DDI Version 3.0 XML document from \code{readSpssSav} output.
}
\usage{
writeDDI(l,spssSavFile,studyunit="ID",maxNumberMissings=100)
}
\arguments{
  \item{l}{\code{readSpssSav} output.}
  \item{spssSavFile}{Character string: the name of the SPSS System file.}
  \item{studyunit}{Character string: a unique string that identifies the study.}
  \item{maxNumberMissings}{Number: maximum number of missing values in the \code{missingValue} DDI's attribute.}
}
\value{
 The function doesn't return a value.
}
\details{
\code{writeDDI} generates the following DDI content: Citation, Abstract, UniverseReference, Purpose, LogicalProduct (CategoryScheme, CodeSchemes, VariableScheme),
PhysicalDataProduct (including ProprietaryRecordLayout for SPSS), PhysicalInstance. 
}
\note{
To validate the DDI instance, please enable physicaldataproduct\_proprietary:3\_0\_Beta.

}
\references{
\describe{
 \item{DDI}{\url{http://www.ddialliance.org/}}
 \item{PSPP}{\url{http://www.gnu.org/software/pspp/}}
 \item{SPSSReader}{\url{http://forge.opendatafoundation.org/gf/project/ukda_dext/frs/}}
}
}
\author{Guido Gay}
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
\keyword{file}
