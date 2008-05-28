\name{readSpssSav}
\alias{readSpssSav}
\title{Read a SPSS Data File}
\description{
  \code{readSpssSav} reads the metadata inside a SPSS System file and optionally the actual data.
}
\usage{
readSpssSav(filename, readdata = FALSE)
}
\arguments{
  \item{filename}{Character string: the name of the SPSS System file to read from.}
  \item{readdata}{reads the actual data?}
}

\details{

A system file encapsulates actual data and dictionary information (metadata). \code{readSpssSav} reads the metadata inside a SPSS System file 
(and optionally the actual data) and returns a list with either three or four elements (\emph{Header}, \emph{Variables}, \emph{Mixed}, \emph{Data}).


\emph{Header}

\describe{
\item{RecordType}{Record type code, set to \$FL2}
\item{ProductName}{Product identification string.}
\item{LayoutCode}{Normally set to 2, rarely 3}
\item{CaseSize}{Number of variables per case}
\item{Compress}{Set to 1 if the data in the file is compressed, 0 otherwise}
\item{WeightIndex}{If one of the variables is used as a weighting variable, set to the value describing its position
within the file dictionary, otherwise 0}
\item{NumCases}{Number of cases}
\item{Bias}{Ordinarily set to 100}
\item{CreationDate}{Date of creation of the system file}
\item{CreationTime}{Time of creation of the system file}
\item{FileLabel}{File label declared by the user}
}

\emph{Variables}

\describe{
\item{Number missings}{If the variable has no missing values, set to 0.  If the variable has
one, two, or three discrete missing values, set to 1, 2, or 3,
respectively.  If the variable has a range for missing variables, set to
-2; if the variable has a range for missing variables plus a single
discrete value, set to -3}
\item{Print fmt}{Number of decimal places, field width, format type (see below), last value set to zero}
\item{Write fmt}{Number of decimal places, field width, format type (see below), last value set to zero}
\item{Varname}{Variable name. The variable name is padded on the right with spaces}
\item{Varlabel}{Variable label}
\item{Missing values}{It has
the same number of elements as the absolute value of
Number missings.  For discrete missing values, each element
represents one missing value.  When a range is present, the first
element denotes the minimum value in the range, and the second element
denotes the maximum value in the range.  When a range plus a value are
present, the third element denotes the additional discrete missing
value.}
\item{Typecode}{Set to 0 for a numeric variable, for a string variable set to its width}
}

Format types are defined as follows (see \file{pspp-dev.pdf}  (\url{http://www.gnu.org/software/pspp/}) for details):

\describe{
\item{String}{1, 2}
\item{Numeric}{3 - 12, 15 - 17, 31 - 37}
\item{Date}{20, 22 - 24, 28 - 30, 38 - 39. Dates are stored as the number of seconds since midnight, October 14, 1582}
\item{Time}{21, 25. Times are stored as a number of seconds that represents a time interval}
\item{Other}{0, 13, 14, 18, 19, 26, 27}
}


\emph{Mixed}

\describe{
\item{Value labels}{Labels for discrete variable's values. Link to variable through the value describing the variable's position within the file dictionary}
\item{Sysmis, highest, lowest }{System missing value; largest possible positive number; second-largest negative number}
\item{Major,minor,revision,floating,endianness,character}{Three software version numbers; floating point representation code (IEEE 754 = 1;IBM 370 = 2; DEC VAX = 3); 
endianness (big-endian = 1, little-endian = 2); character set (EBCDIC = 1, 7-bit ASCII = 2, 8-bit ASCII = 3, DEC Kanji = 4,  plus windows codes)}
\item{Short and long varnames}{Short and long variables names}
\item{Variable sets}{Set name and variables names}
\item{Very long variables}{Very long string variable's width}
\item{Document}{Notes}
\item{Trend}{Trend}
\item{Display}{Display parameters. (see \file{pspp-dev.pdf}  (\url{http://www.gnu.org/software/pspp/}) for details)}
\item{Strings' value labels}{String variables' value labels}
\item{Encoding}{UTF-8, windows-1252, ...}
\item{Other}{Empty}
}

\emph{Data}

Character vector with length equal to CaseSize times NumCases.
}

\note{

Invoking \code{str(readSpssSav("benchmarkSpss16.sav",readdata=T)} generates the following output:

\preformatted{
List of 4
 $ Header   :List of 11
  ..$ RecordType  : chr "$FL2"
  ..$ ProductName : chr "@(#) SPSS DATA FILE MS Windows 16.0.2                       "
  ..$ LayoutCode  : int 2
  ..$ CaseSize    : int 20
  ..$ Compress    : int 1
  ..$ WeightIndex : num 0
  ..$ NumCases    : int 3
  ..$ Bias        : num 100
  ..$ CreationDate: chr "23 Jun 08"
  ..$ CreationTime: chr "16:21:00"
  ..$ FileLabel   : chr "Test file                                                       "
 $ Variables:List of 20
  ..$ V1 :List of 7
  .. ..$ Number missings: int 0
  .. ..$ Print fmt      : int [1:4] 0 8 5 0
  .. ..$ Write fmt      : int [1:4] 2 8 5 0
  .. ..$ Varname        : chr "V1      "
  .. ..$ Varlabel       : chr "Integer"
  .. ..$ Missing values : NULL
  .. ..$ Typecode       : int 0
  ..$ V2 :List of 7
  .. ..$ Number missings: int 0
  .. ..$ Print fmt      : int [1:4] 2 8 5 0
  .. ..$ Write fmt      : int [1:4] 2 8 5 0
  .. ..$ Varname        : chr "V2      "
  .. ..$ Varlabel       : chr "Decimal"
  .. ..$ Missing values : NULL
  .. ..$ Typecode       : int 0
  ..$ V3 :List of 7
  .. ..$ Number missings: int 0
  .. ..$ Print fmt      : int [1:4] 0 8 5 0
  .. ..$ Write fmt      : int [1:4] 2 8 5 0
  .. ..$ Varname        : chr "V3      "
  .. ..$ Varlabel       : chr "Integer, sysmis"
  .. ..$ Missing values : NULL
  .. ..$ Typecode       : int 0
  ..$ V6 :List of 7
  .. ..$ Number missings: int 0
  .. ..$ Print fmt      : int [1:4] 2 8 5 0
  .. ..$ Write fmt      : int [1:4] 2 8 5 0
  .. ..$ Varname        : chr "V6      "
  .. ..$ Varlabel       : chr "Decimal, sysmis"
  .. ..$ Missing values : NULL
  .. ..$ Typecode       : int 0
  ..$ V7 :List of 7
  .. ..$ Number missings: int 2
  .. ..$ Print fmt      : int [1:4] 0 8 5 0
  .. ..$ Write fmt      : int [1:4] 2 8 5 0
  .. ..$ Varname        : chr "V7      "
  .. ..$ Varlabel       : chr "Integer, discrete user missings"
  .. ..$ Missing values : num [1:2] 1 2
  .. ..$ Typecode       : int 0
  ..$ V8 :List of 7
  .. ..$ Number missings: int -2
  .. ..$ Print fmt      : int [1:4] 0 8 5 0
  .. ..$ Write fmt      : int [1:4] 2 8 5 0
  .. ..$ Varname        : chr "V8      "
  .. ..$ Varlabel       : chr "Integer, range user missings"
  .. ..$ Missing values : num [1:2] 1 5
  .. ..$ Typecode       : int 0
  ..$ V9 :List of 7
  .. ..$ Number missings: int -2
  .. ..$ Print fmt      : int [1:4] 2 8 5 0
  .. ..$ Write fmt      : int [1:4] 2 8 5 0
  .. ..$ Varname        : chr "V9      "
  .. ..$ Varlabel       : chr "Decimal, range user missings"
  .. ..$ Missing values : num [1:2] 0.1 10.7
  .. ..$ Typecode       : int 0
  ..$ V10:List of 7
  .. ..$ Number missings: int 0
  .. ..$ Print fmt      : int [1:4] 0 8 5 0
  .. ..$ Write fmt      : int [1:4] 2 8 5 0
  .. ..$ Varname        : chr "V10     "
  .. ..$ Varlabel       : chr "Integer, value labels"
  .. ..$ Missing values : NULL
  .. ..$ Typecode       : int 0
  ..$ V11:List of 7
  .. ..$ Number missings: int 0
  .. ..$ Print fmt      : int [1:4] 0 8 5 0
  .. ..$ Write fmt      : int [1:4] 2 8 5 0
  .. ..$ Varname        : chr "V11     "
  .. ..$ Varlabel       : chr "Integer, value labels, variable attribute"
  .. ..$ Missing values : NULL
  .. ..$ Typecode       : int 0
  ..$ V12:List of 7
  .. ..$ Number missings: int 0
  .. ..$ Print fmt      : int [1:4] 0 8 1 0
  .. ..$ Write fmt      : int [1:4] 0 8 1 0
  .. ..$ Varname        : chr "V12     "
  .. ..$ Varlabel       : chr "String"
  .. ..$ Missing values : NULL
  .. ..$ Typecode       : int 8
  ..$ V13:List of 7
  .. ..$ Number missings: int 0
  .. ..$ Print fmt      : int [1:4] 0 14 1 0
  .. ..$ Write fmt      : int [1:4] 0 8 1 0
  .. ..$ Varname        : chr "V13     "
  .. ..$ Varlabel       : chr "Long string"
  .. ..$ Missing values : NULL
  .. ..$ Typecode       : int 14
  ..$ V14:List of 7
  .. ..$ Number missings: int 0
  .. ..$ Print fmt      : int [1:4] 0 -1 1 0
  .. ..$ Write fmt      : int [1:4] 0 -1 1 0
  .. ..$ Varname        : chr "V14     "
  .. ..$ Varlabel       : chr "Very long string"
  .. ..$ Missing values : NULL
  .. ..$ Typecode       : int 255
  ..$ V15:List of 7
  .. ..$ Number missings: int 0
  .. ..$ Print fmt      : int [1:4] 0 8 1 0
  .. ..$ Write fmt      : int [1:4] 0 8 1 0
  .. ..$ Varname        : chr "V15     "
  .. ..$ Varlabel       : chr "String, value labels"
  .. ..$ Missing values : NULL
  .. ..$ Typecode       : int 8
  ..$ V16:List of 7
  .. ..$ Number missings: int 0
  .. ..$ Print fmt      : int [1:4] 0 14 1 0
  .. ..$ Write fmt      : int [1:4] 0 8 1 0
  .. ..$ Varname        : chr "V16     "
  .. ..$ Varlabel       : chr "Long string, value labels"
  .. ..$ Missing values : NULL
  .. ..$ Typecode       : int 14
  ..$ V17:List of 7
  .. ..$ Number missings: int 0
  .. ..$ Print fmt      : int [1:4] 0 -1 1 0
  .. ..$ Write fmt      : int [1:4] 0 -1 1 0
  .. ..$ Varname        : chr "V17     "
  .. ..$ Varlabel       : chr "Very long string, value labels"
  .. ..$ Missing values : NULL
  .. ..$ Typecode       : int 255
  ..$ V18:List of 7
  .. ..$ Number missings: int 1
  .. ..$ Print fmt      : int [1:4] 0 8 1 0
  .. ..$ Write fmt      : int [1:4] 0 8 1 0
  .. ..$ Varname        : chr "V18     "
  .. ..$ Varlabel       : chr "String, discrete user missings"
  .. ..$ Missing values : chr "aaaa    "
  .. ..$ Typecode       : int 8
  ..$ V19:List of 7
  .. ..$ Number missings: int 1
  .. ..$ Print fmt      : int [1:4] 0 14 1 0
  .. ..$ Write fmt      : int [1:4] 0 14 1 0
  .. ..$ Varname        : chr "V19     "
  .. ..$ Varlabel       : chr "Long string, discrete user missings"
  .. ..$ Missing values : chr "bbbbbb  "
  .. ..$ Typecode       : int 14
  ..$ V20:List of 7
  .. ..$ Number missings: int 1
  .. ..$ Print fmt      : int [1:4] 0 -1 1 0
  .. ..$ Write fmt      : int [1:4] 0 -1 1 0
  .. ..$ Varname        : chr "V20     "
  .. ..$ Varlabel       : chr "Very long string, discrete user missings"
  .. ..$ Missing values : chr "cccccccc"
  .. ..$ Typecode       : int 255
  ..$ V21:List of 7
  .. ..$ Number missings: int 0
  .. ..$ Print fmt      : int [1:4] 0 8 1 0
  .. ..$ Write fmt      : int [1:4] 0 8 1 0
  .. ..$ Varname        : chr "V21     "
  .. ..$ Varlabel       : chr "String, variable attribute"
  .. ..$ Missing values : NULL
  .. ..$ Typecode       : int 8
  ..$ V22:List of 7
  .. ..$ Number missings: int 1
  .. ..$ Print fmt      : int [1:4] 0 14 1 0
  .. ..$ Write fmt      : int [1:4] 0 8 1 0
  .. ..$ Varname        : chr "V22     "
  .. ..$ Varlabel       : chr "Long string, discrete user missings,copia"
  .. ..$ Missing values : chr "bbbbbb  "
  .. ..$ Typecode       : int 14
 $ Mixed    :List of 12
  ..$ Value labels                                      :List of 3
  .. ..$ :List of 3
  .. .. ..$ Value, label : chr [1:2] "1" "male"
  .. .. ..$ Value, label : chr [1:2] "2" "female"
  .. .. ..$ var number(s): int 8
  .. ..$ :List of 3
  .. .. ..$ Value, label : chr [1:2] "1" "male"
  .. .. ..$ Value, label : chr [1:2] "2" "female"
  .. .. ..$ var number(s): int 9
  .. ..$ :List of 2
  .. .. ..$ Value, label : chr [1:2] "aaaaaaaa" "a8"
  .. .. ..$ var number(s): int 13
  ..$ Sysmis, highest, lowest                           : num [1:3] -1.80e+308  1.80e+308 -1.80e+308
  ..$ Major,minor,revision,floating,endianness,character: int [1:6] 16 0 2 1 2 1252
  ..$ Short and long varnames                           :List of 20
  .. ..$ Short, long: chr [1:2] "V1" "V1"
  .. ..$ Short, long: chr [1:2] "V2" "V2"
  .. ..$ Short, long: chr [1:2] "V3" "V3"
  .. ..$ Short, long: chr [1:2] "V6" "V6"
  .. ..$ Short, long: chr [1:2] "V7" "V7"
  .. ..$ Short, long: chr [1:2] "V8" "V8"
  .. ..$ Short, long: chr [1:2] "V9" "V9"
  .. ..$ Short, long: chr [1:2] "V10" "V10"
  .. ..$ Short, long: chr [1:2] "V11" "V11"
  .. ..$ Short, long: chr [1:2] "V12" "V12"
  .. ..$ Short, long: chr [1:2] "V13" "V13"
  .. ..$ Short, long: chr [1:2] "V14" "V14"
  .. ..$ Short, long: chr [1:2] "V15" "V15"
  .. ..$ Short, long: chr [1:2] "V16" "V16"
  .. ..$ Short, long: chr [1:2] "V17" "V17"
  .. ..$ Short, long: chr [1:2] "V18" "V18"
  .. ..$ Short, long: chr [1:2] "V19" "V19"
  .. ..$ Short, long: chr [1:2] "V20" "V20"
  .. ..$ Short, long: chr [1:2] "V21" "V21"
  .. ..$ Short, long: chr [1:2] "V22" "V22"
  ..$ Variable sets                                     :List of 2
  .. ..$ Set, varnames: chr [1:2] "test" " v1 v2 v3"
  .. ..$ Set, varnames: chr [1:2] "another" " v6 v1 v9"
  ..$ Very long variables                               :List of 3
  .. ..$ Varname, length: chr [1:2] "V14" "260"
  .. ..$ Varname, length: chr [1:2] "V17" "260"
  .. ..$ Varname, length: chr [1:2] "V20" "260"
  ..$ Document                                          : chr "Test file.                                                                      New SPSS 16.0 features.                        "| __truncated__
  ..$ Trend                                             : NULL
  ..$ Display                                           : int [1:69] 3 3 1 3 4 1 3 3 1 3 ...
  ..$ Strings' value labels                             :List of 4
  .. ..$ :List of 3
  .. .. ..$ : chr "V16"
  .. .. ..$ : chr [1:2] "bbbbbbbbbbbbbb" "cccccccccccccc"
  .. .. ..$ : chr [1:2] "b14" "c14"
  .. ..$ :List of 3
  .. .. ..$ : chr "V17"
  .. .. ..$ : chr "ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"| __truncated__
  .. .. ..$ : chr "c260"
  .. ..$ :List of 3
  .. .. ..$ : chr "V19"
  .. .. ..$ : chr "bbbbbb        "
  .. .. ..$ : chr "valore mancante"
  .. ..$ :List of 3
  .. .. ..$ : chr "V22"
  .. .. ..$ : chr "bbbbbb        "
  .. .. ..$ : chr "valore mancante"
  ..$ Encoding                                          : chr "windows-1252"
  ..$ Other                                             : chr ""
 $ Data     : chr [1:60] "1" "1.2" "1" "1.2" ...
}

\code{readSpssSav} ignores unknown miscellaneous informational records when reading SPSS system files.
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

essSample<-readSpssSav("essSample.sav")

essSample<-readSpssSav("essSample.sav",readdata=T)

}
}
\keyword{file}
