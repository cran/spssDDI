`readSpssSav` <-
function(filename,readdata=FALSE) {
 #read Spss Sav metadata and, if readdata=TRUE, hopefully data
 #doesn't read SPSS/PC+ files
 #internal references from pspp-dev.pdf, Appendix B System File Format
 #missing codes (nmissing) not changed (0,1,2,3,-2,-3).  Ranges can be specified only for numeric variables.
 #casesize and ncases assumed correct
 #number of variables in variable record (B.2 Variable Record) and  number of variables in mixed record (B.8 Long Variable Names Record) should be equal if number of variables in mixed record > 0
 #(sometimes they are different, reading with SPSS or pspp  and  writing a new SAV file should fix the problem)

#from SPSS 12
#Long variable names (up to 64 bytes) X X X X X

#from  SPSS 13
#Very long text strings (up to 32,767 bytes) X X X X


#from SPSS 16
#Creation of value labels and missing values on strings of any length
#All string values, including null or blank values, are considered to be valid unless you explicitly define them as missing.
#Missing values for string variables cannot exceed eight bytes. (There is no limit on the defined width of the string variable, but defined missing values
#cannot exceed eight bytes.
#To define null or blank values as missing for a string variable, enter a single space in one of the fields under the Discrete missing values selection.

#from SPSS 16 - Unicode
#First, if your data are all numeric or data and variable names and labels contain only unaccented roman characters, there is nothing to worry about despite the warning.
#Until SPSS 16, characters in a sav file were just assumed to be encoded in a way that is consistent with the locale that SPSS was running in.  Most of the time that is true, and when it isn't, it is usually obvious, because the characters look wrong when displayed.  And since SPSS did not do anything with the bytes other than move them around (with the exception of string functions for changing the case and a few other places), the actual character encoding didn't matter much.  Since Mac and Windows do encode some characters differently, though, this problem has always existed in theory.
#Now in SPSS 16, SPSS supports Unicode, and if you have a sav file from SPSS 15 or later, it knows how the characters are encoded.  It also does more active conversions between the frontend and backend or when moving between platforms.  So it warns  you when the encoding in the file is different from the one in the current SPSS locale.
#This still makes no difference if your text consists only of plain unaccented roman characters.  They are represented the same way everywhere (outside the IBM mainframe).  But Mac and Windows encode other characters differently in some cases.  Generally if you set your SPSS locale to match the sav file (the SET LOCALE command), SPSS will figure everything out whether or not the sav file records the character encoding.
#If you use the new Unicode mode for SPSS, which is off by default but can be turned on in Edit/Options, SPSS will transcode your sav file into Unicode, and then it will understand the text universally and across platforms, and the locale setting will not matter, but older versions of SPSS may not understand the sav file contents.  Again, this will not matter if the file contains only unaccented roman characters.
#In Unicode mode, you can mix characters from any locale without problems (roman characters, Polish, Japanese, Korean, Hebrew, whatever).  There are some syntax incompatibilities with string functions such as substr that are byte oriented, because the bytes can be different in Unicode.  We provide some new char functions to bridge this, but you would have to change the syntax to use them.  Field width for strings may also change for Unicode,  because they are measured in bytes and Unicode text takes more bytes (SPSS automatically adjusts this.)  And Unicode sav files may not be understood by SPSS 15 and earlier (again, not an issue with unaccented roman characters).  That is why we did not make Unicode the default in SPSS 16, but if you need to handle world text, Unicode is a big win.
#The spssaux module and the spssdata module updates also have some additional support for the Unicode mode available in SPSS 16.
#Ranges can be specified only for numeric variables.

#SPSS 16
#Measurement Level. Value labels are primarily useful for categorical (nominal and ordinal)
#variables, and some procedures treat categorical and scale variables differently; so it is sometimes
#important to assign the correct measurement level. However, by default, all new numeric variables
#are assigned the scale measurement level. Thus, many variables that are in fact categorical may
#initially be displayed as scale.

#SPSS 16
#Comments can be any length but are limited to 80 bytes (typically 80 characters in single-byte
#languages) per line; lines will automatically wrap at 80 characters. Comments are displayed in the
#same font as text output to accurately reflect how they will appear when displayed in the Viewer.
#A date stamp (the current date in parentheses) is automatically appended to the end of the
#list of comments whenever you add or modify comments.

# Missings in DDI
#There isn't the notion of a range of missing values between a minimimum and a maximum
#If specific values are used to denote missing values, these can be indicated as a space-delimited list in the missingValue attribute. If the missing value is indicated by a blank, this should be indicated by setting the value of blankIsMissingValue to true.
#The ·lexical space· of NMTOKENS is the set of space-separated lists of token, of which each token is in the ·lexical space· of NMTOKEN.
#The ·lexical space· of NMTOKEN is the set of strings that ·match· the Nmtoken production
#An Nmtoken (name token) is any mixture of name characters.
#Names and Tokens
#[4]            NameChar           ::=          Letter | Digit | '.' | '-' | '_' | ':' | CombiningChar | Extender
#[5]            Name       ::=          (Letter | '_' | ':') ( NameChar)*
#[6]            Names      ::=          Name (#x20 Name)*       /* [E62] */
#[7]            Nmtoken    ::=          (NameChar)+
#[8]            Nmtokens           ::=          Nmtoken (#x20 Nmtoken)* /* [E62] */

#From Foreign, R
#Occasionally in SPSS value labels will be added to some values of a continuous variable (eg to distinguish different types of missing data), and you will not want these variables converted to factors. By setting max.val.labels you can specify that variables with a large number of distinct values are not converted to factors even if they have value labels. In addition, variables will not be converted to factors if there are non-missing values that have no value label. The value labels are then returned in the "value.labels" attribute of the variable.
#If SPSS variable labels are present, they are returned as the "variable.labels" attribute of the answer.
#Fixed length strings (including value labels) are padded on the right with spaces by SPSS, and so are read that way by R. The default argument trim_values=TRUE causes trailing spaces to be ignored when matching to value labels, as examples have been seen where the strings and the value labels had different amounts of padding. See the examples for sub for ways to remove trailing spaces in charcter data.
#Value
#A list (or data frame) with one component for each variable in the saved data set.
#If what looks like a Windows codepage was recorded in the SPSS file, it is attached (as a number) as attribute "codepage" to the result.
#There may be attributes "label.table" and "variable.labels". Attribute "label.table" is a named list of value labels with one element per variable, either NULL or a names character vector. Attribute "variable.labels" is a named character vector with names the short variable names and elements the long names.
#Note
#If SPSS value labels are converted to factors the underlying numerical codes will not in general be the same as the SPSS numerical values, since the numerical codes in R are always 1,2,3,...
#You may see warnings about the file encoding for SPSS save files: it is possible such files contain non-ASCII character data which need re-encoding. The most common occurrence is Windows codepage 1252, a superset of Latin-1. The encoding is recorded (as in integer) in attribute "codepage" of the result if it looks like a Windows codepage.



#### metadata

readHeader<-function(zz,endian) {
  #  read header (B.1 File Header Record)
  rectype<-readChar(zz, 4)
  prodname<-readChar(zz, 60)
  layoutcode<-readBin(zz,integer(),1,endian=endian)
  casesize<-readBin(zz,integer(),1,endian=endian)
  compress<-readBin(zz,integer(),1,endian=endian)
  wgtindex<-readBin(zz,integer(),1,endian=endian)
  numcases<-readBin(zz,integer(),1,endian=endian)
  bias<-readBin(zz,double(),1,endian=endian)
  crdate<-readChar(zz, 9)
  crtime<-readChar(zz, 8)
  filelabel<-readChar(zz, 64)
  tmp<-readChar(zz,3)
  return(list(RecordType=rectype,ProductName=prodname,LayoutCode=layoutcode,
    CaseSize=casesize,Compress=compress, WeightIndex=wgtindex,NumCases=numcases,
    Bias=bias,CreationDate=crdate,CreationTime=crtime,FileLabel=filelabel))
}

readVariables<-function(zz,endian,casesize) {
  # read variables (B.2 Variable Record)
  vars_output<-list()
  typecodes<-list()
  varcases<-list()
  k<-0
  for (i in 1:casesize) {
    var_output<-list()
    recordtype<-readBin(zz,integer(),1,endian=endian)
    typecode<-readBin(zz,integer(),1,endian=endian)
    typecodes[[i]]<-typecode
    if (typecode != -1) {
      k<-k+1
      haslabel<-readBin(zz,integer(),1,endian=endian)
      nmissing<-readBin(zz,integer(),1,endian=endian)
      var_output[[1]]<-nmissing
      printfmt<-readBin(zz,integer(),4,1,endian=endian)
      var_output[[2]]<-printfmt
      writefmt<-readBin(zz,integer(),4,1,endian=endian)
      var_output[[3]]<-writefmt
      varname<-readChar(zz,8)
      var_output[[4]]<-varname
      if (haslabel == 1) {
        labellen<-readBin(zz,integer(),1,endian=endian)
        tmp_len<-labellen
        tmp_mod<-(labellen %% 4)
        if (tmp_mod != 0) {
          tmp_len<-tmp_len + (4-tmp_mod)
        }
        varlabel<-substr(readChar(zz,tmp_len),1,labellen)
        var_output[[5]]<-varlabel
      }
      if (nmissing != 0)  {
        misslen<-  abs(nmissing)
        if (typecode == 0) {
          missings<-readBin(zz,double(),misslen,endian=endian)
        } else {
            missings<-c()
            for (ki in 1:misslen) {
              missings[ki]<-readChar(zz,8)
            }
        }
        var_output[[6]]<-missings
      }
      var_output[[7]]<-typecode
      vars_output[[k]]<-var_output
      varcases[[k]]<-i
    } else {
      haslabel<-readBin(zz,integer(),1,endian=endian)
      nmissing<-readBin(zz,integer(),1,endian=endian)
      tmp<-readBin(zz,integer(),4,1,endian=endian)
      tmp<-readBin(zz,integer(),4,1,endian=endian)
      tmp<-readChar(zz,8)
      if (haslabel == 1) {
        labellen<-readBin(zz,integer(),1,endian=endian)
        tmp_len<-labellen
        tmp_mod<-(labellen %% 4)
        if (tmp_mod != 0) {
          tmp_len<-tmp_len + (4-tmp_mod)
        }
        varlabel<-readChar(zz,tmp_len)
      }
      if (nmissing != 0)  {
        misslen<-  abs(nmissing)
        missings<-readBin(zz,double(),misslen,endian=endian)
      }
    }
  }
  return(list(typecodes,vars_output,varcases))
}

readMix<-function(zz,endian,varcases) {
  # read mixed record
  valuelabels<-list()
  n<-1
  end_header<-0
  mix<-list()
  while(end_header==0) {
    rectype<-readBin(zz,integer(),1,endian=endian)
    if  (rectype == "3") {
      valuelabels[[n]]<-readValueLabels(zz,endian,varcases)
      n<-n+1
    } else {
      if  (rectype == "6") {
        mix[[7]]<-readDocument(zz,endian)
      } else {
        if  (rectype == "7") {
          subtype<-readBin(zz,integer(),1,endian=endian)
          if (subtype == "4") {
            mix[[2]]<-readMissings(zz,endian)
          } else {
            if (subtype == "3") {
              mix[[3]]<-readSystem(zz,endian)
            } else {
              if (subtype == "13") {
                mix[[4]]<-readLongVar(zz,endian)
              } else {
                if (subtype == "5") {
                  mix[[5]]<-readVarSet(zz,endian)
                } else {
                  if (subtype == "14") {
                    mix[[6]]<-readVeryLongString(zz,endian)
                  } else {
                    if (subtype == "6") {
                      mix[[8]]<-readTrend(zz,endian)
                    } else {
                      if (subtype == "11") {
                        mix[[9]]<-readDisplay(zz,endian)
                      } else {
                        if (subtype == "21") {
                          mix[[10]]<-readValueLabelsString(zz,endian)
                        } else {
                          if (subtype == "20") {
                            mix[[11]]<-readEncoding(zz,endian)
                          } else {
                            mix[[12]]<-readMisc(zz,endian)
#                            print(paste("warning: unknown type 7 subtype ",subtype," record",sep=""))
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        } else {
          if (rectype == "999") {
            # B.11 Dictionary Termination Record
            tmp<-readBin(zz,integer(),1,endian=endian)
            mix[[12]]<-""
            end_header<-1
          }
        }
      }
    }
  }
  mix[[1]]<-valuelabels
  return(mix)
}

readValueLabels<-function(zz,endian,varcases) {
  # read value labels (B.3 Value Labels Records)
  recs<-list()
  n<-length(varcases)
  count<-readBin(zz,integer(),1,endian=endian)
  for (i in 1:count) {
    value<-readBin(zz,double(),1,endian=endian)
    labellen<-readBin(zz, integer(), 1,1,endian=endian)
    tmp_len<-labellen
    tmp_mod<-((tmp_len+1) %% 8)
    if (tmp_mod != 0) {
      tmp_len<-tmp_len + (8-tmp_mod)
    }
    label<-substr(readChar(zz,tmp_len),1,labellen)
    recs[[i]]<-c(value,label)
  }
  rectype<-readBin(zz,integer(),1,endian=endian)
  count<-readBin(zz,integer(),1,endian=endian)
  vars<-c()
  for (i in 1:count) {
    vars[i]<-readBin(zz,integer(),1,endian=endian)
    for (j in 1:n) {
      if (vars[i] == varcases[[j]]) {
        vars[i]<-j
        break
      }
    }
  }
  n<-length(recs)+1
  recs[[n]]<-vars
  return(recs)
}


readMix1<-function(zz,endian,varcases,variables,mix) {
# read mixed record
  mixold<-mix
  valuelabels<-list()
  n<-1
  end_header<-0
  mix<-list()
  while(end_header==0) {
    rectype<-readBin(zz,integer(),1,endian=endian)
    if  (rectype == "3") {
      valuelabels[[n]]<-readValueLabels1(zz,endian,varcases,variables,mixold,n)
      n<-n+1
    } else {
      if  (rectype == "6") {
        mix[[7]]<-readDocument(zz,endian)
      } else {
        if  (rectype == "7") {
          subtype<-readBin(zz,integer(),1,endian=endian)
          if (subtype == "4") {
            mix[[2]]<-readMissings(zz,endian)
          } else {
            if (subtype == "3") {
              mix[[3]]<-readSystem(zz,endian)
            } else {
              if (subtype == "13") {
                mix[[4]]<-readLongVar(zz,endian)
              } else {
                if (subtype == "5") {
                  mix[[5]]<-readVarSet(zz,endian)
                } else {
                  if (subtype == "14") {
                    mix[[6]]<-readVeryLongString(zz,endian)
                  } else {
                    if (subtype == "6") {
                      mix[[8]]<-readTrend(zz,endian)
                    } else {
                      if (subtype == "11") {
                        mix[[9]]<-readDisplay(zz,endian)
                      } else {
                        if (subtype == "21") {
                          mix[[10]]<-readValueLabelsString1(zz,endian,mixold)
                        } else {
                          if (subtype == "20") {
                            mix[[11]]<-readEncoding(zz,endian)
                          } else {
                            mix[[12]]<-readMisc(zz,endian)
                            print(paste("warning: unknown type 7 subtype ",subtype," record",sep=""))
                            print(mix[[12]])
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        } else {
          if (rectype == "999") {
            # B.11 Dictionary Termination Record
            tmp<-readBin(zz,integer(),1,endian=endian)
            mix[[12]]<-""
            end_header<-1
          }
        }
      }
    }
  }
  mix[[1]]<-valuelabels
  return(mix)
}

readValueLabels1<-function(zz,endian,varcases,variables,mix,z) {
  # read value labels (B.3 Value Labels Records)
  lmix<-length(mix[[1]][[z]])
#  print(c("lmix",lmix))
  imix<-(mix[[1]][[z]][[lmix]])
#  print(c("imix",imix))
  itype<-(variables[[imix[1]]][[7]])
#  print(c("itype",itype))
  recs<-list()
  n<-length(varcases)
  count<-readBin(zz,integer(),1,endian=endian)
  for (i in 1:count) {
    if (itype == 0) {
      value<-readBin(zz,double(),1,endian=endian)
    } else {
      value<-readChar(zz,8)
    }
    labellen<-readBin(zz, integer(), 1,1,endian=endian)
    tmp_len<-labellen
    tmp_mod<-((tmp_len+1) %% 8)
    if (tmp_mod != 0) {
      tmp_len<-tmp_len + (8-tmp_mod)
    }
    label<-substr(readChar(zz,tmp_len),1,labellen)
    recs[[i]]<-c(value,label)
  }
  rectype<-readBin(zz,integer(),1,endian=endian)
  count<-readBin(zz,integer(),1,endian=endian)
  vars<-c()
  for (i in 1:count) {
    vars[i]<-readBin(zz,integer(),1,endian=endian)
    for (j in 1:n) {
      if (vars[i] == varcases[[j]]) {
        vars[i]<-j
        break
      }
    }
  }
  n<-length(recs)+1
  recs[[n]]<-vars
  return(recs)
}

readDocument<-function(zz,endian) {
  # read Document (B.4 Document Record)
  count<-readBin(zz,integer(),1,endian=endian)
  document<-readChar(zz,count*80)
  return(document)
}
readTrend<-function(zz,endian) {
# read Trend information (record type 7 subtype 6)
  datalen<-readBin(zz,integer(),1,endian=endian)
  count<-readBin(zz,integer(),1,endian=endian)
  trend<-readBin(zz,integer(),count,endian=endian)
  return(trend)
}

readSystem<-function(zz,endian) {
  #read version and system  information (B.5 Machine Integer Info Record)
  datalen<-readBin(zz,integer(),1,endian=endian)
  count<-readBin(zz,integer(),1,endian=endian)
  major<-readBin(zz,integer(),1,endian=endian)
  minor<-readBin(zz,integer(),1,endian=endian)
  revision<-readBin(zz,integer(),1,endian=endian)
  machine<-readBin(zz,integer(),1,endian=endian)
  floating<-readBin(zz,integer(),1,endian=endian)
  compression<-readBin(zz,integer(),1,endian=endian)
  endianness<-readBin(zz,integer(),1,endian=endian)
  character<-readBin(zz,integer(),1,endian=endian)
  return(c(major,minor,revision,floating,endianness,character))
}

readLongVar<-function(zz,endian) {
  #read long and short var names (B.8 Long Variable Names Record)
  datalen<-readBin(zz,integer(),1,endian=endian)
  count<-readBin(zz,integer(),1,endian=endian)
  string<-readChar(zz, count)
  stmp<-unlist(strsplit(string,"\t",fixed=TRUE))
  strings<-(strsplit(stmp,"=",fixed=TRUE))
  return(strings)
}

readVarSet<-function(zz,endian) {
  #read var set information (record type 7 subtype 5)
  datalen<-readBin(zz,integer(),1,endian=endian)
  count<-readBin(zz,integer(),1,endian=endian)
  string<-readChar(zz, count)
  stmp<-unlist(strsplit(string,"\n",fixed=TRUE))
  strings<-(strsplit(stmp,"=",fixed=TRUE))
  return(strings)
}

readMissings<-function(zz,endian) {
  # read system missing values (B.6 Machine Floating-Point Info Record)
  datalen<-readBin(zz,integer(),1,endian=endian)
  count<-readBin(zz,integer(),1,endian=endian)
  sysmis<-readBin(zz,double(),1,endian=endian)
  highest<-readBin(zz,double(),1,endian=endian)
  lowest<-readBin(zz,double(),1,endian=endian)
  return(c(sysmis,highest,lowest))
}

readVeryLongString<-function(zz,endian) {
  # read very long string (B.9 Very Long String Record)
  datalen<-readBin(zz,integer(),1,endian=endian)
  count<-readBin(zz,integer(),1,endian=endian)
  rawstring<-charToRaw(readChar(zz,count))
  rawtmp<-c()
  j=1
  for (i in 1:count) {
    if (rawstring[i] != "00") {
      rawtmp[j]<-rawstring[i];j<-j+1
    }
  }
  tmp<-rawToChar(rawtmp)
  stmp<-unlist(strsplit(tmp,"\t",fixed=TRUE))
  strings<-(strsplit(stmp,"=",fixed=TRUE))
  return(strings)
}

readDisplay<-function(zz,endian) {
  # read display information (B.7 Variable Display Parameter Record)
  datalen<-readBin(zz,integer(),1,endian=endian)
  count<-readBin(zz,integer(),1,endian=endian)
  display<-readBin(zz,integer(),count,endian=endian)
  return(display)
}

readValueLabelsString1<-function(zz,endian,mix) {
# read value labels of string variables >8characters
  datalen<-readBin(zz,integer(),1,endian=endian)
  count<-readBin(zz,integer(),1,endian=endian)
  i<-1
  outlist<-list()
  k<-1
  while (i < (datalen*count))   {
    vvalue<-c()
    vlabel<-c()
    z<-1
    lvar<-readBin(zz,integer(),1,endian=endian)
    i<-i+4
    var<-readChar(zz,lvar)
    i<-i+lvar
    tmp<-readBin(zz,integer(),1,endian=endian)
    i<-i+4
    nvaluelabels<-readBin(zz,integer(),1,endian=endian)
    i<-i+4
    for (j in 1:nvaluelabels) {
      lvalue<-readBin(zz,integer(),1,endian=endian)
      i<-i+4
      value<-readChar(zz,lvalue)
      i<-i+lvalue
      lvaluelabel<-readBin(zz,integer(),1,endian=endian)
      i<-i+4
      label<-readChar(zz,lvaluelabel)
      i<-i+lvaluelabel
      vlabel[z]<-label
      vvalue[z]<-value
      z<-z+1
    }
    outlist[[k]]<-list(var,vvalue,vlabel)
    k<-k+1
  }
  return(outlist)
}


readValueLabelsString<-function(zz,endian) {
# read value labels of string variables >8characters
  datalen<-readBin(zz,integer(),1,endian=endian)
      count<-readBin(zz,integer(),1,endian=endian)
      tmp<-readChar(zz,count*datalen)
      return(tmp)
}

readEncoding<-function(zz,endian) {
# read encoding (record 7 subtype 20)
  datalen<-readBin(zz,integer(),1,endian=endian)
      count<-readBin(zz,integer(),1,endian=endian)
      encoding<-readChar(zz,count*datalen)
      return(encoding)
}


readMisc<-function(zz,endian) {
  # read other information
  datalen<-readBin(zz,integer(),1,endian=endian)
  count<-readBin(zz,integer(),1,endian=endian)
  tmp<-readChar(zz,count*datalen)
  return(tmp)
}

weigthFix<-function(weigth,varcases) {
  # change  weight index to reflect variable's position
  n<-length(varcases)
  w1<-weigth
  for (i in 1:n) {
    if (weigth == varcases[[i]]) {
      w1<-i
    }
  }
  return(w1)
}


#### Data

readSpssData<-function(zz,endian,casesize,ncases,typecodes) {
  # read uncompressed data
  numchar<-0
  n<-casesize*ncases
  m <- character(n)
  i<-0
  v<-0
  while (i<n)  {
     i<-i+1
    k<-(i %% casesize)
    if (k == 0) {
      k<- casesize
    }
    if (typecodes[[k]][1]== 0 && numchar == 0) {
      v<-v+1
      m[v]<- readBin(zz,double(),1,endian=endian)
    } else {
        if (typecodes[[k]][1] > 0 && typecodes[[k]][1] < 9 && numchar == 0) {
        v<-v+1
        m[v]<- readChar(zz, 8)
      } else {
          if (typecodes[[k]][1] > 8 && numchar == 0)  {
            if ((typecodes[[k]][1] %% 8) == 0){
              numchar<-typecodes[[k]][1]
            } else {
              numchar<-typecodes[[k]][1] - (typecodes[[k]][1] %% 8)   + 8
            }
          v<-v+1
          m[v]<-readChar(zz, 8)
          numchar<-numchar-8
          vlong<-v
        } else {
            if (typecodes[[k]][1] == -1 && numchar > 0) {
                ctmp<-readChar(zz, 8)
                numchar<-numchar-8
                m[vlong]<-paste(m[vlong],ctmp,sep="")
            } else {
                  if (numchar >  0) {
                    ctmp<-readChar(zz, 8)
                    numchar<-numchar-8
                    m[vlong]<-paste(m[vlong],ctmp,sep="")
                    i<-i-1
                  }
            }
        }
      }
    }
  }
  while (numchar >  0) {
    ctmp<-readChar(zz, 8)
    numchar<-numchar-8
    m[vlong]<-paste(m[vlong],ctmp,sep="")
  }
  return(m[1:v])
}

readSpssDataCompressed<-function(zz,endian,casesize,ncases,typecodes,sysmis) {
  # read compressed data
  numchar<-0
  n<-casesize*ncases
  m <- character(n)
  i<-0
  v<-0
  while (i<n) {
    r<-readBin(zz,integer(),8,1,FALSE,endian=endian)
    for (j in 1:8) {
      if (r[j]>0 &&  r[j]<252 && i<n) {
        i<-i+1
        k<-(i %% casesize)
        if (k == 0) {
          k<- casesize
        }
        v<-v+1
        m[v]<-(r[j]-100)
#        print(c(m[[v]],v,i,"A"))
      }
      if (r[j] == 253 && i<n) {
        i<-i+1
        k<-(i %% casesize)
        if (k == 0) {
          k<- casesize
        }
        if (typecodes[[k]][1]== 0 && numchar == 0) {
          v<-v+1
          m[v]<- readBin(zz,double(),1,endian=endian)
#          print(c(m[[v]],v,i,"B"))
        } else {
          if (typecodes[[k]][1] > 0 && typecodes[[k]][1] < 9 && numchar == 0) {
            v<-v+1
            m[v]<- readChar(zz, 8)
#            print(c(m[[v]],v,i,"C"))
          } else {
            if (typecodes[[k]][1] > 8 && numchar == 0)  {
              if ((typecodes[[k]][1] %% 8) == 0){
                numchar<-typecodes[[k]][1]
              } else {
                numchar<-typecodes[[k]][1] - (typecodes[[k]][1] %% 8)   + 8
              }
              v<-v+1
              m[v]<-readChar(zz, 8)
#              print(c(m[[v]],v,i,"D"))
              numchar<-numchar-8
              vlong<-v
            } else {
              if (typecodes[[k]][1] == -1 && numchar > 0) {
                ctmp<-readChar(zz, 8)
                numchar<-numchar-8
               m[vlong]<-paste(m[vlong],ctmp,sep="")
              } else {
                if (numchar >  0) {
                  ctmp<-readChar(zz, 8)
                  numchar<-numchar-8
                  m[vlong]<-paste(m[vlong],ctmp,sep="")
                  i<-i-1
                }
              }
            }
          }
        }
      }
      if (r[j] == 252 && i<n) {
      }
      if (r[j] == 254 && i<n) {
        i<-i+1
        k<-(i %% casesize)
        if (k == 0) {
          k<- casesize
        }
        if (typecodes[[k]][1] > 0 && typecodes[[k]][1] < 9 && numchar == 0) {
          v<-v+1
          m[v]<- "        "
#          print(c(m[[v]],v,i,"E"))
        } else {
            if (typecodes[[k]][1] > 8 && numchar == 0)  {
              if ((typecodes[[k]][1] %% 8) == 0){
                numchar<-typecodes[[k]][1]
              } else {
                  numchar<-typecodes[[k]][1] - (typecodes[[k]][1] %% 8)   + 8
              }
              v<-v+1
              m[v]<-"        "
#              print(c(m[[v]],v,i,"F"))
              numchar<-numchar-8
              vlong<-v
            } else {
                if (typecodes[[k]][1] == -1 && numchar > 0) {
                  ctmp<-"        "
                  numchar<-numchar-8
                  m[vlong]<-paste(m[vlong],ctmp,sep="")
                } else {
                    if (numchar >  0) {
                      ctmp<-"        "
                      numchar<-numchar-8
                      m[vlong]<-paste(m[vlong],ctmp,sep="")
                      i<-i-1
                    }
                }
            }
        }
      }
      if (r[j] == 255 && i<n) {
        i<-i+1
        v<-v+1
        m[v]<- sysmis
#        print(c(m[[v]],v,i,"G"))
      }
      if (r[j] == 0 && i<n) {
      }
    }
  }
  return(m[1:v])
}



#### other functions

dataFix<-function(data,veryLongVar,shortvarnames,ncases) {
# fix very long string in data
  n<-length(data)
  nvar<-length(shortvarnames)
  ind<-veryLongVarIndex(veryLongVar,shortvarnames)
  l<-character(n)
  v<-1
  i<-0
  while (i < n) {
    w<-0
    i<-i+1
    k<-(v %% nvar)
    if (k == 0) {
      k<- nvar
    }
    if (k %in% ind) {
      j<-match(k,ind)
      w<-1
      stringlength<- as.integer(veryLongVar[[j]][2])
      nsegments<-(stringlength+251)%/%252
      elem<-substr(data[i],1,255)
      nsegments<-nsegments-1
      while (nsegments > 0) {             #
        i<-i+1
        elem<-paste(elem,substr(data[i],1,255),sep="")
      nsegments<-nsegments - 1
      }
      l[v]<-trimTrailingSpaces(elem)
      w<-1
      v<-v+1
    }
    if (w == 0) {
      l[v]<-trimTrailingSpaces(data[i])
      v<-v+1
    }
  }
  if (ncases*nvar != (v-1))  {
    stop("Sorry, cannot read this Sav file")
  }
  return(l[1:(v-1)])
}


trimTrailingSpaces<-function(s) {
  # trim trailing spaces
  n<-nchar(s)
  sTrimmed<-""
  while (substr(s,n,n) == " ") {
    n<-n-1
  }
  return(substr(s,1,n))
}

newIndex<-function(newoldvar,ncases,n1) {
  # index to select elements
  vec<-c()
  k<-1
  n<-length(newoldvar)
  for (i in 1:ncases) {
    for (j in 1:n) {
      vec[k]<-newoldvar[j]+(i-1)*n1
      k<-k+1
    }
  }
  return(vec)
}



veryLongVarIndex<-function(veryLongVar,shortvarnames) {
  # find index of very long variables in the sequence of variables in a case
  l<-list()
  nvlv<-length(veryLongVar)
  nsvn<-length(shortvarnames)
  for (i in 1:nvlv) {
    for (j in 1:nsvn) {
      if (veryLongVar[[i]][1] == shortvarnames[[j]]) {
        l[[i]]<-j
      }
    }
  }
  return(l)
}

mix1Fix<-function(mix,newoldvar) {
  # change var index in value labels
  n<-length(newoldvar)
  nmix<-length(mix)
  out<-c()
  for (i in 1:nmix) {
    vmix<-mix[i]
    for (j in 1:n) {
      if (vmix == newoldvar[j]) {
        out[i]<-j
      }
    }
  }
  return(out)
}

variablesFix<-function(variables,shortvarnames) {
# fix variables if there are "very long" variables (> 255 characters, SPSS > 12)
  n<-length(shortvarnames)
  nl<-length(variables)
  vec<-c()
  for (i in 1:n) {
    lengthName<-nchar(shortvarnames[[i]])
    for (j in 1:nl) {
      trimmedName<-trimTrailingSpaces(variables[[j]][[4]])
      if (shortvarnames[[i]] == trimmedName) {
        vec[i]<-j
      }
    }
  }
  return(list(vec,variables[vec]))
}

##MAIN

  #system endianness
  endian<-.Platform$endian

  #open the spss Sav file in binary mode
  zz <- file(filename, "rb")

  #read header record (B.1 File Header Record) (SPSS Record Type 1 - General information)
  #RecordType, ProductName, LayoutCode, CaseSize,Compress,WeightIndex,NumCases,Bias,CreationDate,CreationTime,FileLabel
  header<-readHeader(zz,endian)

  # check if Sav file
  if (header[[1]] != "$FL2") {
    close(zz); return("This file does not appear to be an SPSS SAV file")
  }

  # check if same endianness, else  swap; close file (header[[3]]=layout code normally 2; sometimes 3)
  if (header[[3]] < 2 || header[[3]] > 3) {
    endian <- "swap"
    close(zz)
  } else {
    close(zz)
  }

  #open the spss Sav file in binary mode  again
  zz <- file(filename, "rb")

  # read header record again
  header<-readHeader(zz,endian)

  # check if same endianness, else  stop
  if (header[[3]] < 2 || header[[3]] > 3) {close(zz); return("Sorry, endianness problem") }

  # check  casesize (Number of data elements per case), ncases (Number of cases); check bias=100
  if (header[[4]] == -1) {close(zz); return("Sorry, casesize set to -1")}
  if (header[[7]] == -1) {close(zz); return("Sorry, ncases set to -1")}
  if (header[[8]] != 100) {close(zz); return("Sorry, bias not equal 100")}

  # create temporary variables
  casesize<-header[[4]]

  # read variables' metadata
  out<-readVariables(zz,endian,casesize)
  varcases<-out[[3]]    # list relating data elements  (the elements in the list) to vars  (the elements' order)

  # read other information
  mix<-readMix(zz,endian,varcases)

  #print warning if UTF-8
      if (!is.null(mix[[11]]) && mix[[11]]=="UTF-8") {
        print("warning: Unicode UTF-8 not catered for. Some characters may be unreadable")
      }

  # close Sav file
  close(zz)

   #open the spss Sav file in binary mode  third time
  zz <- file(filename, "rb")

  # read header record again
  header<-readHeader(zz,endian)

  # create temporary variables
  casesize<-header[[4]]
  compress<-header[[5]]
  ncases<-header[[7]]

  # read variables' metadata
  out<-readVariables(zz,endian,casesize)
  typecodes<-out[[1]]   # type of data elements
  variables<-out[[2]]   # var names and labels
  varcases<-out[[3]]    # list relating data elements  (the elements in the list) to vars  (the elements' order)

  # read other information
  # recognizes numbers or string using information in mix
  mix<-readMix1(zz,endian,varcases,variables,mix)

# create sysmis temporary variable
  sysmis<-mix[[2]][1]

  # if TRUE, read compressed or uncompressed data
  if (readdata) {
    if (compress == 1) {
      data<-readSpssDataCompressed(zz,endian,casesize,ncases,typecodes,sysmis)
    } else {
      data<-readSpssData(zz,endian,casesize,ncases,typecodes)
    }
  }



# close Sav file
  close(zz)

# create list containing short var names (if SPSS > 11)
  shortvarnames<- mapply(function(x) x[[1]] ,mix[[4]],  SIMPLIFY = FALSE)



# fix weight value
  header[[6]]<-weigthFix(header[[6]],varcases)

  # fix metadata if there are "very long" variables (> 255 characters, SPSS > 12)
  if (!is.null(mix[[6]])) {
    out<-variablesFix(variables,shortvarnames)
    newoldvar<-out[[1]]
    variables<-out[[2]]
    header[[6]]<-ifelse(is.na(match(header[[6]],newoldvar)),0, match(header[[6]],newoldvar))
    if (length(mix[[1]])>0) {
      for (i in 1:length(mix[[1]])) {
        nmix<-length(mix[[1]][[i]])
        mix[[1]][[i]][[nmix]]<-mix1Fix(mix[[1]][[i]][[nmix]],newoldvar)
      }
    }
  }

  # fix number of vars
    header[[4]]<-length(variables)

# fix data if there are "very long" variables (> 255 characters, SPSS > 12)
  if (!is.null(mix[[6]]) && readdata) {
      data<-dataFix(data,mix[[6]],shortvarnames,ncases)
  }
  # check if shortevarnames and variables have the same length
  if (length(shortvarnames)>0) {
    if (length(variables) != length(shortvarnames)) {
      return("Number of variables not correct. Reading and writing the SAV file with SPSS or pspp may fix the problem")
    }
   }

  # make output list, if TRUE, list with 4 elements; otherwise with 3 elements
  if (readdata) {
    out<-list(Header=header,Variables=variables,Mixed=mix,Data=data)
  } else {
    out<-list(Header=header,Variables=variables,Mixed=mix)
  }

  # create list attributes
  if (length(shortvarnames)>0) {
    attr(out[[2]],"names")<-shortvarnames
    for (i in 1:length(shortvarnames)) {
       attr(out[[2]][[i]],"names")<-c("Number missings","Print fmt","Write fmt","Varname","Varlabel","Missing values","Typecode")
    }
   }

  attr(out[[3]],"names")<-c("Value labels", "Sysmis, highest, lowest", "Major,minor,revision,floating,endianness,character",
      "Short and long varnames", "Variable sets", "Very long variables","Document","Trend","Display","Strings\' value labels ","Encoding","Other")

  if (length(out[[3]][[1]])>0) {
    for (i in 1:length(out[[3]][[1]])) {
          attr(out[[3]][[1]][[i]],"names")<-c(rep("Value, label", length(out[[3]][[1]][[i]])-1),"var number(s)")
    }
  }

  if (length(out[[3]][[4]])>0) {
    attr(out[[3]][[4]],"names")<-c(rep("Short, long", length(out[[3]][[4]])))
  }

  if (length(out[[3]][[5]])>0) {
    attr(out[[3]][[5]],"names")<-c(rep("Set, varnames", length(out[[3]][[5]])))
  }

  if (length(out[[3]][[6]])>0) {
    attr(out[[3]][[6]],"names")<-c(rep("Varname, length", length(out[[3]][[6]])))
  }

# return list
  return(out)


}

