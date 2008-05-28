`writeDDI` <-
function(l,spssSavFile,studyunit="ID",maxNumberMissings=100) {
###### write DDI
# l is  readSpssSav's output; spssSavFile the spss file; studyunit is the study's ID string
# SPSS var may have names that are invalid in the DDI instance (".", "#", ...) so in <l:Variable id="varname">  varname is "V"+number from 1 to number of variables
#Characters from different encodings in general are not treated correctly. Not a problem with unaccented roman characters, but see note in readSpssSav.

#DataRelationship defines which variables comprise a logical record
# A link to a LogicalRecord in a DataRelationship is required by all PhysicalStructure descriptions. 
# In its simplest form a DataRelationship for a microdata file (variables) must contain the following:
# <DataRelationship isIdentifiable=”true” id=”XX”>
# <LogicalRecord isIdentifiable=”true” id=”YY” hasLocator=”false”>
# <VariablesInRecord allVariablesInLogicalProduct=”true”/>
# </LogicalRecord>
# </DataRelationship>
# This states that all the variables in the logical product are part of a single logical record which has no variable field that identifies its record type.
# This is the structure used by most simple surveys.

#isWeight indicates whether the variable is a weight; true if the variable is the SPSS weight variable
#isWeight="xs:boolean [0..1]" 
#Indicates whether the variable is a weight

#  <xs:import namespace="ddi:physicaldataproduct_proprietary:3_0_Beta" schemaLocation="physicaldataproduct_proprietary.xsd"/>
#  <!-- Commented out because this is a beta - remove comments to include, and add an xmlns attribute with a prefix of "m4" -->
# <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns="ddi:instance:3_0" xmlns:m4="ddi:physicaldataproduct_proprietary:3_0_Beta" 

writeCategoryScheme<-function(valuelabels,variables,longValueLabels,studyunit) {
# write CategoryScheme
  cat(paste("<l:CategoryScheme id=\"",studyunit,"_CategoryScheme","\">\n",sep=""))
  nGroupsOfLongValueLabels<-length(longValueLabels)
  nGroupsOfValueLabels<-length(valuelabels)
  if (nGroupsOfValueLabels > 0) {
        for (i in 1:nGroupsOfValueLabels) {
                nValueLabels<-length(valuelabels[[i]])
                  listVariables<- valuelabels[[i]][[nValueLabels]]
                for (j in 1:(nValueLabels-1)) {
                        writeCategory(listVariables,valuelabels[[i]][[j]],variables,i,j,studyunit)
                }
          }
   }
  if (nGroupsOfLongValueLabels > 0) {
        for (i in 1:nGroupsOfLongValueLabels) {
      writeLongCategories(longValueLabels[[i]],i,studyunit)
          }
   }
  cat("</l:CategoryScheme>\n")
}

writeLongCategories<-function(longValueLabels,i,studyunit) {
# write categories  for long value labels
  for (j in 1:length(longValueLabels[[3]])) {
    cat(paste("<l:Category  id=\"",studyunit,"_Category_Long_",i,"_",j,"\">\n",sep=""))
    cat("<r:Label>")
    cat(sanitize(as.character(longValueLabels[[3]][[j]])))
    cat("</r:Label>\n")
        cat("</l:Category>\n")
  }
}

writeCategory<-function(listVariables,valuelabel,variables,i,j,studyunit) {
# write a category
  for (k in listVariables) {
    if (variables[[k]][[1]] == 0) {
          cat(paste("<l:Category  id=\"",studyunit,"_Category_",i,"_",k,"_",j,"\">\n",sep=""))
      cat("<r:Label>")
      cat(sanitize(as.character(valuelabel[2])))
      cat("</r:Label>\n")
      cat("</l:Category>\n")
    }
    if (variables[[k]][[1]] > 0) {
                  if (valuelabel[1] %in% variables[[k]][[6]]) {
                  cat(paste("<l:Category missing=\"true\" id=\"",studyunit,"_Category_",i,"_",k,"_",j,"\">\n",sep=""))
        cat("<r:Label>")
        cat(sanitize(as.character(valuelabel[2])))
        cat("</r:Label>\n")
        cat("</l:Category>\n")
                  } else {
                  cat(paste("<l:Category  id=\"",studyunit,"_Category_",i,"_",k,"_",j,"\">\n",sep=""))
        cat("<r:Label>")
        cat(sanitize(as.character(valuelabel[2])))
        cat("</r:Label>\n")
        cat("</l:Category>\n")
      }
    }
    if (variables[[k]][[1]] == -2) {
                  if (as.integer(valuelabel[1]) >= variables[[k]][[6]][1] && as.integer(valuelabel[1]) <= variables[[k]][[6]][2]) {
                  cat(paste("<l:Category missing=\"true\" id=\"",studyunit,"_Category_",i,"_",k,"_",j,"\">\n",sep=""))
        cat("<r:Label>")
        cat(sanitize(as.character(valuelabel[2])))
        cat("</r:Label>\n")
        cat("</l:Category>\n")
                  } else {
        cat(paste("<l:Category  id=\"",studyunit,"_Category_",i,"_",k,"_",j,"\">\n",sep=""))
        cat("<r:Label>")
        cat(sanitize(as.character(valuelabel[2])))
        cat("</r:Label>\n")
        cat("</l:Category>\n")
                  }
    }
    if (variables[[k]][[1]] == -3) {
                  if ((as.integer(valuelabel[1]) == variables[[k]][[6]][3]) || (as.integer(valuelabel[1]) >= variables[[k]][[6]][1] && as.integer(valuelabel[1]) <= variables[[k]][[6]][2])) {
                  cat(paste("<l:Category missing=\"true\" id=\"",studyunit,"_Category_",i,"_",k,"_",j,"\">\n",sep=""))
        cat("<r:Label>")
        cat(sanitize(as.character(valuelabel[2])))
        cat("</r:Label>\n")
        cat("</l:Category>\n")
                  } else {
        cat(paste("<l:Category  id=\"",studyunit,"_Category_",i,"_",k,"_",j,"\">\n",sep=""))
        cat("<r:Label>")
        cat(sanitize(as.character(valuelabel[2])))
        cat("</r:Label>\n")
        cat("</l:Category>\n")
      }
    }
  }
}


writeCodeSchemes<-function(valuelabels,variables,longValueLabels,studyunit) {
# write CodeSchemes
  nGroupsOfValueLabels<-length(valuelabels)
  nGroupsOfLongValueLabels<-length(longValueLabels)
  if (nGroupsOfValueLabels > 0) {
    for (i in 1:nGroupsOfValueLabels) {
      nValueLabels<-length(valuelabels[[i]])
      listVariables<- valuelabels[[i]][[nValueLabels]]
                  for (j in listVariables) {
                    writeCodeScheme(nValueLabels,valuelabels,variables,i,j,studyunit)
                  }
    }
  }
  if (nGroupsOfLongValueLabels > 0) {
    for (i in 1:nGroupsOfLongValueLabels) {
      writeLongCodeScheme(longValueLabels[[i]],i,studyunit)
          }
  }
}

writeLongCodeScheme<-function(longValueLabels,i,studyunit) {
# write a Long CodeScheme
  cat(paste("<l:CodeScheme id=\"",studyunit,"_CodeScheme_Long_",i,"\">\n",sep=""))
  for (j in 1:length(longValueLabels[[2]])) {
    cat("<l:Code>\n")
    cat("<l:CategoryReference>\n")
    cat("<r:ID>")
        cat(paste(studyunit,"_Category_Long_",i,"_",j,sep=""))
    cat("</r:ID>\n")
    cat("</l:CategoryReference>\n")
    cat("<l:Value>")
    cat(sanitize(as.character(longValueLabels[[2]][[j]])))
    cat("</l:Value>\n")
    cat("</l:Code>\n")
  }
  cat("</l:CodeScheme>\n")
}


writeCodeScheme<-function(nValueLabels,valuelabels,variables,i,j,studyunit) {
# write a CodeScheme
  cat(paste("<l:CodeScheme id=\"",studyunit,"_CodeScheme_",i,"_",j,"\">\n",sep=""))
        for (k in 1:(nValueLabels-1)) {
          writeCode(valuelabels[[i]][[k]],variables[[j]],i,j,k,studyunit)
        }
  cat("</l:CodeScheme>\n")
}

writeCode<-function(valuelabel,variable,i,j,k,studyunit) {
# write a code
  cat("<l:Code>\n")
  cat("<l:CategoryReference>\n")
  cat("<r:ID>")
        cat(paste(studyunit,"_Category_",i,"_",j,"_",k,sep=""))
  cat("</r:ID>\n")
  cat("</l:CategoryReference>\n")
  cat("<l:Value>")
  cat(sanitize(as.character(valuelabel[1])))
  cat("</l:Value>\n")
  cat("</l:Code>\n")
}



writeVariableScheme<-function(valuelabels,variables,longValueLabels,shortLongVarNames,studyunit,weight,maxNumberMissings) {
  nVariables<-length(variables)
  nGroupsOfValueLabels<-length(valuelabels)
  nGroupsOfLongValueLabels<-length(longValueLabels)
  nShortLongVarNames<-length(shortLongVarNames)
  cat(paste("<l:VariableScheme id=\"",studyunit,"_VariableScheme\">\n",sep=""))
  for (i in 1:nVariables) {
    ind<-0
    indLong<-0
    varName<-variables[[i]][[4]]
    varLabel<-variables[[i]][[5]]
    varTypeCode<-variables[[i]][[7]]
    nMissings<-variables[[i]][[1]]
    missingValues<-variables[[i]][[6]]
    if (i == weight){
     cat(paste("<l:Variable isWeight=\"true\" id=\"V",i,"\">\n",sep=""))   
    }  else {
    cat(paste("<l:Variable id=\"V",i,"\">\n",sep=""))    
    }
    cat("<r:Name>")
    cat(trimTrailingSpaces(varName))
    cat("</r:Name>\n")
    cat("<r:Label type=\"label\">")
    cat(sanitize(as.character(varLabel)))
    cat("</r:Label>\n")
    if (!is.null(shortLongVarNames)) {
      cat("<r:Label type=\"name\">")
      cat(shortLongVarNames[[i]][[2]])
      cat("</r:Label>\n")
    }
    cat("<l:VariableDefinition>")
    if (variables[[i]][[1]] == 0) {
      cat("No missing values")
    }
    if (variables[[i]][[1]] ==1) {
      cat(paste("One discrete missing value: ",variables[[i]][[6]][[1]],sep=""))
    }
    if (variables[[i]][[1]] == 2) {
      cat(paste("Two discrete missing values: ",variables[[i]][[6]][[1]]," and ",variables[[i]][[6]][[2]],sep=""))
    }
    if (variables[[i]][[1]] == 3) {
      cat(paste("Three discrete missing values: ",variables[[i]][[6]][[1]],", ",variables[[i]][[6]][[2]]," and ",variables[[i]][[6]][[3]],sep=""))
    }
    if (variables[[i]][[1]] == -2) {
      cat(paste("Missing values between ",variables[[i]][[6]][[1]]," and ",variables[[i]][[6]][[2]],sep=""))
    }
    if (variables[[i]][[1]] == -3) {
      cat(paste("Missing values between ",variables[[i]][[6]][[1]]," and ",variables[[i]][[6]][[2]],"; One discrete missing value: ",variables[[i]][[6]][[3]],sep=""))
    }
    cat("</l:VariableDefinition>\n")
    cat("<l:Representation>\n")
    if (nGroupsOfValueLabels > 0) {
      ind<-writeVariableCodeRepresentation(nGroupsOfValueLabels,valuelabels,i,studyunit,varTypeCode,nMissings,missingValues,maxNumberMissings)
    } 
    if (nGroupsOfLongValueLabels > 0) {
      indLong<-writeLongVariableCodeRepresentation(nGroupsOfLongValueLabels,longValueLabels,shortLongVarNames[[i]],studyunit,varTypeCode,nMissings,missingValues,maxNumberMissings)
    } 
    if (ind==0 &&  indLong==0){
      writeRepresentation(i,studyunit,varTypeCode,nMissings,missingValues,maxNumberMissings)  
    }
    cat("</l:Representation>\n")
    cat("</l:Variable>\n")
  }
  cat("</l:VariableScheme>\n")
}

writeRepresentation<-function(i,studyunit,varTypeCode,nMissings,missingValues,maxNumberMissings) {
  if (varTypeCode == 0) {  
    if (nMissings == 0) {
      cat(paste("<l:NumericRepresentation type=\"Decimal\"/>\n",sep=""))
    }
    if (nMissings > 0) {
      cat("<l:NumericRepresentation type=\"Decimal\" missingValue=\"")
      for (i in missingValues) cat(paste(i," ",sep=""))
      cat("\"/>\n")
    }
    if (nMissings == -2) {
      cat("<l:NumericRepresentation type=\"Decimal\"")
      if (abs(missingValues[2]-missingValues[1]) < maxNumberMissings) {
        cat(" missingValue=\"")
        for (i in missingValues[1]:missingValues[2]) cat(paste(i," ",sep=""))
        cat("\"/>\n")
      } else {
        cat("/>\n")      
      }
    }
    if (nMissings == -3) {
      cat("<l:NumericRepresentation type=\"Decimal\"")
      if (abs(missingValues[2]-missingValues[1]) < maxNumberMissings) {
        cat(" missingValue=\"")
        for (i in missingValues[1]:missingValues[2]) cat(paste(i," ",sep=""))
        cat(missingValues[3])
        cat("\"/>\n")
      } else {
        cat("/>\n")      
      }
    }
  } else {
    if (nMissings == 0) {
      cat(paste("<l:TextRepresentation maxLength=\"",varTypeCode,"\"/>\n",sep=""))
    }
    if (nMissings > 0) {
      cat(paste("<l:TextRepresentation maxLength=\"",varTypeCode,"\" missingValue=\"",sep=""))
      for (i in missingValues) cat(paste(i," ",sep=""))
      cat("\"/>\n")
    }
  }
}

writeVariableCodeRepresentation<-function(nGroupsOfValueLabels,valuelabels,i,studyunit,varTypeCode,nMissings,missingValues,maxNumberMissings) {
  for (j in 1:nGroupsOfValueLabels) {
    nValueLabels<-length(valuelabels[[j]])
    if (i %in% valuelabels[[j]][[nValueLabels]]) {
      if (nMissings == 0) {
        cat(paste("<l:CodeRepresentation>\n",sep=""))
      }
      if (nMissings > 0) {
        cat("<l:CodeRepresentation missingValue=\"")
        for (k in missingValues) cat(paste(k," ",sep=""))
        cat("\">\n")
      }
      if (nMissings == -2) {
        cat("<l:CodeRepresentation")
        if (abs(missingValues[2]-missingValues[1]) < maxNumberMissings) {
          cat(" missingValue=\"")
          for (k in missingValues[1]:missingValues[2]) cat(paste(k," ",sep=""))
          cat("\">\n")
        } else {
          cat(">\n")      
        }
      }
      if (nMissings == -3) {
        cat("<l:CodeRepresentation")
        if (abs(missingValues[2]-missingValues[1]) < maxNumberMissings) {
          cat(" missingValue=\"")
          for (k in missingValues[1]:missingValues[2]) cat(paste(k," ",sep=""))
          cat(missingValues[3])
          cat("\">\n")
        } else {
          cat(">\n")      
        }
      }
      cat("<r:CodeSchemeReference>\n<r:ID>")
      cat(paste(studyunit,"_CodeScheme_",j,"_",i,sep=""))
      cat("</r:ID>\n</r:CodeSchemeReference>\n</l:CodeRepresentation>\n")
      return(1)
    }
  }
  return(0)
}

writeLongVariableCodeRepresentation<-function(nGroupsOfLongValueLabels,longValueLabels,shortLongVarName,studyunit,varTypeCode,nMissings,missingValues,maxNumberMissings) {
  for (j in 1:nGroupsOfLongValueLabels) {
    if (shortLongVarName[[2]] == longValueLabels[[j]][[1]]) {
      if (nMissings == 0) {
        cat(paste("<l:CodeRepresentation>\n",sep=""))
      }
      if (nMissings > 0) {
        cat("<l:CodeRepresentation missingValue=\"")
        for (i in missingValues) cat(paste(i," ",sep=""))
        cat("\">\n")
      }
      if (nMissings == -2) {
        cat("<l:CodeRepresentation")
        if (abs(missingValues[2]-missingValues[1]) < maxNumberMissings) {
          cat(" missingValue=\"")
          for (i in missingValues[1]:missingValues[2]) cat(paste(i," ",sep=""))
          cat("\">\n")
        } else {
          cat(">\n")      
        }
      }
      if (nMissings == -3) {
        cat("<l:CodeRepresentation")
        if (abs(missingValues[2]-missingValues[1]) < maxNumberMissings) {
          cat(" missingValue=\"")
          for (i in missingValues[1]:missingValues[2]) cat(paste(i," ",sep=""))
          cat(missingValues[3])
          cat("\">\n")
        } else {
          cat(">\n")      
        }
      }
      cat("<r:CodeSchemeReference>\n<r:ID>")
      cat(paste(studyunit,"_CodeScheme_Long_",j,sep=""))
      cat("</r:ID>\n</r:CodeSchemeReference>\n</l:CodeRepresentation>\n")
      return(1)
    }
  }
  return(0)
}


writeDDIheader<-function(studyunit) {
# write DDI header
  cat("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n")
  cat(paste("<DDIInstance id=\"",studyunit,"_01\" xsi:schemaLocation=\"ddi:instance:3_0 instance.xsd\" xmlns=\"ddi:instance:3_0\" xmlns:l=\"ddi:logicalproduct:3_0\" xmlns:pi=\"ddi:physicalinstance:3_0\" xmlns:p=\"ddi:physicaldataproduct:3_0\" xmlns:m4=\"ddi:physicaldataproduct_proprietary:3_0_Beta\"  xmlns:s=\"ddi:studyunit:3_0\" xmlns:a=\"ddi:archive:3_0\" xmlns:o=\"ddi:organization:3_0\" xmlns:r=\"ddi:reusable:3_0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n",sep=""))
  cat(paste("<s:StudyUnit id=\"",studyunit,"_02\">\n",sep=""))
}

writeCitation<-function(filelabel) {
  cat("<r:Citation>\n")
  cat("<r:Title>")
  cat(sanitize(filelabel))
  cat("</r:Title>\n")
  cat("</r:Citation>\n")
}

writeAbstract<-function(studyunit,document) {
  cat(paste("<s:Abstract id=\"",studyunit,"_03\">\n",sep=""))
  cat("<r:Content>")
  cat(sanitize(document))
  cat("</r:Content>\n")
  cat("</s:Abstract>\n")
}

writeUniverseReference<-function() {
  cat("<r:UniverseReference>\n")
  cat("<r:ID>Unavailable</r:ID>\n")
  cat("</r:UniverseReference>\n")
}

writePurpose<-function(studyunit) {
  cat(paste("<s:Purpose id=\"",studyunit,"_04\">\n",sep=""))
  cat("<r:Content>The purpose of the study, in human-readable form</r:Content>\n")
  cat("</s:Purpose>\n")
}

writeLogicalProduct<-function(studyunit) {
  cat(paste("<l:LogicalProduct id=\"",studyunit,"_05\">\n",sep=""))
  cat(paste("<l:DataRelationship isIdentifiable=\"true\" id=\"",studyunit,"_05_1\">\n",sep=""))
  cat(paste("<l:LogicalRecord isIdentifiable=\"true\" hasLocator=\"false\" id=\"",studyunit,"_05_1_1\">\n",sep=""))
  cat("<l:VariablesInRecord allVariablesInLogicalProduct=\"true\">\n")
  cat("<l:VariableSchemeReference>\n")
  cat("<r:ID>")
  cat(paste(studyunit,"_VariableScheme",sep=""))
  cat("</r:ID>\n")
  cat("</l:VariableSchemeReference>\n")
  cat("</l:VariablesInRecord>\n")
  cat("</l:LogicalRecord>\n")
  cat("</l:DataRelationship>\n")
}

writePhysicalDataProduct<-function(nCases,nVariables,spssSavFile,studyunit,l) {
# Write PhysicalDataProduct
  cat("</l:LogicalProduct>\n")
  cat(paste("<p:PhysicalDataProduct id=\"",studyunit,"_06\">\n",sep=""))
  cat(paste("<p:PhysicalStructureScheme id=\"",studyunit,"_06_1\">\n",sep=""))
  cat(paste("<p:PhysicalStructure id=\"",studyunit,"_06_1_1\">\n",sep=""))
  cat("<p:LogicalProductReference>\n")
  cat("<r:ID>")
  cat(paste(studyunit,"_05",sep=""))
  cat("</r:ID>\n")
  cat("</p:LogicalProductReference>\n")
  cat("<p:Format>SPSS</p:Format>\n")
  cat("<p:DefaultDecimalSeparator>.</p:DefaultDecimalSeparator>\n")
  cat(paste("<p:GrossRecordStructure isIdentifiable=\"true\" numberOfPhysicalSegments=\"1\" id=\"",studyunit,"_06_1_1_1\">\n",sep=""))
  cat("<p:LogicalRecordReference>\n")
  cat("<r:ID>")
  cat(paste(studyunit,"_05_1_1",sep=""))
  cat("</r:ID>\n")
  cat("</p:LogicalRecordReference>\n")
  cat(paste("<p:PhysicalRecordSegment hasSegmentKey=\"false\" isIdentifiable=\"true\" segmentOrder=\"1\" id=\"",studyunit,"_06_1_1_1_1\"/>\n",sep=""))
  cat("</p:GrossRecordStructure>\n")
  cat("</p:PhysicalStructure>\n")
  cat("</p:PhysicalStructureScheme>\n")
#  writeRecordLayoutScheme(nCases,nVariables,spssSavFile,studyunit)
  writeRecordLayoutSchemeSPSS(nCases,nVariables,spssSavFile,studyunit,l)
  cat("</p:PhysicalDataProduct>\n")
}

writeRecordLayoutSchemeSPSS<-function(nCases,nVariables,spssSavFile,studyunit,l) {
# Write   RecordLayoutScheme    
  cat(paste("<p:RecordLayoutScheme id=\"",studyunit,"_06_2\">\n",sep=""))
  cat(paste("<m4:ProprietaryRecordLayout id=\"",studyunit,"_06_2_1\">\n",sep=""))
  cat("<p:PhysicalStructureReference>\n")
  cat("<r:ID>")
  cat(paste(studyunit,"_06_1_1",sep=""))
  cat("</r:ID>\n")
  cat("<p:PhysicalRecordSegmentUsed>")
  cat(paste(studyunit,"_06_1_1_1_1",sep=""))
  cat("</p:PhysicalRecordSegmentUsed>\n")
  cat("</p:PhysicalStructureReference>\n")
  cat("<p:CharacterSet>")
  cat(ifelse(is.null(l[[3]][[11]]),l[[3]][[3]][6],l[[3]][[11]][1]))
  cat("</p:CharacterSet>\n")
  cat("<p:ArrayBase>1</p:ArrayBase>\n")
  cat("<r:Software>")
  cat("<r:Name>")
  cat("SPSS")
  cat("</r:Name>\n")
  cat("<r:Version>")
  cat(paste(l[[3]][[3]][1],".",l[[3]][[3]][2],".",l[[3]][[3]][3],sep=""))
  cat("</r:Version>\n")
  cat("<r:Description>")
  cat(l[[1]][[2]])  
  cat("</r:Description>\n")
  cat("</r:Software>\n")
  cat("<m4:DefaultVariableSchemeReference>\n")
  cat("<r:ID>")
  cat(paste(studyunit,"_VariableScheme",sep=""))
  cat("</r:ID>\n")
  cat("</m4:DefaultVariableSchemeReference>\n")

  cat("<r:ProprietaryInfo>")
  cat("<r:ProprietaryProperty name=\"Compression\">")
  cat(l[[1]][[5]])
  cat("</r:ProprietaryProperty>\n")
  cat("<r:ProprietaryProperty name=\"CompressionBias\">")
  cat(l[[1]][[8]])
  cat("</r:ProprietaryProperty>\n")
#  cat("<r:ProprietaryProperty name=\"MachineCode\">")
#  cat(l[[1]][[2]])
#  cat("</r:ProprietaryProperty>\n")
  cat("<r:ProprietaryProperty name=\"FloatingPointRepresentation\">")
  cat(l[[3]][[3]][4])
  cat("</r:ProprietaryProperty>\n")
  cat("<r:ProprietaryProperty name=\"Endianness\">")
  cat(l[[3]][[3]][5])
  cat("</r:ProprietaryProperty>\n")
  cat("<r:ProprietaryProperty name=\"CharacterSet\">")
  cat(ifelse(is.null(l[[3]][[11]]),l[[3]][[3]][6],l[[3]][[11]][1]))
  cat("</r:ProprietaryProperty>\n")
  cat("<r:ProprietaryProperty name=\"Sysmiss\">")
  cat(l[[3]][[2]][1])
  cat("</r:ProprietaryProperty>\n")
  cat("<r:ProprietaryProperty name=\"HighestSysmissRecode\">")
  cat(l[[3]][[2]][2])
  cat("</r:ProprietaryProperty>\n")
  cat("<r:ProprietaryProperty name=\"LowestSysmissRecode\">")
  cat(l[[3]][[2]][3])
  cat("</r:ProprietaryProperty>\n")
  cat("</r:ProprietaryInfo>\n")
  for (i in 1:nVariables) {
    cat("<m4:DataItem>\n")
    cat("<m4:VariableReference>\n")
    cat("<r:ID>")
    cat(paste("V",i,sep=""))
    cat("</r:ID>\n")
    cat("</m4:VariableReference>\n")
    cat("<m4:ProprietaryDataType>")
    cat(ifelse(l[[2]][[i]][[7]]==0,"numeric","string"))
    cat("</m4:ProprietaryDataType>\n")
#    cat("<m4:ProprietaryOutputFormat>")
#    cat(1)
#    cat("</m4:ProprietaryOutputFormat>\n")
    cat("<r:ProprietaryInfo>\n")
#    cat("<r:ProprietaryProperty name=\"Width\">")
#    cat(1)
#    cat("</r:ProprietaryProperty>\n")
#    cat("<r:ProprietaryProperty name=\"Decimals\">")
#    cat(1)
#    cat("</r:ProprietaryProperty>\n")
    cat("<r:ProprietaryProperty name=\"MissingFormatCode\">")
    cat(l[[2]][[i]][[1]])
    cat("</r:ProprietaryProperty>\n")
    if (l[[2]][[i]][[1]] != 0) {
      for (j in 1:abs(l[[2]][[i]][[1]])) {
        cat(paste("<r:ProprietaryProperty name=\"MissingValue",j,"\">",sep=""))
        cat(l[[2]][[i]][[6]][j])
        cat("</r:ProprietaryProperty>\n")
      }
    }
#    cat("<r:ProprietaryProperty name=\"DisplayWidth\">")
#    cat(1)
#    cat("</r:ProprietaryProperty>\n")
#    cat("<r:ProprietaryProperty name=\"Alignment\">")
#    cat(1)
#    cat("</r:ProprietaryProperty>\n")
#    cat("<r:ProprietaryProperty name=\"Measure\">")
#    cat(1)
#    cat("</r:ProprietaryProperty>\n")
    cat("</r:ProprietaryInfo>\n")
    cat("</m4:DataItem>\n")
  }
  cat("</m4:ProprietaryRecordLayout>\n") 
  cat("</p:RecordLayoutScheme>\n")
}

writeRecordLayoutScheme<-function(nCases,nVariables,spssSavFile,studyunit) {
# Write   RecordLayoutScheme    
  cat(paste("<p:RecordLayoutScheme id=\"",studyunit,"_06_2\">\n",sep=""))
  cat(paste("<p:RecordLayout id=\"",studyunit,"_06_2_1\">\n",sep=""))
  cat("<p:PhysicalStructureReference>\n")
  cat("<r:ID>")
  cat(paste(studyunit,"_06_1_1",sep=""))
  cat("</r:ID>\n")
  cat("<p:PhysicalRecordSegmentUsed>")
  cat(paste(studyunit,"_06_1_1_1_1",sep=""))
  cat("</p:PhysicalRecordSegmentUsed>\n")
  cat("</p:PhysicalStructureReference>\n")
  cat("<p:CharacterSet>")
  cat("ASCII")
  cat("</p:CharacterSet>\n")
  cat("<p:ArrayBase>1</p:ArrayBase>\n")
  cat("<p:DefaultVariableSchemeReference>\n")
  cat("<r:ID>")
  cat(paste(studyunit,"_VariableScheme",sep=""))
  cat("</r:ID>\n")
  cat("</p:DefaultVariableSchemeReference>\n")
  for (i in 1:nVariables) {
    cat("<p:DataItem>\n")
    cat("<p:VariableReference>\n")
    cat("<r:ID>")
    cat(paste("V",i,sep=""))
    cat("</r:ID>\n")
    cat("</p:VariableReference>\n")
    cat("<p:PhysicalLocation>\n")
    cat("</p:PhysicalLocation>\n")
    cat("</p:DataItem>\n")
  }
  cat("</p:RecordLayout>\n") 
  cat("</p:RecordLayoutScheme>\n")
}


writePhysicalInstance<-function(nCases,nVariables,spssSavFile,studyunit) {
# Write PhysicalInstance
  cat(paste("<pi:PhysicalInstance id=\"",studyunit,"_07\">\n",sep=""))
  cat(paste("<pi:RecordLayoutReference>\n",sep=""))
  cat("<r:ID>")
  cat(paste(studyunit,"_06",sep=""))
  cat("</r:ID>\n")
  cat("</pi:RecordLayoutReference>\n")
  cat(paste("<pi:DataFileIdentification id=\"",studyunit,"_08\">\n",sep=""))
  cat("<pi:URI>")
  cat(spssSavFile)
  cat("</pi:URI>\n")
  cat("</pi:DataFileIdentification>\n")
  cat(paste("<pi:GrossFileStructure  id=\"",studyunit,"_09\">\n",sep=""))
  cat("<pi:CaseQuantity>")
  cat(nCases)
  cat("</pi:CaseQuantity>\n")
  cat("<pi:OverallRecordCount>")
  cat(nCases)
  cat("</pi:OverallRecordCount>\n")
  cat("</pi:GrossFileStructure >\n")
  cat("</pi:PhysicalInstance>\n")
}

writeDDIfooter<-function() {
# Write DDI footer
        cat("</s:StudyUnit>\n</DDIInstance>\n")
}


sanitize <- function(str) {
  result <- str
  result <- gsub("&","&amp;",result,fixed=TRUE)
  result <- gsub(">","&gt;",result,fixed=TRUE)
  result <- gsub("<","&lt;",result,fixed=TRUE)
  return(result)
}

sanitizeVar <- function(str) {
  result <- str
  result <- gsub(".","_",result,fixed=TRUE)
  result <- gsub("#","_",result,fixed=TRUE)
  return(result)
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


## MAIN

  nCases<-l[[1]][[7]]
  weight<-l[[1]][[6]]
  variables<-l[[2]]
  nVariables<-length(variables)
  valuelabels<-l[[3]][[1]]
  sysmis<-l[[3]][[2]][1]
  crdate<-l[[1]][[9]]
  longValueLabels<-l[[3]][[10]]
  shortLongVarNames<-l[[3]][[4]]
  filelabel<-ifelse(is.null(l[[1]][[11]]),"Title",l[[1]][[11]])
  document<-ifelse(is.null(l[[3]][[7]]),"Abstract",l[[3]][[7]])
  writeDDIheader(studyunit)
  writeCitation(filelabel)
  writeAbstract(studyunit,document)
  writeUniverseReference()
  writePurpose(studyunit)
  writeLogicalProduct(studyunit)
  writeCategoryScheme(valuelabels,variables,longValueLabels,studyunit)
  writeCodeSchemes(valuelabels,variables,longValueLabels,studyunit)
  writeVariableScheme(valuelabels,variables,longValueLabels,shortLongVarNames,studyunit,weight,maxNumberMissings)
  writePhysicalDataProduct(nCases,nVariables,spssSavFile,studyunit,l)
  writePhysicalInstance(nCases,nVariables,spssSavFile,studyunit)
  writeDDIfooter()



}

