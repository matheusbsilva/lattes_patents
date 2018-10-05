library(xml2)
library(dplyr)
library(XML)

# Define diretório padrão
setwd("~/Documentos/ds4all/gcee/")

# Caminho de todos os curriculos
all.xmls <- list.files("teste/", full = TRUE)
lista.patentes <- list()

for (path.xml in all.xmls) {
  lista.xml <- xmlToList(path.xml)
  
  if(is.null(lista.xml[["PRODUCAO-TECNICA"]][["PATENTE"]])) next
  
  for (item in names(lista.xml[["PRODUCAO-TECNICA"]])) {
    if (item == "PATENTE") {                        
      lista.patentes[lista.xml[[".attrs"]][["NUMERO-IDENTIFICADOR"]]] <- list(lista.xml[["PRODUCAO-TECNICA"]][item])
    }
  }
  
  #lista.patentes[lista.xml[[".attrs"]][["NUMERO-IDENTIFICADOR"]]] <- list(lista.xml[["PRODUCAO-TECNICA"]])
  
}

