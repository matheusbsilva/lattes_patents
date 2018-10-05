library(xml2)

# Define diretório padrão
setwd("~/Documentos/ds4all/gcee/")

# Caminho de todos os curriculos
all.xmls <- list.files("curriculos/", full = TRUE)
lista.patentes <- list()

for (path.xml in all.xmls) {
  lista.xml <- read_xml(path.xml)
  
  # Busca pela TAG patente em todo o xml
  patentes <- xml_find_all(lista.xml, "//PATENTE")
  
  if (length(patentes) != 0) {
    lista.patentes[[xml_attrs(lista.xml)[["NUMERO-IDENTIFICADOR"]]]] <- as_list(patentes)
  }
}

