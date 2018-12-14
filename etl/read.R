library(XML)
library(rowr)
library(dplyr)

ReadCvs <- function(path) {
  setwd(path)
  all.xmls <- list.files(".")
  
  df_patentes <- data.frame()
  
  for(path.xml in all.xmls){
    file <- xmlParse(path.xml)
    root <- xmlRoot(file)
    cv_patentes <- ReadPatente(root)
    
    df_patentes <- bind_rows(df_patentes, cv_patentes)
  }
  
  df_patentes
}

ReadPatente <- function(root) {
  #df_autores <- ReadAutores(root, 'PATENTE')
  
  df_dados_basicos <- XML:::xmlAttrsToDataFrame(
    getNodeSet(root, path='//PATENTE/DADOS-BASICOS-DA-PATENTE'))
  df_detalhamento <- XML:::xmlAttrsToDataFrame(
    getNodeSet(root, path='//PATENTE/DETALHAMENTO-DA-PATENTE')) 
  df_registro_ou_patente <- XML:::xmlAttrsToDataFrame(getNodeSet(
    root, path='//PATENTE/DETALHAMENTO-DA-PATENTE/REGISTRO-OU-PATENTE'))
  df_palavras_chave <- XML:::xmlAttrsToDataFrame(getNodeSet(
    root, path='//PATENTE/PALAVRAS-CHAVE'))
  df_setores_atividade <- XML:::xmlAttrsToDataFrame(getNodeSet(
    root, path='//PATENTE/SETORES-DE-ATIVIDADE'))
  df_info_adicional <- XML:::xmlAttrsToDataFrame(getNodeSet(
    root, path='//PATENTE/INFORMACOES-ADICIONAIS'))
  
  df_patentes <- cbind.no_empty(df_dados_basicos, df_detalhamento, df_registro_ou_patente,
                       df_palavras_chave, df_setores_atividade, df_info_adicional)

  df_patentes
}

cbind.no_empty <- function(...) {
  result = data.frame()
  for(item in list(...)) {
    if(nrow(result) == 0) {
      result <- item
      next
    }
    if(nrow(item) == 0) next
    result <- cbind.fill(result, item)
  }
  
  result
}

ReadAutores <- function(root, TAG) {
  xpath <- paste('//', TAG, '/AUTORES', sep = "")
  df_autores <- XML:::xmlAttrsToDataFrame(getNodeSet(root, path=xpath))

  # Change empty values to 'VAZIO'
  df_autores[] = lapply(df_autores, function(x) ifelse(x == "", 'VAZIO', x))
  
  df_autores
}