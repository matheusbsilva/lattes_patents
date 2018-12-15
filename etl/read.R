library(XML)
library(rowr)
library(dplyr)
library(plyr)

source('utils.R')

ReadCvs <- function(path) {
  setwd(path)
  all.xmls <- list.files(".")
  
  df_patentes <- data.frame()
  
  for(path.xml in all.xmls){
    file <- xmlParse(path.xml)
    root <- xmlRoot(file)
    cv_patentes <- ReadPatente(root)
    if(nrow(cv_patentes) != 0){
      cv_patentes['id_lattes'] <- gsub(".xml", "", path.xml)
      cv_patentes['nome_completo'] <- Validate(root[["DADOS-GERAIS"]], "NOME-COMPLETO")
    }
    
    df_patentes <- rbind.fill(df_patentes, cv_patentes)
  }
  
  df_patentes
}

ReadPatente <- function(root) {
  
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
  df_seq_producao <- XML:::xmlAttrsToDataFrame(getNodeSet(
    root, path='//PATENTE'))
  
  df_autores <- ReadAutores(root, 'PATENTE')
  
  df_patentes <- cbind.no_empty(df_dados_basicos, df_detalhamento, df_registro_ou_patente,
                       df_palavras_chave, df_setores_atividade, df_info_adicional, df_seq_producao)
  
  if(nrow(df_patentes) != 0 && nrow(df_autores) != 0)
    df_patentes <- merge(df_patentes, df_autores, by = 'SEQUENCIA.PRODUCAO')
  
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
  patentes <- root[["PRODUCAO-TECNICA"]][TAG, all = TRUE]
  nome_autores <- "VAZIO"
  nome_citacao_autores <- "VAZIO"
  ordem_autores <- "VAZIO"
  id_autores <- "VAZIO"
  df_autores <- data.frame()
  
  for (patente in patentes) {
    patentes_autores <- patente["AUTORES", all = TRUE]
    
    for (autor in patentes_autores) {
      nome_completo <- Validate(autor, "NOME-COMPLETO-DO-AUTOR")
      nome_citacao <- Validate(autor, "NOME-PARA-CITACAO")
      ordem <- Validate(autor, "ORDEM-DE-AUTORIA")
      idcnpq <- Validate(autor, "NRO-ID-CNPQ")
      
      nome_autores <- SemiColon(nome_completo, nome_autores)
      nome_citacao_autores <- SemiColon(nome_citacao, nome_citacao_autores)
      ordem_autores <- SemiColon(ordem, ordem_autores)
      id_autores <- SemiColon(idcnpq, id_autores)
    }
    df_pat <- data.frame(SEQUENCIA.PRODUCAO = Validate(patente, "SEQUENCIA-PRODUCAO"),
                         NOME.AUTORES=nome_autores,
                         NOME.CITACAO.AUTORES=nome_citacao_autores,
                         ORDEM.AUTORES=ordem_autores,
                         ID.AUTORES=id_autores)
    
    df_autores <- rbind(df_autores, df_pat)
  }
  
  df_autores
}

change.empty <- function(x) {
  result <- ifelse(x == "", 'VAZIO', x)
  result
}