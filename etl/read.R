
ReadCvs <- function(path) {
  setwd(path)
  all.xmls <- list.files(".")
  
  df_patentes <- data.frame()
  
  for(path.xml in all.xmls){
    file <- xmlParse(path.xml)
    root <- xmlRoot(file)
    cv_patentes <- ReadPatente(root)
    
    df_patentes <- rbind(df_patentes, cv_patentes)
  }
}


ReadPatente <- function(root) {
  df_autores <- ReadAutores(root, 'PATENTE')
  
  df_dados_basicos <- XML:::xmlAttrsToDataFrame(
    getNodeSet(root, path='//PATENTE/DADOS-BASICOS-DA-PATENTE'))
  df_detalhamento <- XML:::xmlAttrsToDataFrame(
    getNodeSet(root, path='//PATENTE/DETALHAMENTO-DA-PATENTE')) 
  df_registro_ou_patente <- XML:::xmlAttrsToDataFrame(getNodeSet(
    root, path='//PATENTE/DETALHAMENTO-DA-PATENTE/REGISTRO-OU-PATENTE'))
  df_historico_situacoes <- XML:::xmlAttrsToDataFrame(getNodeSet(
    root, path='//PATENTE/DETALHAMENTO-DA-PATENTE/HISTORICO-SITUACOES-PATENTE'))
  df_palavras_chave <- XML:::xmlAttrsToDataFrame(getNodeSet(
    root, path='//PATENTE/PALAVRAS-CHAVE'))
  df_setores_atividade <- XML:::xmlAttrsToDataFrame(getNodeSet(
    root, path='//PATENTE/SETORES-DE-ATIVIDADE'))
  df_info_adicional <- XML:::xmlAttrsToDataFrame(getNodeSet(
    root, path='//PATENTE/INFORMACOES-ADICIONAIS'))
  
  df_patentes <- cbind(df_dados_basicos, df_detalhamento, df_registro_ou_patente,
                       df_historico_situacoes, df_palavras_chave, df_setores_atividade,
                       df_info_adicional)
  
  df_patentes
}

ReadAutores <- function(root, TAG) {
  xpath <- paste('//', TAG, '/AUTORES', sep = "")
  df_autores <- XML:::xmlAttrsToDataFrame(getNodeSet(root, path=xpath))

  # Change empty values to 'VAZIO'
  df_autores[] = lapply(df_autores, function(x) ifelse(x == "", 'VAZIO', x))
  
  df_autores
}