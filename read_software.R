setwd("~/Documentos/UNB/2-18/ds/git/lattes_patents/curriculos")


library(XML)
library(xlsx)
library(dplyr)


#################################################
# Função para validar xml attr do E-lattes
Validate <- function (element, field) {
  result <- ""
  
  tryCatch(
    result <- xmlGetAttr(element, field)  ,
    error = function(e) {
      
    }
  )
  
  if (is.null(result) || is.na(result))
    result <- ""
  result
}

# Função para ponto vírgula e espaço
SemiColon <- function (field, target){ 
  if (is.null(field) || field == "") {
    target <- paste(target, "VAZIO", sep = "; ")
    
  }
  else{
    if (target != "VAZIO") {
      target <- paste(target, field, sep = "; ")
    }
    else{
      target <- paste(field, sep = "; ")
    }
  }
  
  target
}
################################################

all.xmls <- list.files(".")
#path <- "0000325690951570.xml"
df_pat <- data.frame()

for (path.xml in all.xmls) {
  file <- xmlParse(path.xml)
  #file <- xmlParse(path)
  root <- xmlRoot(file)
  df_aux <- NULL
  
  softwares <- root[["PRODUCAO-TECNICA"]]["SOFTWARE", all = TRUE]
  nome_autores <- "VAZIO"
  nome_citacao_autores <- "VAZIO"
  ordem_autores <- "VAZIO"
  id_autores <- "VAZIO"
  
  for (software in softwares) {
    software_autores <- software["AUTORES", all = TRUE]
    for (autor in software_autores) {
      nome_completo <- Validate(autor, "NOME-COMPLETO-DO-AUTOR")
      nome_citacao <- Validate(autor, "NOME-PARA-CITACAO")
      ordem <- Validate(autor, "ORDEM-DE-AUTORIA")
      idcnpq <- Validate(autor, "NRO-ID-CNPQ")
      
      nome_autores <- SemiColon(nome_completo, nome_autores)
      nome_citacao_autores <- SemiColon(nome_citacao, nome_citacao_autores)
      ordem_autores <- SemiColon(ordem, ordem_autores)
      id_autores <- SemiColon(idcnpq, id_autores)
    }
    
    
    # variáveis das tags
    
    DADOS_BASICOS <- "DADOS-BASICOS-DO-SOFTWARE"
    DETALHAMENTO <- "DETALHAMENTO-DO-SOFTWARE"
    
    df_aux <- data.frame(
      # Informações básicas
      id_lattes = gsub(".xml", "", path.xml),
      nome_completo = Validate(root[["DADOS-GERAIS"]], "NOME-COMPLETO"),
      id_patente = Validate(software[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "CODIGO-DO-REGISTRO-OU-PATENTE"),
      
      # TAG: DADOS-BASICOS-DO-SOFTWARE
      natureza = Validate(software[[DADOS_BASICOS]], "NATUREZA"),
      titulo_do_software = Validate(software[[DADOS_BASICOS]], "TITULO-DO-SOFTWARE"),
      ano = Validate(software[[DADOS_BASICOS]], "ANO"),
      pais = Validate(software[[DADOS_BASICOS]], "PAIS"),
      idioma = Validate(software[[DADOS_BASICOS]], "IDIOMA"),
      meio_de_divulgacao = Validate(software[[DADOS_BASICOS]], "MEIO-DE-DIVULGACAO"),
      home_page_do_trabalho = Validate(software[[DADOS_BASICOS]], "HOME-PAGE-DO-TRABALHO"),
      flag_relevancia = Validate(software[[DADOS_BASICOS]], "FLAG-RELEVANCIA"),
      doi = Validate(software[[DADOS_BASICOS]], "DOI"),
      titulo_do_software_ingles = Validate(software[[DADOS_BASICOS]], "TITULO-DO-SOFTWARE-INGLES"),
      flas_divulgacao_cientifica = Validate(software[[DADOS_BASICOS]], "FLAG-DIVULGACAO-CIENTIFICA"),
      flag_potencial_inovacao = Validate(software[[DADOS_BASICOS]], "FLAG-POTENCIAL-INOVACAO"),
      
      # TAG: DETALHAMENTO-DO-SOFTWARE
      finalidade = Validate(software[[DETALHAMENTO]], "FINALIDADE"),
      plataforma = Validate(software[[DETALHAMENTO]], "PLATAFORMA"),
      ambiente = Validate(software[[DETALHAMENTO]], "AMBIENTE"),
      disponibilidade = Validate(software[[DETALHAMENTO]], "DISPONIBILIDADE"),
      instituicao_financiadora = Validate(software[[DETALHAMENTO]], "INSTITUICAO-FINANCIADORA"),
      finalidade_ingles = Validate(software[[DETALHAMENTO]], "FINALIDADE-INGLES"),
      
      # TAG: DETALHAMENTO-DO-SOFTWARE > REGISTRO-OU-PATENTE
      #WARN: Informação duplicada, TITULO-PATENTE
      tipo_patente = Validate(software[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "TIPO-PATENTE"),
      titulo_patente = Validate(software[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "TITULO-PATENTE"),
      data_pedido_de_deposito = Validate(software[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "DATA-PEDIDO-DE-DEPOSITO"),
      data_pedido_de_exame = Validate(software[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "DATA-DE-PEDIDO-DE-EXAME"),
      data_de_concessao = Validate(software[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "DATA-DE-CONCESSAO"),
      instituicao_deposito_registro = Validate(software[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "INSTITUICAO-DEPOSITO-REGISTRO"),
      numero_deposito_pct = Validate(software[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "NUMERO-DEPOSITO-PCT"),
      formato_data_deposito_pct = Validate(software[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "FORMATO-DATA-DEPOSITO-PCT"),
      data_deposito_pct = Validate(software[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "DATA-DEPOSITO-PCT"),
      nome_titular = Validate(software[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "NOME-DO-TITULAR"),
      nome_depositante = Validate(software[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "NOME-DO-DEPOSITANTE"),
      sta_validacao = Validate(software[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "STA-VALIDADO"),
      
      # TAG: AREAS-DO-CONHECIMENTO 
      # TODO: se tiver mais de uma?
      nome_grande_area_do_conhecimento_1 = Validate(software[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-1"]], "NOME-GRANDE-AREA-DO-CONHECIMENTO"),
      nome_da_area_do_conhecimento_1 = Validate(software[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-1"]], "NOME-DA-AREA-DO-CONHECIMENTO"),
      nome_da_sub_area_do_conhecimento_1 = Validate(software[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-1"]], "NOME-DA-SUB-AREA-DO-CONHECIMENTO"),
      nome_da_especialidade_1 = Validate(software[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-1"]], "NOME-DA-ESPECIALIDADE"),
      
      nome_grande_area_do_conhecimento_2 = Validate(software[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-2"]], "NOME-GRANDE-AREA-DO-CONHECIMENTO"),
      nome_da_area_do_conhecimento_2 = Validate(software[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-2"]], "NOME-DA-AREA-DO-CONHECIMENTO"),
      nome_da_sub_area_do_conhecimento_2 = Validate(software[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-2"]], "NOME-DA-SUB-AREA-DO-CONHECIMENTO"),
      nome_da_especialidade_2 = Validate(software[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-2"]], "NOME-DA-ESPECIALIDADE"),
      
      nome_grande_area_do_conhecimento_3 = Validate(software[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-3"]], "NOME-GRANDE-AREA-DO-CONHECIMENTO"),
      nome_da_area_do_conhecimento_3 = Validate(software[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-3"]], "NOME-DA-AREA-DO-CONHECIMENTO"),
      nome_da_sub_area_do_conhecimento_3 = Validate(software[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-3"]], "NOME-DA-SUB-AREA-DO-CONHECIMENTO"),
      nome_da_especialidade_3 = Validate(software[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-3"]], "NOME-DA-ESPECIALIDADE"),
      
      
      # TAG: SETORES-DE-ATIVIDADE
      setor_de_atividade_1 = Validate(software[["SETORES-DE-ATIVIDADE"]], "SETOR-DE-ATIVIDADE-1"),
      setor_de_atividade_2 = Validate(software[["SETORES-DE-ATIVIDADE"]], "SETOR-DE-ATIVIDADE-2"),
      setor_de_atividade_3 = Validate(software[["SETORES-DE-ATIVIDADE"]], "SETOR-DE-ATIVIDADE-3"),
      
      # TAG: INFORMACOES-ADICIONAIS
      descricao_info_adicionais = Validate(software[["INFORMACOES-ADICIONAIS"]], "DESCRICAO-INFORMACOES-ADICIONAIS"),
      descricao_info_adicionais_ingles = Validate(software[["INFORMACOES-ADICIONAIS"]], "DESCRICAO-INFORMACOES-ADICIONAIS-INGLES"),
      
      # AUTORES
      nome_completo_autores = nome_autores,
      nome_citacao_autores = nome_citacao_autores,
      ordem_autoria_autores = I(list(ordem_autores)),
      id_autores = I(list(id_autores)),
      
      stringsAsFactors = FALSE
    )
    df_pat <- rbind(df_pat, df_aux)
  }
}
