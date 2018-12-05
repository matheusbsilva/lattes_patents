source("utils.R")

# Params:
#   path = Path of the folder with the cvs
ReadPatents <- function(path) {
  setwd(path)
  all.xmls <- list.files(".")
  df_pat <- data.frame()
  
  for (path.xml in all.xmls) {
    file <- xmlParse(path.xml)
    root <- xmlRoot(file)
    df_aux <- NULL
    
    patentes <- root[["PRODUCAO-TECNICA"]]["PATENTE", all = TRUE]
    nome_autores <- "VAZIO"
    nome_citacao_autores <- "VAZIO"
    ordem_autores <- "VAZIO"
    id_autores <- "VAZIO"
    
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
      df_aux <- data.frame(
        # Informações básicas
        id_lattes = gsub(".xml", "", path.xml),
        nome_completo = Validate(root[["DADOS-GERAIS"]], "NOME-COMPLETO"),
        id_patente = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]], "CODIGO-DO-REGISTRO-OU-PATENTE"),
        
        # TAG: DADOS-BASICOS-DA-PATENTE
        titulo_patente = Validate(patente[["DADOS-BASICOS-DA-PATENTE"]], "TITULO"),
        ano_desenvolvimento = Validate(patente[["DADOS-BASICOS-DA-PATENTE"]], "ANO-DESENVOLVIMENTO"),
        pais = Validate(patente[["DADOS-BASICOS-DA-PATENTE"]], "PAIS"),
        meio_de_divulgacao = Validate(patente[["DADOS-BASICOS-DA-PATENTE"]], "MEIO-DE-DIVULGACAO"),
        flag_relevancia = Validate(patente[["DADOS-BASICOS-DA-PATENTE"]], "FLAG-RELEVANCIA"),
        flag_potencial_inovacao = Validate(patente[["DADOS-BASICOS-DA-PATENTE"]], "FLAG-POTENCIAL-INOVACAO"),
        
        # TAG: DETALHAMENTO-DA-PATENTE
        finalidade = Validate(patente[["DETALHAMENTO-DA-PATENTE"]], "FINALIDADE"),
        instituicao_financiadora = Validate(patente[["DETALHAMENTO-DA-PATENTE"]], "INSTITUICAO-FINANCIADORA"),
        categoria = Validate(patente[["DETALHAMENTO-DA-PATENTE"]], "CATEGORIA"),
        
        # TAG: DETALHAMENTO-DA-PATENTE > REGISTRO-OU-PATENTE
        tipo_patente = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]], "TIPO-PATENTE"),
        #WARN: Informação duplicada, TITULO-PATENTE
        titulo_patente_reg = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]], "TITULO-PATENTE"),
        data_pedido_de_deposito = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]], "DATA-PEDIDO-DE-DEPOSITO"),
        data_pedido_de_exame = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]], "DATA-DE-PEDIDO-DE-EXAME"),
        data_de_concessao = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]], "DATA-DE-CONCESSAO"),
        instituicao_deposito_registro = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]], "INSTITUICAO-DEPOSITO-REGISTRO"),
        numero_deposito_pct = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]], "NUMERO-DEPOSITO-PCT"),
        formato_data_deposito_pct = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]], "FORMATO-DATA-DEPOSITO-PCT"),
        data_deposito_pct = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]], "DATA-DEPOSITO-PCT"),
        nome_titular = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]], "NOME-DO-TITULAR"),
        nome_depositante = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]], "NOME-DO-DEPOSITANTE"),
        sta_validacao = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]], "STA-VALIDADO"),
        
        # TAG: DETALHAMENTO-DA-PATENTE > HISTORICO-SITUACOES-PATENTE
        descricao_situacao = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["HISTORICO-SITUACOES-PATENTE"]], "DESCRICAO-SITUACAO-PATENTE"),
        data_situacao = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["HISTORICO-SITUACOES-PATENTE"]], "DATA-SITUACAO-PATENTE"),
        status_situacao = Validate(patente[["DETALHAMENTO-DA-PATENTE"]][["HISTORICO-SITUACOES-PATENTE"]], "STATUS-SITUACAO-PATENTE"),
        
        # TAG: PALAVRAS-CHAVE
        palavra_chave_1 = Validate(patente[["PALAVRAS-CHAVE"]], "PALAVRA-CHAVE-1"),
        palavra_chave_2 = Validate(patente[["PALAVRAS-CHAVE"]], "PALAVRA-CHAVE-2"),
        palavra_chave_3 = Validate(patente[["PALAVRAS-CHAVE"]], "PALAVRA-CHAVE-3"),
        palavra_chave_4 = Validate(patente[["PALAVRAS-CHAVE"]], "PALAVRA-CHAVE-4"),
        palavra_chave_5 = Validate(patente[["PALAVRAS-CHAVE"]], "PALAVRA-CHAVE-5"),
        palavra_chave_6 = Validate(patente[["PALAVRAS-CHAVE"]], "PALAVRA-CHAVE-6"),
        
        # TAG: SETORES-DE-ATIVIDADE
        setor_de_atividade_1 = Validate(patente[["SETORES-DE-ATIVIDADE"]], "SETOR-DE-ATIVIDADE-1"),
        setor_de_atividade_2 = Validate(patente[["SETORES-DE-ATIVIDADE"]], "SETOR-DE-ATIVIDADE-2"),
        setor_de_atividade_3 = Validate(patente[["SETORES-DE-ATIVIDADE"]], "SETOR-DE-ATIVIDADE-3"),
        
        # TAG: INFORMACOES-ADICIONAIS
        descricao_info_adicionais = Validate(patente[["INFORMACOES-ADICIONAIS"]], "DESCRICAO-INFORMACOES-ADICIONAIS"),
        
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
  # Remove unused data frame
  rm(df_aux)
  
  df_pat$id_patente <- RemoveNonAlNum(df_pat$id_patente)

  df_pat
}


