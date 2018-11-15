source("utils.R")

ReadCultivarRegistrada <- function(path) {
  setwd(path)
  all.xmls <- list.files(".")
  df_cultivar_reg <- data.frame()
  
  for (path.xml in all.xmls) {
    file <- xmlParse(path.xml)
    #file <- xmlParse(path)
    root <- xmlRoot(file)
    df_aux <- NULL
    
    cultivares <- root[["PRODUCAO-TECNICA"]]["CULTIVAR-REGISTRADA", all = TRUE]
    nome_autores <- "VAZIO"
    nome_citacao_autores <- "VAZIO"
    ordem_autores <- "VAZIO"
    id_autores <- "VAZIO"
    
    for (cultivar in cultivares) {
      cultivar_autores <- cultivar["AUTORES", all = TRUE]
      for (autor in cultivar_autores) {
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
      
      DADOS_BASICOS <- "DADOS-BASICOS-DA-CULTIVAR"
      DETALHAMENTO <- "DETALHAMENTO-DA-CULTIVAR"
      
      df_aux <- data.frame(
        # Informações básicas
        id_lattes = gsub(".xml", "", path.xml),
        nome_completo = Validate(root[["DADOS-GERAIS"]], "NOME-COMPLETO"),
        id_patente = Validate(cultivar[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "CODIGO-DO-REGISTRO-OU-PATENTE"),
        
        # TAG: DADOS-BASICOS-DA-CULTIVAR-REGISTRADA
        denominacao = Validate(cultivar[[DADOS_BASICOS]], "DENOMINACAO"),
        ano_solicitacao = Validate(cultivar[[DADOS_BASICOS]], "ANO-SOLICITACAO"),
        pais = Validate(cultivar[[DADOS_BASICOS]], "PAIS"),
        flag_relevancia = Validate(cultivar[[DADOS_BASICOS]], "FLAG-RELEVANCIA"),
        denominacao_ingles = Validate(cultivar[[DADOS_BASICOS]], "DENOMINACAO-INGLES"),
        flag_potencial_inovacao = Validate(cultivar[[DADOS_BASICOS]], "FLAG-POTENCIAL-INOVACAO"),
        
        # TAG: DETALHAMENTO-DA-CULTIVAR-REGISTRADA
        finalidade = Validate(cultivar[[DETALHAMENTO]], "FINALIDADE"),
        instituicao_financiadora = Validate(cultivar[[DETALHAMENTO]], "INSTITUICAO-FINANCIADORA"),
        finalidade_ingles = Validate(cultivar[[DETALHAMENTO]], "FINALIDADE-INGLES"),
        
        # TAG: DETALHAMENTO-DA-CULTIVAR-REGISTRADA > REGISTRO-OU-PATENTE
        #WARN: Informação duplicada, TITULO-PATENTE
        titulo_patente_reg = Validate(cultivar[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "TITULO-PATENTE"),
        data_pedido_de_deposito = Validate(cultivar[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "DATA-PEDIDO-DE-DEPOSITO"),
        data_pedido_de_exame = Validate(cultivar[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "DATA-DE-PEDIDO-DE-EXAME"),
        data_de_concessao = Validate(cultivar[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "DATA-DE-CONCESSAO"),
        instituicao_deposito_registro = Validate(cultivar[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "INSTITUICAO-DEPOSITO-REGISTRO"),
        numero_deposito_pct = Validate(cultivar[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "NUMERO-DEPOSITO-PCT"),
        formato_data_deposito_pct = Validate(cultivar[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "FORMATO-DATA-DEPOSITO-PCT"),
        data_deposito_pct = Validate(cultivar[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "DATA-DEPOSITO-PCT"),
        nome_titular = Validate(cultivar[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "NOME-DO-TITULAR"),
        nome_depositante = Validate(cultivar[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "NOME-DO-DEPOSITANTE"),
        sta_validacao = Validate(cultivar[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "STA-VALIDADO"),
        
        # TAG: AREAS-DO-CONHECIMENTO 
        # TODO: se tiver mais de uma?
        nome_grande_area_do_conhecimento_1 = Validate(cultivar[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-1"]], "NOME-GRANDE-AREA-DO-CONHECIMENTO"),
        nome_da_area_do_conhecimento_1 = Validate(cultivar[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-1"]], "NOME-DA-AREA-DO-CONHECIMENTO"),
        nome_da_sub_area_do_conhecimento_1 = Validate(cultivar[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-1"]], "NOME-DA-SUB-AREA-DO-CONHECIMENTO"),
        nome_da_especialidade_1 = Validate(cultivar[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-1"]], "NOME-DA-ESPECIALIDADE"),
        
        nome_grande_area_do_conhecimento_2 = Validate(cultivar[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-2"]], "NOME-GRANDE-AREA-DO-CONHECIMENTO"),
        nome_da_area_do_conhecimento_2 = Validate(cultivar[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-2"]], "NOME-DA-AREA-DO-CONHECIMENTO"),
        nome_da_sub_area_do_conhecimento_2 = Validate(cultivar[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-2"]], "NOME-DA-SUB-AREA-DO-CONHECIMENTO"),
        nome_da_especialidade_2 = Validate(cultivar[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-2"]], "NOME-DA-ESPECIALIDADE"),
        
        # TAG: SETORES-DE-ATIVIDADE
        setor_de_atividade_1 = Validate(cultivar[["SETORES-DE-ATIVIDADE"]], "SETOR-DE-ATIVIDADE-1"),
        setor_de_atividade_2 = Validate(cultivar[["SETORES-DE-ATIVIDADE"]], "SETOR-DE-ATIVIDADE-2"),
        setor_de_atividade_3 = Validate(cultivar[["SETORES-DE-ATIVIDADE"]], "SETOR-DE-ATIVIDADE-3"),
        
        
        # AUTORES
        nome_completo_autores = nome_autores,
        nome_citacao_autores = nome_citacao_autores,
        ordem_autoria_autores = I(list(ordem_autores)),
        id_autores = I(list(id_autores)),
        
        stringsAsFactors = FALSE
      )
      df_cultivar_reg <- rbind(df_cultivar_reg, df_aux)
    }
  }
  rm(df_aux)
  
  df_cultivar_reg$id_patente <- RemoveNonAlNum(df_cultivar_reg$id_patente)
  
  df_cultivar_reg
}
