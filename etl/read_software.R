source("utils.R")

ReadSoftware <- function(path) {
  setwd(path)
  all.xmls <- list.files(".")
  df_software <- data.frame()
  
  for (path.xml in all.xmls) {
    file <- xmlParse(path.xml)
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
        titulo_software = Validate(software[[DADOS_BASICOS]], "TITULO-DO-SOFTWARE"),
        ano_desenvolvimento = Validate(software[[DADOS_BASICOS]], "ANO"),
        pais = Validate(software[[DADOS_BASICOS]], "PAIS"),
        idioma = Validate(software[[DADOS_BASICOS]], "IDIOMA"),
        flag_relevancia = Validate(software[[DADOS_BASICOS]], "FLAG-RELEVANCIA"),
        doi = Validate(software[[DADOS_BASICOS]], "FLAG-RELEVANCIA"),
        meio_de_divulgacao = Validate(software[[DADOS_BASICOS]], "MEIO-DE-DIVULGACAO"),
        titulo_ingles = Validate(software[[DADOS_BASICOS]], "TITULO-DO-SOFTWARE-INGLES"),
        flag_potencial_inovacao = Validate(software[[DADOS_BASICOS]], "FLAG-POTENCIAL-INOVACAO"),
        flag_divulgacao_cientifica = Validate(software[[DADOS_BASICOS]], "FLAG-DIVULGACAO-CIENTIFICA"),
        
        # TAG: DETALHAMENTO-DO-SOFTWARE
        plataforma = Validate(software[[DETALHAMENTO]], "PLATAFORMA"),
        disponibilidade = Validate(software[[DETALHAMENTO]], "DISPONIBILIDADE"),
        finalidade = Validate(software[[DETALHAMENTO]], "FINALIDADE"),
        finalidade_ingles = Validate(software[[DETALHAMENTO]], "FINALIDADE-INGLES"),
        ambiente = Validate(software[[DETALHAMENTO]], "AMBIENTE"),
        instituicao_financiadora = Validate(software[[DETALHAMENTO]], "INSTITUICAO-FINANCIADORA"),
        
        # TAG: DETALHAMENTO-DO-SOFTWARE > REGISTRO-OU-PATENTE
        tipo_patente = Validate(software[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "TIPO-PATENTE"),
        #WARN: Informação duplicada, TITULO-PATENTE
        titulo_patente_reg = Validate(software[[DETALHAMENTO]][["REGISTRO-OU-PATENTE"]], "TITULO-PATENTE"),
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
        
        # TAG: AREA-DO-CONHECIMENTO 
        # TODO: se tiver mais de uma?
        nome_grande_area_do_conhecimento = Validate(software[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-1"]], "NOME-GRANDE-AREA-DO-CONHECIMENTO"),
        nome_da_area_do_conhecimento = Validate(software[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-1"]], "NOME-DA-AREA-DO-CONHECIMENTO"),
        nome_da_sub_area_do_conhecimento = Validate(software[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-1"]], "NOME-DA-SUB-AREA-DO-CONHECIMENTO"),
        nome_da_especialidade = Validate(software[["AREAS-DO-CONHECIMENTO"]][["AREA-DO-CONHECIMENTO-1"]], "NOME-DA-ESPECIALIDADE"),
        
        # TAG: PALAVRAS-CHAVE
        palavra_chave_1 = Validate(software[["PALAVRAS-CHAVE"]], "PALAVRA-CHAVE-1"),
        palavra_chave_2 = Validate(software[["PALAVRAS-CHAVE"]], "PALAVRA-CHAVE-2"),
        palavra_chave_3 = Validate(software[["PALAVRAS-CHAVE"]], "PALAVRA-CHAVE-3"),
        palavra_chave_4 = Validate(software[["PALAVRAS-CHAVE"]], "PALAVRA-CHAVE-4"),
        palavra_chave_5 = Validate(software[["PALAVRAS-CHAVE"]], "PALAVRA-CHAVE-5"),
        palavra_chave_6 = Validate(software[["PALAVRAS-CHAVE"]], "PALAVRA-CHAVE-6"),
        
        # TAG: SETORES-DE-ATIVIDADE
        setor_de_atividade_1 = Validate(software[["SETORES-DE-ATIVIDADE"]], "SETOR-DE-ATIVIDADE-1"),
        setor_de_atividade_2 = Validate(software[["SETORES-DE-ATIVIDADE"]], "SETOR-DE-ATIVIDADE-2"),
        setor_de_atividade_3 = Validate(software[["SETORES-DE-ATIVIDADE"]], "SETOR-DE-ATIVIDADE-3"),
        
        # AUTORES
        nome_completo_autores = nome_autores,
        nome_citacao_autores = nome_citacao_autores,
        ordem_autoria_autores = I(list(ordem_autores)),
        id_autores = I(list(id_autores)),
        
        stringsAsFactors = FALSE
      )
      df_software <- rbind(df_software, df_aux)
    }
  }
  rm(df_aux)
  
  df_software$id_patente <- RemoveNonAlNum(df_software$id_patente)
  
  df_software
}