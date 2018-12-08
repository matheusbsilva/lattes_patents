library(openxlsx)
library(dplyr)

setwd("~/Documentos/ds4all/gcee/results")
source("graph_results.R")


df_patents <- read.xlsx("../dados-processados/patentes.xlsx")
df_marcas <- read.xlsx("../dados-processados/marca.xlsx")
df_software <- read.xlsx("../dados-processados/software.xlsx")
df_topografia <- read.xlsx("../dados-processados/topografia.xlsx")
df_desenho <- read.xlsx("../dados-processados/desenho.xlsx")
df_cultivar_reg <- read.xlsx("../dados-processados/cultivar_registrada.xlsx")
df_cultivar_prot <- read.xlsx("../dados-processados/cultivar_protegida.xlsx")

# Normalize year of all dfs
df_patents <- transform(df_patents, ano = as.numeric(as.character(ano_desenvolvimento)))
df_marcas <- transform(df_marcas, ano = as.numeric(as.character(ano_desenvolvimento)))
df_software <- transform(df_software, ano = as.numeric(as.character(ano_desenvolvimento)))
df_topografia <- transform(df_topografia, ano = as.numeric(as.character(ano_desenvolvimento)))
df_desenho <- transform(df_desenho, ano = as.numeric(as.character(ano_desenvolvimento)))
df_cultivar_reg <- transform(df_cultivar_reg, ano = as.numeric(as.character(ano_solicitacao)))
df_cultivar_prot <- transform(df_cultivar_prot, ano = as.numeric(as.character(ano_solicitacao)))

df_patents <- transform(df_patents, grupo_tematico = as.character(as.character(grupo_tematico)))
df_marcas <- transform(df_marcas, grupo_tematico = as.character(as.character(grupo_tematico)))
df_software <- transform(df_software, grupo_tematico = as.character(as.character(grupo_tematico)))
df_topografia <- transform(df_topografia, grupo_tematico = as.character(as.character(grupo_tematico)))
df_desenho <- transform(df_desenho, grupo_tematico = as.character(as.character(grupo_tematico)))
df_cultivar_reg <- transform(df_cultivar_reg, grupo_tematico = as.character(as.character(grupo_tematico)))
df_cultivar_prot <- transform(df_cultivar_prot, grupo_tematico = as.character(as.character(grupo_tematico)))

# Create a merged df with all types of patents
df_all <- Reduce(function(x, y) merge(x, y, all=TRUE), list(df_patents, df_marcas, df_software, df_topografia, df_desenho, df_cultivar_prot, df_cultivar_reg))

# Graphs for all types of patents
graph_by_year <- GraphByYear(df_all)

year_df <- data.frame(table(FilterGreaterThan1996(df_all)$ano), stringsAsFactors = FALSE)
colnames(year_df) <- c('ano', 'qtd_patentes')
# Aqui explora-se a distribuição das patentes no decorrer do ano, pode-se observar que houve uma 
# crescente relevante no número de patentes nos três anos seguintes a 2010, ano de criação dos INCTs.
# Sendo que estes três anos são os que possuem o maior número de patentes de acordo com conjunto de dados, 
# uma média de 581 patentes.
graph_by_mean_year <- GraphByMeanYear(df_all)

greater_1996 <- FilterGreaterThan1996(df_all)
before_2010 <- filter(greater_1996, ano >= 2002 & ano < 2010)
after_2010 <- filter(greater_1996, ano >= 2010 & ano < 2018)
before_2010 <- data.frame(table(before_2010$ano), stringsAsFactors = FALSE)
colnames(before_2010) <- c('ano', 'qtd_patentes')
after_2010 <- data.frame(table(after_2010$ano), stringsAsFactors = FALSE)
colnames(after_2010) <- c('ano', 'qtd_patentes')
means_years <- data.frame("year" = c("2002-2009","2010-2017"), "media" = c(mean(before_2010$qtd_patentes), mean(after_2010$qtd_patentes)))
# Inclusive é possível observar que existe uma aumento na média do número de patentes após 2010.

# ****IMPORTANTE TODOS OS GRÁFICOS ABAIXO LEVAM EM CONTA OS DADOS A PARTIR DE 2010

graph_by_country <- GraphByCountry(df_all)

result_after_2010 <- FilterGreaterThan2010(df_all)
country_df <- data.frame(table(result_after_2010$pais))
colnames(country_df) <- c('pais', 'qtd_patentes')
filtered_countries <- filter(country_df, qtd_patentes > 1 & pais != 'Brasil')
# Outra observação importante é a quantidade de patentes por países. Nesse gráfico não foi levado
# em conta o Brasil por ter claramente o maior número de patentes.

graph_by_tematico <- GraphByTematico(df_all)
df_all$grupo_tematico <- gsub('\\s', '', df_all$grupo_tematico)
result_after_2010 <- FilterGreaterThan2010(df_all)
pat_tematico <- data.frame(table(result_after_2010$grupo_tematico))
colnames(pat_tematico) <- c('grupo_tematico', 'qtd_patentes')
# Aqui pode-se começar a observar a tendência dos temas das patentes analisadas, onde a maioria
# pertence ao grupo temático relacacionado a Saúde.


keywords_cloud <- CloudKeyWords(df_all)

# Para realizar uma exploração mais profunda sobre o objetivo geral das patentes analisadas, 
# tomou-se como base as palavras chaves disponíveis em cada registro de patente. Ao agrupar essas
# palavras chaves e observar sua recorrência através de uma nuvem de palavras pode-se perceber que
# as temáticas giram em torno do tema saúde, apresentando como palavras chaves mais recorrentes 
# temas críticos como Alzheimer, Câncer e Aedes aegypti. Além disso existem temas relevantes 
# de outras áreas como Biotecnologia, Biodisel e Aplicações embarcadas. 

graph_by_inct <- GraphTopNIncts(df_all, 10)

# Por fim foi analisado a quantidade de patentes agrupado por INCT. Foi disponilizado no gráfico
# os primeiros 10 INCTS com o maior número de patentes. A distribuição apresentada mostra o INCT
# em Medicina Assistida por Computação Científica como o grupo com a maior quantia de patentes.
# Comprovando as observações anteriores onde o tema saúde foi o mais recorrente nas informações
# analisadas.