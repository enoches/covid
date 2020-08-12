
library(scales)
library(tidyverse)
library(magrittr)
library(utf8)
library(geobr)
library(readxl)
library(mice)
library(VIM)
library(lattice)
library(foreign)


# Carregando dados do arquivo csv (diretamente do pc) referente as estatisiticas de covid-19
brasil.io <- read.csv('caso.csv', fileEncoding = "UTF-8")

# Ajustando dados para o formato caracteres

brasil.io$state %<>% as.character()
brasil.io$city %<>% as.character()
brasil.io$date %<>% as.Date()

# Puxando base de Dados dos questionarios de arquivo excel diretamente do pc

gastoscovid <- read_excel("C:/Users/User/Dropbox/Averting Expenditures Paper/rmd/Dadosbrutos/gastoscovid_v1206.xlsx", 
                          sheet = "respostas")

# convertendo a coluna data para formato Date e grafia desejada.
gastoscovid$data %<>% as.Date()

# Criando dataframe com dados ibge


ibgecode <- lookup_muni(code_muni = 'all') # optei por puxar TODOS os municipios pois poderei usa-los em outros trabalhos

# Reduzir ibgecode 
# Criando um subset para reduzir a base de codigos do ibge
ibgecode_2 <- subset.data.frame(ibgecode, select = c(code_muni, name_muni, abrev_state))

# Fazendo o MERGE 

# Merge entre base de dados do questionario e dos códigos do IBGE 
gastos_citycodes <- merge(gastoscovid, ibgecode_2,
                          by.x = c("cidade", "uf"),
                          by.y = c("name_muni" , "abrev_state"), 
                          all.x = TRUE, all.y = FALSE)

# MERGE com dados do brasil.io
baseampla2 <- merge(gastos_citycodes, brasil.io,
                    by.x = c("code_muni", "data"),
                    by.y = c("city_ibge_code", "date"), 
                    all.x = TRUE, all.y = FALSE)



# Verificando a classe das variaveis 


# reduzindo dataframe para somente variaveis de interesse
baseampla <- baseampla2[, -c(5, 6, 28:30, 33, 34)]

# Salvar dataframe como arquivo dta (para usar no stata)
# Desejo salvar essa base maior com todos os demais datasets. Ela serve de ponto de inicio da analise.
# Aqui os dados ainda apresentam missing values

# PREPARANDO VARIAVEIS

# convertendo para numeral

baseampla$rendafam <- as.numeric(baseampla$rendafam)
baseampla$qtd_filho_dep <- as.numeric(baseampla$qtd_filho_dep)
baseampla$gasto_equip_defensiv <- as.numeric(baseampla$gasto_equip_defensiv)
baseampla$confirmed <- as.numeric(baseampla$confirmed)
baseampla$deaths <- as.numeric(baseampla$deaths)
baseampla$estimated_population_2019 <- as.numeric(baseampla$estimated_population_2019)

# Criando dummies

#Dummi para genero (1=homem, 0=mulher)

baseampla$sexo = ifelse(baseampla$sexo == "Masculino", 1, 0)

# Dummy para etnia(1=Brancos, 0=nao brancos)
#baseampla$raca = ifelse(baseampla$raca == "Branco", 1, 0)
baseampla$d.raca[baseampla$raca == "Branco"] <- 1
baseampla$d.raca[baseampla$raca == "Preto" ] <- -0
baseampla$d.raca[baseampla$raca == "Pardo" ] <- 0
baseampla$d.raca[baseampla$raca == "Amarelo" ] <- 0
baseampla$d.raca[baseampla$raca == "Ind�?gena" ] <- 0

baseampla$d.raca <- as.numeric(baseampla$d.raca)

# categorias de niveis para escolaridade (ensino sup, PG e medio = 1, 0 do contrário)
baseampla$d.escolaridade[baseampla$escolaridade == "Ensino Superior"] <- 1
baseampla$d.escolaridade[baseampla$escolaridade == "Pós-Graduação" ] <- 1
baseampla$d.escolaridade[baseampla$escolaridade == "Médio" ] <- 0
baseampla$d.escolaridade[baseampla$escolaridade == "Ensino Fundamental" ] <- 0
baseampla$d.escolaridade[baseampla$escolaridade == "Ensino Básico" ] <- 0

# Dummy para estado civil (1=casado, 0=nao casado)
baseampla$estcivil = ifelse(baseampla$estcivil == "Casado", 1, 0)

# Dummy filhos ou dependentes morando em casa (1=sim, 0 do contrário)
baseampla$filho_dep = ifelse(baseampla$filho_dep == 
                               "Sim, filhos ou dependentes moram comigo.", 1, 0)

# Dummy plano de saude (1= sim, 0= nao)
baseampla$plano = ifelse(baseampla$plano == "Sim", 1, 0)

# Dummy contratou plano de saude por causa do covid  (1= sim, 0= nao)
baseampla$plano_covid = ifelse(baseampla$pgt_plano == "Sim", 1, 0)

# Dummy contratou plano de saude durante pandemia  (1= sim, 0= nao)
baseampla$data_plano = ifelse(baseampla$data_plano == "Você contratou o plano de saúde DURANTE A PANDEMIA.", 1, 0)

# Dummy pagou plano de saude durante pandemia  (1= sim, 0= nao)
baseampla$pgt_plano = ifelse(baseampla$pgt_plano == "Sim", 1, 0)

# Dummy fez teste covid (1=testou covid, 0=Nao testou)
baseampla$teste_cov = ifelse(baseampla$teste_cov == "Sim", 1, 0)

# Dummy resultado teste covid (1=negativo , 0=outro)
baseampla$result_teste_1 = ifelse(baseampla$result_teste_cov == "Negativo", 1, 0)

# Dummy contato pessoa contaminada (1=sim, 0=Nao)
baseampla$contat_cov = ifelse(baseampla$contat_cov == "Sim", 1, 0)

# Dummy conhecia alguem que morreu por covid (sim=1, 0=Nao)
baseampla$morte_cov = ifelse(baseampla$morte_cov == "Sim", 1, 0)

# Dummy comprou defensivos contra covid (sim=1, 0=Nao)
baseampla$equip_defensiv = ifelse(baseampla$equip_defensiv == "Sim", 1, 0)

# Dummy para idoso (1=idoso, 0 nao idoso)
baseampla$idoso = ifelse(baseampla$idade >= 60, 1, 0)

# Renomeando valor do gasto defensivo total
baseampla$gasto_equip_defensivo <- baseampla$gasto_equip_defensiv


# Salvando base variaveis criadas para o formato para STATA.
# write.dta(baseampla, "covid_averting.dta")




# Visualizando a distribuicao das respostas por estado (uf)

#Reduzindo dataframe para apenas variaveis de interesse

baseampla_red <- subset(baseampla, select = c("gasto_equip_defensivo", "raca", "escolaridade", "equip_defensiv", "idoso", "morte_cov", "idade", "contat_cov", "result_teste_1", "teste_cov", "pgt_plano","data_plano", "plano", "filho_dep", "estcivil", "d.escolaridade", "d.raca","raca", "sexo", "renda", "rendafam", "qtd_filho_dep","confirmed", "deaths", "confirmed_per_100k_inhabitants"))

# Salvando dataframe com apenas variaveis de interesse em formato STATA
write.dta(baseampla_red, "covid_full.dta")


# Tratando Missing Values 

# Para tratar os missing values estou usando a técnica de imputacao presente nos links abaixo.

# Ref:  https://stats.idre.ucla.edu/r/faq/how-do-i-perform-multiple-imputation-using-predictive-mean-matching-in-r/
# https://datasciencebeginners.com/2018/11/11/a-brief-introduction-to-mice-r-package/


## missing data patterns
md.pattern(baseampla_red)

# Predictive Mean Matching (PMM) e uma abordagem semi-paramétrica de imputação. E semelhante ao método de regressão, exceto que, para cada valor ausente, preenche um valor aleatoriamente dentre os valores de doadores observados de uma observação cujos valores preditos por regressão estão mais próximos do valor predito por regressão para o valor ausente da regressão simulada modelo (Heitjan e Little 1991; Schenker e Taylor 1996).

# O método PMM garante que os valores imputados sejam plaus�?veis; pode ser mais apropriado que o método de regressão (que assume uma distribuição normal multivariada conjunta) se a premissa de normalidade for violada (Horton e Lipsitz 2001, p. 246).


## Number of observations per patterns for all pairs of variables
p <- md.pairs(baseampla_red)

# Missing Data Visualization

## Margin plot of d.escolaridade  and morte_cov
marginplot(baseampla_red[c(4, 13)], col = c("blue", "red", "orange"))

## distributions of missing variable by confirmed variable - 
pbox(baseampla_red, pos = 19)

# Using MICE (Mulitple Imputation by Chained Equations)

## Como temos um numero grande missing values, optei por fazer a imputacao de dados seguindo o metodo PMM

## by default it does 5 imputations for all missing values
imp1 <- mice(baseampla_red, m = 5) # imputacao multipla

# he output states that, as we requested, 
# 5 imputed datasets were created. Our two variables with missing values were imputed using "pmm"



# Imputation Diagnostic Checks
# The first three observation were missing information for equip_defensiv.
imp1$imp$contat_cov

# Combining imp_dataset with observed data set
imp_tot2 <- complete(imp1, "long", inc = TRUE)
# APós imputacao o dataframe passa a ter 22 columas e 6 conjunto de dados definidos na coluna 1 .imp

# We can inspect the distributions of the original and imputed data using the stripplot function that is part of the lattice package.

## labels observed data in blue and imputed data in red for y1
col <- rep(c("blue", "red")[1 + as.numeric(is.na(imp1$data$contat_cov))], 6)

## plots data for y1 by imputation
stripplot(contat_cov ~ .imp, data = imp_tot2, jit = TRUE, col = col, xlab = "imputation Number")

# Regression with imputed datasets

## linear regression for each imputed data set - 5 regression are run
fitm <- with(data = imp1, lm(gasto_equip_defensivo ~ morte_cov + contat_cov))
summary(fitm)

# R will estimate our regression model separately for each imputed dataset, 1 though 5. We then need to summarize or pool those estimates to get one overall set of parameter estimates.

## pool coefficients and standard errors across all 5 regression models
pool(fitm)

## output parameter estimates
summary(pool(fitm))

# Dataframe after imputation

# Renomeando base imputada para base_imp
base_imp <- subset(imp_tot2, select = c("gasto_equip_defensivo", "equip_defensiv", "idoso", "morte_cov", "idade","contat_cov", "result_teste_1", "teste_cov", "pgt_plano","data_plano", "plano", "filho_dep", "estcivil","escolaridade", "d.escolaridade","d.raca","raca", "sexo", "renda", "rendafam",   "qtd_filho_dep","confirmed", "deaths", "confirmed_per_100k_inhabitants"))

# Trabalharei com essa base
write.dta(base_imp, "covid_basedados_imputado.dta")

