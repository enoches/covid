library(scales)
library(tidyverse)
library(magrittr)
library(utf8)
library(foreign)
library(haven)
library(foreign)
library(stargazer)
library("margins")
library("datasets")
library(stargazer)


covid_basedados <- 
        read_dta("C:/Users/User/Dropbox/Averting Expenditures Paper/rmd/dados_dta/covid_full.dta")

base_cov_full <- covid_basedados


summary(base_cov_full)



ols_gd <- lm(gasto_equip_defensivo ~  sexo + estcivil + morte_cov + contat_cov + filho_dep + idade + deaths + confirmed + d_raca + renda + d_escolaridade, data = base_cov_full)

summary(ols_gd)



# Depois de analisar os dados acima, optei por escolher o modelo "reg_ed_prob"

# Calculando os Efeitos marginais do PROBIT

# https://github.com/leeper/margins


# Esse modelo ficou bastante interessante. Melhor discutir com cadu e Roberta
probit_6 <- glm(equip_defensiv ~ sexo + estcivil + morte_cov + contat_cov + deaths + confirmed + d_escolaridade, data = base_cov_full, family = binomial(link = "probit"))


# considerando os modelos GLM, escolho o modelo probit_6

stargazer(ols_gd, probit_6, type = 'text')

j = TRUE)


# Analisando Heckman

library(sampleSelection)

heck_cov = heckit(
  selection = equip_defensiv ~ sexo + morte_cov + estcivil,
  outcome = gasto_equip_defensivo ~ sexo + morte_cov + contat_cov + filho_dep + idade + deaths + confirmed + d_raca + renda + d_escolaridade,
  data = base_cov_full,
  method = "2step"
)

summary(heck_cov)
stargazer(heck_cov, type = 'text')

teste <- lm(gasto_equip_defensivo ~ deaths, data =  base_cov)
stargazer(teste, type = 'text')





