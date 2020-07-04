
library(tidyverse)
library(readxl)

basecov_survey <- read_excel("base_survey.xlsx", sheet = "respostas")

basecov_ampla <- readr::read_csv("baseampla.csv")

# basemampla_load.R

usethis::use_data(basecov_survey, overwrite = TRUE)

usethis::use_data(basecov_ampla, overwrite = TRUE)


