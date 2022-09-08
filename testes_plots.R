library(ipeadatar)
library(ipeadatar)
library(tidyverse)
library(ggplot2)
library(tsibble)
library(ggrepel)

busca <- ipeadatar::available_series(language = "br")

busca <- search_series(terms = "real", fields = 'name', language = "br")

salario_data <- ipeadatar::ipeadata("GAC12_SALMINRE12")
salario_data <- salario_data %>% select(-uname, -tcode, -code)
salario_data <- salario_data %>% mutate(
  milagre = if_else(date >= "1968-01-01" & date <= "1972-01-01", TRUE, FALSE)
)

salario_ts <- tsibble(
  Ano = (salario_data$date),
  Valor = salario_data$value,
  Milagre = salario_data$milagre,
  index = Ano
)


salario_plot <- salario_ts %>%
  filter(between(Ano, as.Date("1965-01-01"), as.Date("1975-01-01"))) %>% 
  ggplot(aes(x = Ano, y = Valor)) +
  geom_line(size = 1) +
  geom_smooth(method=lm, se=FALSE, col='red', size=2)

salario_plot


pib_plot <- pib_ts %>%
  filter(between(Ano, as.Date("1955-01-01"), as.Date("1966-01-01"))) %>% 
  ggplot(aes(x = Ano, y = Valor)) +
  geom_line(size = 1.5)

pib_plot
