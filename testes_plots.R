rm(list = ls())

library(ipeadatar)
library(tidyverse)
library(ggplot2)
library(tsibble)
library(ggrepel)

busca <- ipeadatar::available_series(language = "br")

busca <- search_series(terms = "per capita", fields = 'name', language = "br")

pibpercapita_data <- ipeadatar::ipeadata("GAC_PIBCAPR")
pibpercapita_data <- pibpercapita_data %>% select(-uname, -tcode, -code)

pibpercapita_ts <- tsibble(
  Ano = (pibpercapita_data$date),
  Valor = pibpercapita_data$value,
  index = Ano
)


salario_data <- ipeadatar::ipeadata("GAC12_SALMINRE12")
salario_data <- salario_data %>% select(-uname, -tcode, -code)
salario_data <- salario_data %>% mutate(
  milagre = if_else(date >= "1968-01-01" & date <= "1973-01-01", TRUE, FALSE)
)

salario_ts <- tsibble(
  Ano = (salario_data$date),
  Valor = salario_data$value,
  Milagre = salario_data$milagre,
  index = Ano
)

salario_plot <- salario_ts %>%
  filter(between(Ano, as.Date("1965-01-01"), as.Date("1975-01-01"))) %>% 
  ggplot(aes(x = Ano, y = Valor), group = Milagre) +
  labs(title = "Salário mínimo real",
       subtitle = "1965-1975",
       caption = "Fonte: Ipeadata",
       x = "Anos",
       y = "Valor em R$") +
  geom_line(aes(col = Milagre, group = 1), size = 1.5) +
  scale_color_manual(values = c("#000000", "#FF0000")) +
  geom_smooth(method=lm, se=FALSE, col='blue', size=2) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(#aspect.ratio = 0.8,
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.caption = element_text(hjust = 0),
    axis.line = element_line(size = 0.5),
    plot.title = element_text(size = 18),
    legend.position = "none")

salario_plot


pibpercapita_plot <- pibpercapita_ts %>%
  filter(between(Ano, as.Date("1955-01-01"), as.Date("1985-01-01"))) %>% 
  ggplot(aes(x = Ano, y = Valor)) +
  labs(title = "	Produto interno bruto (PIB) per capita (preços 2019)",
       subtitle = "1955-1985",
       caption = "Fonte: Ipeadata",
       x = "Anos",
       y = "Valor em R$") +
  geom_line(size = 1.5) +
  scale_x_date(date_labels = "%Y", date_breaks = "5 year") +
  theme(#aspect.ratio = 0.8,
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.caption = element_text(hjust = 0),
    axis.line = element_line(size = 0.5),
    plot.title = element_text(size = 18),
    legend.position = "none")

pibpercapita_plot
