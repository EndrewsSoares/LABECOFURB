# Carregar pacotes
library(WDI)
library(ggplot2)
library(dplyr)
library(scales)
library(RColorBrewer)  # para usar paleta super variada de cores
library(ggthemes)      # para tema clean

# Buscar os dados
Currentaccountbalance <- WDI(country = 'all',
                             indicator = 'BN.CAB.XOKA.CD')

# Limpar dados
Currentaccountbalance <- Currentaccountbalance %>%
  filter(!is.na(BN.CAB.XOKA.CD)) %>%
  filter(!is.na(country)) %>%
  filter(!region %in% c("Aggregates"))  # Remover agregações tipo "World", etc.

# Pegar ano mais recente
latest_year <- max(Currentaccountbalance$year, na.rm = TRUE)

# Top 10 países no ano mais recente
top10_countries <- Currentaccountbalance %>%
  filter(year == latest_year) %>%
  arrange(desc(BN.CAB.XOKA.CD)) %>%
  slice(1:10) %>%
  pull(country)

# Garantir Brasil incluso
if (!"Brazil" %in% top10_countries) {
  top10_countries <- c(top10_countries, "Brazil")
}

# Filtrar dados
Currentaccountbalance_top10 <- Currentaccountbalance %>%
  filter(country %in% top10_countries)

# Paleta de cores bem variada
cores <- brewer.pal(n = length(unique(Currentaccountbalance_top10$country)), name = "Set3")

# Gráfico atualizado
grafpainel <- ggplot(Currentaccountbalance_top10, 
                     aes(x = year, 
                         y = BN.CAB.XOKA.CD, 
                         color = country, 
                         group = country)) +
  geom_line(size = 1.5) +  # linha mais grossa
  geom_point(size = 2) +   # pontos nos anos
  scale_color_manual(values = cores) +  # cores distintas
  scale_y_continuous(labels = label_dollar(scale = 1e-9, suffix = "B")) +  # Eixo Y em bilhões
  scale_x_continuous(
    breaks = seq(min(Currentaccountbalance_top10$year, na.rm = TRUE),
                 max(Currentaccountbalance_top10$year, na.rm = TRUE),
                 by = 5)) +  # Saltos de 5 em 5 anos
  labs(title = "Evolução do Saldo da Conta Corrente - Top 10 Países + Brasil",
       subtitle = "Dados a cada 5 anos | Valores em bilhões de dólares (USD)",
       x = "Ano",
       y = "Saldo da Conta Corrente (USD bilhões)",
       color = "País") +
  theme_minimal(base_family = "Arial") +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "italic"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 13),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))  # Legenda em duas linhas

# Exibir o gráfico
print(grafpainel)
