# Carregar pacotes
library(WDI)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(scales)
#install.packages("scales")
# Buscar os dados
Currentaccountbalance <- WDI(country = 'all',
                             indicator = 'BN.CAB.XOKA.CD')

# Filtrar o último ano disponível
latest_year <- max(Currentaccountbalance$year, na.rm = TRUE)

Currentaccountbalance_latest <- Currentaccountbalance %>%
  filter(year == latest_year) %>%
  filter(!is.na(BN.CAB.XOKA.CD))

# Pegar Top 20 países
top20 <- Currentaccountbalance_latest %>%
  arrange(desc(BN.CAB.XOKA.CD)) %>%
  slice(1:20)

# Garantir Brasil incluso
brasil <- Currentaccountbalance_latest %>%
  filter(country == "Brazil")

if (!(brasil$iso2c %in% top20$iso2c)) {
  top20 <- bind_rows(top20, brasil)
}

# Gráfico
grafpainel <- ggplot(top20, aes(x = reorder(country, BN.CAB.XOKA.CD),
                                y = BN.CAB.XOKA.CD,
                                fill = country == "Brazil")) +
  geom_col(show.legend = FALSE, width = 0.7) +  # barras mais finas
  coord_flip() + 
  scale_fill_manual(values = c("TRUE" = "#0072B2", "FALSE" = "#CCCCCC")) +  # Azul bonito para Brasil
  labs(title = "Top 20 Países com Maior Saldo de Conta Corrente (e Brasil)",
       subtitle = "Saldo em Exportações Líquidas (Conta Corrente) - Dados do Banco Mundial",
       x = "País",
       y = "Exportações (Em bilhões de Dólares)",
       caption = paste0("Ano: ", latest_year, " | Fonte: WDI")) +
  theme_minimal(base_family = "Arial") +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 13, face = "italic"),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title = element_text(size = 13)) +
  geom_text(aes(label = dollar(BN.CAB.XOKA.CD, scale = 1e-9, suffix = "B")),
            hjust = -0.1, size = 3.5)

# Mostrar
print(grafpainel)
