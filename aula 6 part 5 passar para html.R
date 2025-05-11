# Carregar pacotes
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(WDI)
library(plotly)
library(scales)

# Buscar dados da conta corrente
Currentaccountbalance <- WDI(country = 'all',
                             indicator = 'BN.CAB.XOKA.CD')

# Limpar os dados
Currentaccountbalance <- Currentaccountbalance %>%
  filter(!is.na(BN.CAB.XOKA.CD)) %>%
  filter(!is.na(country)) %>%
  filter(!is.na(iso2c) & nchar(iso2c) == 2)

# Pegar o ano mais recente disponível
latest_year <- max(Currentaccountbalance$year, na.rm = TRUE)

# Selecionar os top 9 países com maior superávit no último ano
top9_countries <- Currentaccountbalance %>%
  filter(year == latest_year) %>%
  arrange(desc(BN.CAB.XOKA.CD)) %>%
  slice(1:9) %>%
  pull(country)

# Garantir que o Brasil seja incluído
brasil_dados <- Currentaccountbalance %>%
  filter(country == "Brazil")

# Combinar os dados dos top 9 + Brasil
top9_com_brasil <- Currentaccountbalance %>%
  filter(country %in% top9_countries) %>%
  bind_rows(brasil_dados) %>%
  distinct(country, year, .keep_all = TRUE)

# Adicionar tooltip formatado
Currentaccountbalance_top9 <- top9_com_brasil %>%
  mutate(text = paste0("País: ", country,
                       "<br>Ano: ", year,
                       "<br>Valor: US$ ", scales::comma(BN.CAB.XOKA.CD, scale = 1e-9, suffix = "B")))

# Paleta de cores
cores <- brewer.pal(n = length(unique(Currentaccountbalance_top9$country)), name = "Set3")

# Criar gráfico com linhas
grafpainel <- ggplot(Currentaccountbalance_top9,
                     aes(x = year, y = BN.CAB.XOKA.CD, color = country, group = country, text = text)) + 
  geom_line(size = 1) +
  scale_color_manual(values = cores) +
  scale_x_continuous(breaks = seq(min(Currentaccountbalance_top9$year, na.rm = TRUE),
                                  max(Currentaccountbalance_top9$year, na.rm = TRUE), by = 5)) +
  scale_y_continuous(labels = label_dollar(scale = 1e-9, suffix = "B")) +
  labs(title = "Evolução do Saldo da Conta Corrente - Top 9 Países + Brasil",
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
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

# Tornar gráfico interativo
grafpainel_interativo <- ggplotly(grafpainel, tooltip = "text")

# Exibir gráfico
grafpainel_interativo



# corte transversal

# Carregar pacotes
library(WDI)
library(dplyr)
library(ggplot2)
library(scales)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(plotly)

# Buscar dados da conta corrente (2023)
Currentaccountbalance2023 <- WDI(country = "all",
                                 indicator = "BN.CAB.XOKA.CD",
                                 start = 2023,
                                 end = 2023,
                                 extra = TRUE)

# Limpar os dados
Currentaccountbalance2023 <- Currentaccountbalance2023 %>%
  filter(!is.na(BN.CAB.XOKA.CD),
         !is.na(iso3c),
         region != "Aggregates")

# Obter limites realistas
min_value <- min(Currentaccountbalance2023$BN.CAB.XOKA.CD, na.rm = TRUE)
max_value <- max(Currentaccountbalance2023$BN.CAB.XOKA.CD, na.rm = TRUE)

# Obter o mapa do mundo
world <- ne_countries(scale = "medium", returnclass = "sf")

# Juntar com os dados
mapa_mundi <- left_join(world, 
                        Currentaccountbalance2023, 
                        by = c("iso_a3" = "iso3c"))

# Gráfico com cores corrigidas e mapa maior
grafico_mapa <- ggplot(mapa_mundi) +
  geom_sf(aes(fill = BN.CAB.XOKA.CD,
              text = paste("País: ", name, "<br>Saldo: ", 
                           label_dollar(scale = 1e-9, suffix = "B")(BN.CAB.XOKA.CD))),
          color = "white", size = 0.1) +
  
  # Paleta personalizada: vermelho forte → amarelo → verde claro → verde escuro
  scale_fill_gradientn(
    colours = c("#8B0000", "#FF4500", "#FFFF00", "#ADFF2F", "#006400"),
    values = scales::rescale(c(min_value, -5e10, 0, 5e10, max_value)),
    limits = c(min_value, max_value),
    breaks = seq(round(min_value, -10), round(max_value, -10), by = 1e11),
    labels = label_dollar(scale = 1e-9, suffix = "B"),
    name = "Saldo da Conta Corrente\n(Bilhões de USD)",
    guide = guide_colorbar(
      title.position = "top",
      barwidth = 0.5,
      barheight = 20,
      direction = "vertical"
    )
  ) +
  
  labs(
    title = "Saldo da Conta Corrente por País (2023)",
    subtitle = "Quanto mais verde, maior o superávit; quanto mais vermelho, maior o déficit",
    caption = "Fonte: World Bank (WDI)"
  ) +
  
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 13, face = "italic"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

# Converter para interativo
grafico_interativo <- ggplotly(grafico_mapa, tooltip = "text") %>%
  layout(height = 700, width = 1100)  # Tamanho maior do mapa

# Exibir o gráfico
grafico_interativo

#Serie temporal
# Carregar pacotes
library(ggplot2)
library(dplyr)
library(WDI)
library(scales)
library(plotly)

# Buscar e preparar dados do Brasil
Currentaccountbalancebr <- WDI(country = 'BR',
                               indicator = 'BN.CAB.XOKA.CD') %>%
  arrange(year) %>%
  rename(saldo_conta_corrente = BN.CAB.XOKA.CD) %>%
  mutate(tipo_saldo = ifelse(saldo_conta_corrente >= 0, "Superávit", "Déficit"),
         saldo_bilhoes = saldo_conta_corrente / 1e9,
         tooltip_text = paste0("Ano: ", year, 
                               "\nSaldo: US$ ", formatC(saldo_bilhoes, format = "f", digits = 2), " B"))

# Identificar maiores impactos
maior_superavit <- Currentaccountbalancebr %>%
  filter(saldo_conta_corrente == max(saldo_conta_corrente, na.rm = TRUE))

maior_deficit <- Currentaccountbalancebr %>%
  filter(saldo_conta_corrente == min(saldo_conta_corrente, na.rm = TRUE))

# Gráfico corrigido com linha visível
grafico_brasil <- ggplot(Currentaccountbalancebr, aes(x = year, y = saldo_conta_corrente)) +
  geom_area(aes(fill = tipo_saldo, text = tooltip_text), alpha = 0.3) +
  
  # Força a linha aparecer
  geom_line(aes(x = year, y = saldo_conta_corrente, group = 1, text = tooltip_text),
            color = "#0072B2", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(data = maior_superavit, aes(x = year, y = saldo_conta_corrente), color = "green", size = 3) +
  geom_point(data = maior_deficit, aes(x = year, y = saldo_conta_corrente), color = "red", size = 3) +
  geom_text(data = maior_superavit, aes(x = year, y = saldo_conta_corrente, label = paste0("Maior superávit\n", year)), 
            vjust = -1.2, size = 3.5, color = "darkgreen") +
  geom_text(data = maior_deficit, aes(x = year, y = saldo_conta_corrente, label = paste0("Maior déficit\n", year)), 
            vjust = 1.8, size = 3.5, color = "darkred") +
  scale_fill_manual(values = c("Superávit" = "#56B4E9", "Déficit" = "#E69F00")) +
  scale_y_continuous(labels = label_dollar(scale = 1e-9, suffix = "B")) +
  scale_x_continuous(breaks = seq(min(Currentaccountbalancebr$year), 
                                  max(Currentaccountbalancebr$year), by = 5)) +
  labs(title = "Histórico do Balanço de Conta Corrente - Brasil",
       subtitle = "Valores anuais em bilhões de dólares (USD)",
       x = "Ano",
       y = "Saldo da Conta Corrente (USD bilhões)",
       fill = "Tipo de Saldo") +
  theme_minimal(base_family = "Arial") +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "italic"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 13))

# Interativo
grafico_brasil_interativo <- ggplotly(grafico_brasil, tooltip = "text")

# Exibir
grafico_brasil_interativo
