# API (PORTA/FORMA DE ACESSO REMOTO)
# ACESSAMOS OS DADOS DO BANCO MUNDIAL (WORLD BANK)
# WORLD DEVELOPMENT INDICATORS (WDI)
# INDICADORES DE DESENVOLVIMENTO MUNDIAL 

# PIB (PRODUTO INTERNO BRUTO)

library(WDI) # CARREGAR A BIBLIOTECA/PACOTE

options(scipen = 999) # REMOVER A NOT. CIENT.
# dados em painel
dadospin <- WDI(country = 'all',
                indicator = 'NY.GDP.MKTP.CD')


paises <- c('BR', 'US')
dadospibbrus <- WDI(country = paises,
                    indicator = 'NY.GDP.MKTP.CD')
# CORTE TRANSVERSAL


dadospib2023 <- WDI(country = 'all',
                    indicator = 'NY.GDP.MKTP.CD',
                    start = 2023, end = 2023)

# SERIE TEMPORAL

dadospibbr <- WDI(country = 'BR',
                  indicator = 'NY.GDP.MKTP.CD')

