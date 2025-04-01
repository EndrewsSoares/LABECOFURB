# API (PORTA/FORMA DE ACESSO REMOTO)
# ACESSAMOS OS DADOS DO BANCO MUNDIAL (WORLD BANK)
# WORLD DEVELOPMENT INDICATORS (WDI)
# INDICADORES DE DESENVOLVIMENTO MUNDIAL 

# PIB (PRODUTO INTERNO BRUTO)

library(WDI) # CARREGAR A BIBLIOTECA/PACOTE

options(scipen = 999) # REMOVER A NOT. CIENT.
# dados em painel
Currentaccountbalance <- WDI(country = 'all',
                indicator = 'BN.CAB.XOKA.CD')


paises <- c('BR', 'US')
Currentaccountbalancebrus <- WDI(country = paises,
                    indicator = 'BN.CAB.XOKA.CD')
# CORTE TRANSVERSAL


Currentaccountbalance2023 <- WDI(country = 'all',
                    indicator = 'BN.CAB.XOKA.CD',
                    start = 2023, end = 2023)

# SERIE TEMPORAL

Currentaccountbalancebr <- WDI(country = 'BR',
                  indicator = 'BN.CAB.XOKA.CD')

# GRAFICO 

# biblioteca ggplot2 (tidyverse)
#install.packages("tidyverse")
library(tidyverse)
## DADOS EM PAINEL

grafpainel <- ggplot(Currentaccountbalance,
                     mapping = aes(y = BN.CAB.XOKA.CD,
                                   x = year)) + 
  geom_point()

print(grafpainel)

# corte transversal

grafcorte <-  ggplot(Currentaccountbalance2023,
                     mapping = aes(y = BN.CAB.XOKA.CD,
                                   x = year)) +
  geom_point()
print(grafcorte)



# serie temporal

grafserie <-ggplot(Currentaccountbalancebr,
                   mapping = aes(y = BN.CAB.XOKA.CD,
                                 x = year)) + 
  geom_line()
print(grafserie)


