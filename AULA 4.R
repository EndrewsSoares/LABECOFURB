# WDI WORLD DEVELOPMENT INDICATORS
# OS DADOS DO BANCO UNDIAL DE DADOS 

#install.packages("WDI")
library(WDI) # SEMPRE CHAMAR O PACOTE

# procurem as vigenttes
# paginas com instruçôes dos pacotes

# GDP (current US$)(NY.GDP.MKTP.CD)
# pib, preços correntes em dólar norte-americano 
# código: ny.gdp.mktp.cd

# vamos pesquisar o termos 'gdp' no WDI

varpib <- WDIsearch('gdp') # criar objeto com o res.

# buscar os dados com o código do site 

options(scipen = 999) # ajusta a not. cientifica.

dadospib <- WDI(country = 'all',
                indicator = 'NY.GDP.MKTP.CD')

dadospib2023 <- WDI(country = 'all',
                    indicator = 'NY.GDP.MKTP.CD',
                    start = 2023, end = 2023)
                
            
