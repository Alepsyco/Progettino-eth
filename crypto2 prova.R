install.packages("crypto2")
remove.packages("a")

#OPERAZIONI PRELIMINARI
C

library("crypto2")
library("dplyr")
library("tidyverse")
library("extrafont") #Ingnorare 40+ errori generati dalla libreria (developer comment)
library("ggplot2")
library("plotly")
library("htmlwidgets")
library("grid")
library("png")
library("ggimage")
library("patchwork")
library("ggridges")
library("scales")

#library("conflicted") # Possibile risoluzione per errore
#Unable to find an inherited method for function select for signature "data.frame" (conflitti tra pacchetti)

set.seed(1984)

#CoinMarketCap CNC prende info da vari exchange di cryptovalute e mostra dati (volumi, prezzo, transazioni)


#------------------------------------------------
#funzioni prova (alcune chiamate lente, forse conviene usare saveRDS readRDS)

#restituisce crypto (attive/disattive su CMC, untracked sono monete che CMC non considera affidabili 
cryptoACTIVE = crypto_list(only_active = TRUE, add_untracked = FALSE)
cryptoALL = crypto_list(only_active = FALSE, add_untracked = TRUE)

#crypto_info da altre informazioni (principalmente flag)
bitcoin = crypto_list(only_active = TRUE, add_untracked = FALSE)
bitcoin <- bitcoin %>% filter(slug == "bitcoin")

bitcoinINFO = crypto_info(coin_list = bitcoin)

#andamento crypto (giornaliero o qualunque intervallo)
bitcoinHOUR = crypto_history(coin_list = bitcoin, start_date = "20250101", end_date = "20250102", interval = "1h")

#crypto_listings è come list ma da classifica e dati prezzo capitalizzazione volumi (forse basta usare listing)
listing = crypto_listings(which = "latest", quote = TRUE) #latest new historical

#crypto_global_quotes statistiche di tutto il mercato
andamento = crypto_global_quotes(which = "historical", quote = TRUE, start_date = "20250101", end_date = "20250102", interval = "daily") #latest historical

#exchange_list come crypto_list
exchangeACTIVE = exchange_list(only_active = TRUE, add_untracked = FALSE)

#exchange_info restituisce informazioni su un exchange scelto con lo slug
binance = exchange_list(only_active = TRUE, add_untracked = FALSE)
binance <- binance %>% filter(slug == "binance")

binanceINFO = exchange_info(exchange_list = binance)

#------------------------------------------------



################################################################################################################################
################################################################################################################################

#TODO E IDEE

#a seconda di cosa si può fare per il pacchetto, si potrebbe fare anche un misto tra data art e analisi crypto (??) in alternativa controllo di quanto
#lanciare una crypto influenzi i big (bitcoin ethereum) o quanto shitcoin si influenzino a vicenda

#altra idea quanto tempo durano shitcoin prima di rugpull

#quale crypto ha % di volume spostata in una singola transazione

#la parola coin influenza successo di una crypto (togliendo bitcoin che fa da "riferimento")