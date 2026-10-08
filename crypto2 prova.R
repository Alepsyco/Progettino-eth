install.packages("a")
remove.packages("a")

#OPERAZIONI PRELIMINARI

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
#funzioni prova (alcune chiamate lente, forse conviene usare saveRDS readRDS???????????????)

#restituisce crypto (attive/disattive su CMC, untracked sono monete che CMC non considera affidabili
cryptoACTIVE = crypto_list(only_active = TRUE, add_untracked = FALSE)
cryptoALL = crypto_list(only_active = FALSE, add_untracked = TRUE)

#crypto_info da altre informazioni (principalmente flag)
bitcoin = crypto_list(only_active = TRUE, add_untracked = FALSE)
bitcoin <- bitcoin %>% filter(slug == "bitcoin")

bitcoinINFO = crypto_info(coin_list = bitcoin)

#andamento crypto (giornaliero o qualunque intervallo)
bitcoinDAY = crypto_history(coin_list = bitcoin, interval = "daily")
bitcoinDAY <- bitcoinDAY %>% distinct() #alcune entry sono ripetute??

#crypto_listings è come list ma da classifica e dati prezzo capitalizzazione volumi (forse basta usare listing)
listing = crypto_listings(which = "latest", quote = FALSE) #latest new historical

#crypto_global_quotes statistiche di tutto il mercato
andamento = crypto_global_quotes(which = "historical", quote = TRUE, interval = "daily") #latest historical

#exchange_list come crypto_list
exchangeACTIVE = exchange_list(only_active = TRUE, add_untracked = FALSE)

#exchange_info restituisce informazioni su un exchange scelto con lo slug
binance = exchange_list(only_active = TRUE, add_untracked = FALSE)
binance <- binance %>% filter(slug == "binance")

binanceINFO = exchange_info(exchange_list = binance)


#------------------------------------------------
#PARTE 1 bitcoin è davvero così dominante come crediamo?

#storico bitcoin
bitcoinANDAMENTO <- bitcoinDAY %>%
  ggplot(aes(x = timestamp, y = close, group = 1,
             text = paste("Data:", time_open, "<br>", "Prezzo di chiusura:", round(close, 2), "$"))) +
  geom_line(colour = "orange") +
  labs(
    title = "Valore di 1 Bitcoin",
    subtitle = "Prezzo di chiusura giornaliero in dollari (scala logaritmica)",
    x = "Data",
    y = "Prezzo"
  ) +
  scale_y_log10(labels = label_dollar()) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid',colour = "grey"),
    panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',colour = "grey"),
    plot.title = element_text(face = "bold", size = 14),
    text = element_text(size = 10)
  )

ggplotly(bitcoinANDAMENTO, tooltip = ("text"))

#------------------------------------------------

#numero di crypto attive nel tempo
cryptoANDAMENTO_generico <- andamento %>%
  filter(!is.na(active_cryptocurrencies)) %>%
  ggplot(aes(x = timestamp, y = active_cryptocurrencies, group = 1,
             text = paste("Data:", timestamp, "<br>", "Crypto attive:", active_cryptocurrencies))) +
  geom_line(colour = "orange") +
  labs(
    title = "Numero di crypto attive",
    subtitle = "Crypto attive su CoinMarketCap in ogni giorno",
    x = "Data",
    y = "Crypto attive"
  ) +
  
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid',colour = "grey"),
    panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',colour = "grey"),
    plot.title = element_text(face = "bold", size = 14),
    text = element_text(size = 10)
  )

ggplotly(cryptoANDAMENTO_generico, tooltip = ("text")) #per mostrare hovering text definito prima


#------------------------------------------------

#crypto attive, inattive e untracked
stato <- cryptoALL %>%
  mutate(stato = case_when(id %in% cryptoACTIVE$id ~ "Attive",
                           id %in% crypto_list(only_active = FALSE, add_untracked = FALSE)$id ~ "Inattive",
                           TRUE ~ "Untracked"))

stato <- stato %>% count(stato)
stato <- stato %>% mutate(stato = factor(stato, levels = c("Attive", "Inattive", "Untracked")))


statoGRAPH <- stato %>%
  ggplot(aes(x = stato, y = n, fill = stato,
             text = paste("Stato:", stato, "<br>", "Crypto:", n))) +geom_col(colour = "grey40") +
  labs(
    title = "Numero di cryptovalute su CMC",
    x = NULL,
    y = NULL
  ) +
  scale_fill_manual(values = c("Attive" = "skyblue2", "Inattive" = "#F17D7D", "Untracked" = "grey55")) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid',colour = "grey"),
    panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',colour = "grey"),
    text = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  )

ggplotly(statoGRAPH, tooltip = ("text"))

#------------------------------------------------

#fetta di mercato di bitcoin, ethereum, tether e altre crypto
marketcap_DOM <- crypto_listings(which = "historical", quote = TRUE, start_date = "20130429", end_date = "20260429", interval = "year", limit = 50000)

marketcap_DOM <- marketcap_DOM %>% left_join(andamento, by = c("date" = "timestamp"))

#market cap % disponibile solo per bitcoin, le altre vanno ricavate
marketcap_DOM <- marketcap_DOM %>% mutate(fetta = btc_dominance * market_cap / market_cap[slug == "bitcoin"], .by = date) %>% filter(slug %in% c("bitcoin", "ethereum", "tether"))

marketcap_DOM <- marketcap_DOM %>% mutate(Anno = format(date, "%Y"), Crypto = symbol)

#market cap % per tutte le altre crypto
marketcap_DOM <- marketcap_DOM %>% bind_rows(marketcap_DOM %>% group_by(Anno) %>% summarise(Crypto = "Altre", fetta = 100 - sum(fetta)))

marketcap_DOM <- marketcap_DOM %>% mutate(Crypto = factor(Crypto, levels = c("BTC", "ETH", "USDT", "Altre")))

#creo grafico, se assente text = 0 se 
marketcap_crypto <- marketcap_DOM %>%
  ggplot(aes(x = Anno, y = fetta, fill = Crypto,
             text = paste("Anno:", Anno, "<br>", Crypto, ":", paste0(round(fetta, 1), "%")))) +
  geom_col(position = position_stack(reverse = TRUE), width = 0.8, colour = "white", linewidth = 0.3)+
  geom_text(aes(label = ifelse(fetta >= 4, paste0(round(fetta), "%"), "")),
            position = position_stack(vjust = 0.5, reverse = TRUE), size = 2.8, colour = "grey15")+
  labs(
    title = "Market cap percentuale di BTC, ETH, USDT e altre crypto",
    x = NULL,
    y = NULL,
    fill = NULL
  )+
  scale_fill_manual(values = c("BTC" = "orange", "ETH" = "skyblue2", "USDT" = "#F17D7D", "Altre" = "grey60"))+
  scale_y_continuous(labels = label_percent(scale = 1), expand = c(0, 0))+
  theme_minimal(base_size = 11)+
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )

ggplotly(marketcap_crypto, tooltip = ("text"))

#------------------------------------------------



################################################################################################################################
################################################################################################################################

#TODO E IDEE

#grafico più interessante è marketcap ma non si vede bene, abbellirlo o animazione di spostamento (per vedere 3/4 colonne alla volta)
#top capitalizzazione oggi (da confontare con passato? oggi è più o meno indicativo)


#a seconda di cosa si può fare per il pacchetto, si potrebbe fare anche un misto tra data art e analisi crypto (??) in alternativa controllo di quanto
#lanciare una crypto influenzi i big (bitcoin ethereum) o quanto shitcoin si influenzino a vicenda

#altra idea quanto tempo durano shitcoin prima di rugpull

#la parola coin influenza successo di una crypto (togliendo bitcoin che fa da "riferimento")

#PARTE 3 CURIOSITA tenere controllata quotazione di Sony (06/10 avanti)
