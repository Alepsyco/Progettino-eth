#OPERAZIONI PRELIMINARI
install.packages("a")
remove.packages("a")

library("httr2")
library("dplyr")
library("purrr")
library("ggplot2")
library("plotly")
library("scales")
library("igraph")
library("tidyverse")
library("conflicted")
library("moments")

set.seed(1984)

#Rete delle transazioni Ethereum con nodi = indirizzi, archi = transazioni (peso = numero / quantità trasferita)

#gwei = unità di misura del prezzo del gas, wei è l'unità più piccola di Ethereum 1 ETH = 10^18 wei
#gas è commissione che si paga per fare transazione in un blocco quando la rete ha tante transazioni il costo gas sale


#Disclaimer sull'uso dell’AI: il codice è stato in parte generato con Claude e poi corretto a mano. 
#Le scelte di progetto (idee, direzione, giornate da analizzare) e le interpretazioni dei risultati sono personali.
##########################################################################################
#param

#setwd in cartella con ETH.R
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#primo e ultimo blocco analizzato, 12 marzo 2020 (covid) circa 6500 blocchi
start <- 9653384
end <- 9659831

#pubblico no key
endpoint <- "https://eth-mainnet.g.alchemy.com/public"


##########################################################################################
#prep apicall

getblocco <- function(num) {
  Sys.sleep(0.1)     #rallenta call
  
  #costruzione chiamata api (endpoint pubblico)
  response <- request(endpoint) %>%
    req_body_json(
      list(
        jsonrpc = "2.0",
        method = "eth_getBlockByNumber",
        params = list(sprintf("0x%x", num), TRUE),id = 1))
  
  response <- response %>% req_perform() %>% resp_body_json(simplifyVector = TRUE)  #invia e legge risposta
  
  
  transactions = response$result$transactions %>% as_tibble() #crea tabella da risposta
  
  if (nrow(transactions) == 0) return(NULL)     #blocco senza transazioni salto
  
  transactions <- transactions %>% mutate(timeStamp = response$result$timestamp) #JSON-RPC ha timestamp in blocco e non in transazione
  
  #SE INIZIA PER 0x ho solo indirizzo, quindi niente istruzioni quindi niente contratto
  #trasferimento eth: scambio eth
  #contratto: altre operazioni (scambio token, ntf, votazioni per DAO)
  
  transactions <- transactions %>% mutate(tipo = ifelse(input == "0x", "trasferimento eth", "contratto"))
  
  #leggo dati estratti come numeri HEX
  transactions <- transactions %>%
    transmute(
      block = as.numeric(blockNumber),
      time = as.POSIXct(
        as.numeric(timeStamp),
        origin = "1970-01-01",
        tz = "UTC"
      ),
      hash, #id 
      from, #invia
      to, #riceve (NA se creo contratto ma sono <1%)
      value_eth = as.numeric(value) / 1e18,  #valore eth in wei (1 ETH = 10^18 wei)
      gas_price_gwei = as.numeric(gasPrice) / 1e9, #prezzo gas in wei (10^9 wei)
      tipo #da sopra 
    )
  transactions
}

#controlla se esiste file per evitare apicall
if (file.exists("transactions.rds")) {
  transactions <- readRDS("transactions.rds")
} else {
#scarica ogni blocco e aggiunge a altre righe già presenti (salto 200 blocchi vuoti)
  transactions <- map_dfr(start:end, function(n) tryCatch(getblocco(n), error = function(e) NULL))
  saveRDS(transactions, "transactions.rds")

}

transactions <- transactions %>% dplyr::filter(block >= start, block <= end)

##########################################################################################
#funzioni di prova (da link)


transactions %>% summarise("Numero transazioni" = n(), 
                           "Tot movimenti eth" = sum(value_eth), 
                           "Media movimenti eth" = mean(value_eth))


transactions %>%
  mutate(date = as.POSIXct(trunc(time, "hours"))) %>%    
  count(date) %>%
  ggplot(aes(date, n)) +
  geom_line() +
  labs(
    title = "Ethereum transactions over time",
    x = NULL,
    y = "Number of transactions"
  ) + theme_minimal()


transactions %>%
  dplyr::filter(value_eth > 0) %>%
  ggplot(aes(value_eth)) +
  geom_histogram(bins = 50) +
  scale_x_log10() +
  labs(
    title = "Distribution of transaction values (log-n)",
    x = "ETH transferred",
    y = "Transactions"
  ) + theme_minimal()



# who are the major senders?
count(transactions, from, sort = TRUE)    
# who are the major receivers?
count(transactions, to, sort = TRUE)     

##########################################################################################
##########################################################################################
#anaisi principale

#tipo transazioni


tiponum <- transactions %>% group_by(tipo) %>% summarise(Transazioni = n(), "ETH trasferiti" = sum(value_eth))
tiponum <- tiponum %>% pivot_longer(-tipo, names_to = "misura", values_to = "valore")     #una riga per tipo e per misura
tiponum <- tiponum %>% group_by(misura) %>% mutate(quota = valore / sum(valore)) %>% ungroup()     #quota di ogni tipo dentro la misura

#nome di ogni tipo scritto sopra il suo segmento nella barra delle transazioni (centrato sul segmento)
etichette <- tiponum %>% dplyr::filter(misura == "Transazioni")
etichette <- etichette %>% mutate(x = cumsum(quota) - quota / 2, nome = c("Chiamate a contratti", "Trasferimenti semplici di ETH"))

tipoGRAPH <- tiponum %>%
  ggplot(aes(x = quota, y = misura, fill = tipo,
             text = paste("Tipo:", tipo, "<br>", misura, ":", comma(valore, big.mark = ".", decimal.mark = ",", accuracy = 1), "<br>", "Quota:", paste0(round(100 * quota, 1), "%")))) +
  geom_col(width = 0.55, colour = "white", linewidth = 1, position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = paste0(round(100 * quota), "%"), colour = tipo), position = position_stack(vjust = 0.5, reverse = TRUE), size = 4) +
  geom_text(data = etichette, aes(x = x, y = misura, label = nome), inherit.aes = FALSE, nudge_y = 0.42, size = 3.4, colour = "#52514e") +
  labs(
    title = "Due transazioni su tre sono chiamate a contratti,\nma muovono solo il 13% degli ETH",
    x = NULL,
    y = NULL
  ) +
  scale_fill_manual(values = c("contratto" = "#3e5c8a", "trasferimento eth" = "#dcc7a0")) +
  scale_colour_manual(values = c("contratto" = "white", "trasferimento eth" = "#1a1a1a")) +
  scale_x_continuous(labels = label_percent(), expand = expansion(mult = c(0, 0.02))) +
  scale_y_discrete(expand = expansion(add = c(0.4, 0.7))) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    text = element_text(size = 10, colour = "#52514e"),
    plot.title = element_text(face = "bold", size = 14, colour = "#0b0b0b"),
    axis.text.x = element_text(colour = "#898781"),
    axis.text.y = element_text(colour = "#0b0b0b", size = 11),
    legend.position = "none"
  )

ggplotly(tipoGRAPH, tooltip = ("text"), height = 220)

##########################################################################################
#creazione grafi per analisi
#escluse 18.580 transazioni (circa 2%): 16.345 con self e 2.235 creazioni di contratto (per semplificare analisi)

#rete A: contratti + trasferimenti eth (completa)
edgeA <- transactions %>% dplyr::filter(!is.na(to), from != to)
edgeA <- edgeA %>% summarise(n_tx = n(), eth = sum(value_eth), .by = c(from, to))

graphA <- graph_from_data_frame(edgeA, directed = TRUE)     #n_tx ed eth diventano attributi degli archi

#rete B: solo trasferimenti eth
edgeB <- transactions %>% dplyr::filter(tipo == "trasferimento eth", from != to)
edgeB <- edgeB %>% summarise(n_tx = n(), eth = sum(value_eth), .by = c(from, to))

graphB <- graph_from_data_frame(edgeB, directed = TRUE)

#confronto tra le due reti (wCC, degree medio mediano e reciprocità)
compA <- components(graphA, mode = "weak")
compB <- components(graphB, mode = "weak")

confronto <- data.frame(rete = c("A: contratto + trasferimento eth", "B: trasferimenti eth"),
                        nodi = c(vcount(graphA), vcount(graphB)),
                        archi = c(ecount(graphA), ecount(graphB)),
                        gradomedio = c(mean(degree(graphA)), mean(degree(graphB))),
                        gradomediano = c(median(degree(graphA)), median(degree(graphB))),
                        componentemax_n = c(max(compA$csize), max(compB$csize)),     #nodi della componente connessa maggiore
                        componentemax = c(max(compA$csize) / vcount(graphA), max(compB$csize) / vcount(graphB)),     #quota sul totale dei nodi
                        reciprocita = c(reciprocity(graphA), reciprocity(graphB)))     #quota di archi che hanno anche l'arco inverso

print(confronto)

#nodi con più connessioni, con peso numero di transazioni (la parte con peso eth è più sotto)
hubA <- data.frame(indirizzo = V(graphA)$name,
                   grado = degree(graphA),
                   grado_in = degree(graphA, mode = "in"),    
                   trns = strength(graphA, weights = E(graphA)$n_tx),
                   eth = strength(graphA, weights = E(graphA)$eth),
                   row.names = NULL)

hubA <- hubA %>% arrange(desc(grado)) %>% head(10)

hubB <- data.frame(indirizzo = V(graphB)$name,
                   grado = degree(graphB),
                   grado_in = degree(graphB, mode = "in"),
                   trns = strength(graphB, weights = E(graphB)$n_tx),
                   eth = strength(graphB, weights = E(graphB)$eth),
                   row.names = NULL)
hubB <- hubB %>% arrange(desc(grado)) %>% head(10)

###########################################################################################

#gradi sono collegamenti in+out tra nodi, indipendentemente da numero transazioni tra di loro
#distribuzione dei gradi, scala log-log

degrees <- bind_rows(data.frame(rete = "A: tutte le transazioni", grado = degree(graphA)),
                   data.frame(rete = "B: solo trasferimenti eth", grado = degree(graphB)))
degrees <- degrees %>% count(rete, grado)

#CCDF
#per ogni grado k, quota di nodi con grado >= k (così si verifica la legge di potenza)
degrees <- degrees %>% group_by(rete) %>% mutate(ccdf = rev(cumsum(rev(n))) / sum(n)) %>% ungroup()

degreesGRAPH <- degrees %>%
  ggplot(aes(x = grado, y = ccdf, colour = rete, group = rete,
             text = paste("Grado:", grado, "<br>", "Nodi con grado >=:", paste0(round(100 * ccdf, 4), "%")))) +
  geom_line(linewidth = 0.9) +
  labs(
    title = "Quasi tutti gli indirizzi hanno pochi collegamenti,\npochissimi ne hanno migliaia",
    x = "Grado",
    y = "quota nodi >= k",
    colour = NULL
  ) +
  scale_colour_manual(values = c("A: tutte le transazioni" = "#3e5c8a", "B: solo trasferimenti eth" = "#a98448")) +
  scale_x_log10(breaks = c(1, 10, 100, 1000, 10000), labels = label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_log10(breaks = 10^(-5:0), labels = function(x) paste0(format(100 * x, scientific = FALSE, drop0trailing = TRUE, decimal.mark = ","), "%")) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#e1e0d9", linewidth = 0.4),
    text = element_text(size = 10, colour = "#52514e"),
    plot.title = element_text(face = "bold", size = 14, colour = "#0b0b0b"),
    axis.text = element_text(colour = "#898781"),
    legend.position = "bottom"
  )

ggplotly(degreesGRAPH, tooltip = ("text"))

##########################################################################################

#per grafo A o B
directed <- graphA
undirected <- as_undirected(directed, mode = "collapse", edge.attr.comb = list(n_tx = "sum", eth = "sum"))

#indirizzi a contratti (ricevuto chiamata con input)
contratti <- setdiff(unique(transactions$to[transactions$tipo == "contratto"]), transactions$from)     #tolgo chi invia transazioni, un contratto non ne invia (978 indirizzi erano portafogli)

#GRADI
#PROBLEMA: ci sono hub enormi (skewness 300) se tolti i primi 10 vado già a skewness 60 
#significa che ho 10 nodi con gradi altissimi e "singolari" che dominano il grafico (ci sono molti buchi tra es grado 500 e grado 30908)
#per risolvere uso CCDF per creare grafo in cui confronto quanti nodi hanno grado >= k, se simile alla retta indica legge di potenza (?)

grado <- degree(directed) 
skewness(grado) 

ccdf <- data.frame(grado = sort(unique(grado)))
ccdf$quota <- sapply(ccdf$grado, function(k) mean(grado >= k))     #nodi con grado >= k
summary(lm(log10(quota) ~ log10(grado), data = ccdf))           #retta grafico

alfa <- 1 - coef(lm(log10(quota) ~ log10(grado), data = ccdf))[2]     #la pendenza della retta è 1 - alfa: alfa è l'esponente della legge di potenza

ggplot(ccdf, aes(x = grado, y = quota)) +
  geom_point(color = "#3e5c8a", size = 1.8, alpha = 0.85) +
  geom_smooth(method = "lm", color = "#52514e", linetype = "dashed", linewidth = 0.8, se = FALSE) +
  labs(x = "Grado", y = "quota nodi >= k",
       title = paste0("I gradi seguono quasi una retta in scala log-log:\nlegge di potenza con esponente circa ", format(round(alfa, 1), nsmall = 1, decimal.mark = ","))) +
  scale_x_log10(breaks = c(1, 10, 100, 1000, 10000), labels = label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_log10(breaks = 10^(-5:0), labels = function(x) paste0(format(100 * x, scientific = FALSE, drop0trailing = TRUE, decimal.mark = ","), "%")) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#e1e0d9", linewidth = 0.4),
    text = element_text(size = 10, colour = "#52514e"),
    plot.title = element_text(face = "bold", size = 14, colour = "#0b0b0b"),
    axis.text = element_text(colour = "#898781")
  )

cat("Nodo con grado più alto:", names(which.max(grado)), "con", max(grado), "connessioni\n")


##########################################################################################
#dist e diamtro

comp <- components(undirected)
lscc <- induced_subgraph(undirected, which(comp$membership == which.max(comp$csize)))

campione <- sample(vcount(lscc), 2000)     #2000 nodi a caso da cui partire

distanze <- distances(lscc, v = campione)     #per ognuno, i passi verso tutti gli altri
distanze <- distanze[distanze > 0]     #tolgo gli 0 (ogni nodo con se stesso)

mean(distanze)     #distanza media
max(distanze)     #distanza massima trovata
table(distanze)     #quante coppie a ogni distanza


##########################################################################################
#comunità

#cluster_louvain divide la rete in gruppi connessi tra loro e pesi usiamo numero transazioni 
comunita <- cluster_louvain(undirected, weights = E(undirected)$n_tx)
V(undirected)$community <- membership(comunita)     #membership da comunità di appartenenza

#modularity misura la divisione tra le comunità, tra 0 e 1
cat("Numero di comunità:", length(comunita), "Divisione:", round(modularity(comunita), 3), "\n")

comunitaeth <- cluster_louvain(undirected, weights = E(undirected)$eth)
compare(comunita, comunitaeth, method = "nmi")

#crossing bool per ogni arco se collega nodi di altre comunità
E(undirected)$cross_community <- igraph::crossing(comunita, undirected)
table(E(undirected)$cross_community)     

#sizes per numero di nodi di ogni comunità
dimensioni <- as.numeric(sizes(comunita))
summary(dimensioni)     #minimo, mediana, media e massimo delle dimensioni

#quota di nodi della rete che sta in ognuna delle 20 comunità più grandi
grandi <- data.frame(posizione = 1:20, quota = head(sort(dimensioni, decreasing = TRUE), 20) / vcount(undirected))

ggplot(grandi, aes(x = factor(posizione), y = quota, fill = posizione <= 10)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("TRUE" = "#3e5c8a", "FALSE" = "#7d97bb")) +
  labs(x = "Comunità (dalla più grande alla ventesima)", y = "Quota di nodi della rete",
       title = paste0("Le 10 comunità più grandi contengono il ", round(100 * sum(grandi$quota[1:10])), "% degli indirizzi"),
       subtitle = paste0("Su ", format(length(comunita), big.mark = ".", decimal.mark = ","), " comunità trovate: quasi tutte le altre sono piccolissime")) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.08))) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(colour = "#e1e0d9", linewidth = 0.4),
    text = element_text(colour = "#52514e"),
    plot.title = element_text(face = "bold", size = 14, colour = "#0b0b0b"),
    axis.text = element_text(colour = "#898781"),
    legend.position = "none"
  )

#le comunità sono composte da hub + vari clienti 
##########################################################################################
#assortatività e centralità

#assortatività per grado, da -1 a 1 misura nodi con molte connessioni sono collegati a nodi con grado alto o basso
assortativity_degree(directed)

#transitività globale, da 0 a 1
transitivity(undirected, type = "global")
#visto che è quasi 0 (ma comunque c'è) struttura a stella, se collegato ad hub (contratto) quasi mai si scambiano eth

#betweenness centrality misura quanti percorsi più brevi passano da ogni nodo (riduco per tempi)
betweensubraph <- induced_subgraph(undirected, order(coreness(undirected), degree(undirected), decreasing = TRUE)[1:5000])     

betweennessdf <- data.frame(name = V(betweensubraph)$name, betweenness = betweenness(betweensubraph, weights = NA, normalized = TRUE), contratto = V(betweensubraph)$name %in% contratti)
betweennessdf %>% arrange(desc(betweenness)) %>% head(10)

#pagerank dei nodi
head(sort(page_rank(directed, weights = E(directed)$n_tx)$vector, decreasing = TRUE), 10)
head(sort(page_rank(directed, weights = E(directed)$eth)$vector, decreasing = TRUE), 10)


##########################################################################################
#archi con peso eth

#grado pesato

#forza è grado pesato (sommo movimenti totali eth per indirizzo + bool per contratto)
forza <- data.frame(indirizzo = V(directed)$name, grado = degree(directed), eth = strength(directed, weights = E(directed)$eth), contratto = V(directed)$name %in% contratti)

#i 10 indirizzi con più eth scambiati, con il loro grado (quelli con più collegamenti sono in hubA)
forza %>% arrange(desc(eth)) %>% head(10)

#uso spearman (controlla ordine valori piuttosto che valori in se) come correlazione per evitare che gradi alti rovinino
#se 0 no legame se 1/-1 chiara correlzione positiva o negativa
cor(forza$grado, forza$eth, method = "spearman")

#info generiche contratti vs trasferimenti eth, i contratti sono gli hub (molti hanno 0 forza)
forza %>% group_by(contratto) %>% summarise(nodi = n(), grado_medio = mean(grado), eth_mediano = median(eth))

#tolgo eth 0 per scala logaritmica e valori piccolissimi che rovinano visualizzazione
forzaGRAPH <- forza %>% dplyr::filter(eth > 1e-6) 
  
legg <- function(x) format(x, scientific = FALSE, drop0trailing = TRUE, big.mark = ".", decimal.mark = ",")

ggplot(forzaGRAPH, aes(x = grado, y = eth)) +
  geom_bin_2d(bins = 40) +
  scale_x_log10(breaks = 10^(0:4), labels = legg) +
  scale_y_log10(breaks = 10^seq(-6, 6, 2), labels = legg) +
  scale_fill_gradientn(colours = c("#dbe4f0", "#8fa8c9", "#3e5c8a", "#1d3050"), transform = "log10", name = "Indirizzi") +     #dal chiaro (pochi indirizzi) allo scuro (tanti)
  facet_wrap(~ contratto, labeller = as_labeller(c("FALSE" = "Indirizzi normali", "TRUE" = "Contratti"))) +
  labs(title = "Il numero di collegamenti spiega poco gli ETH mossi",
       subtitle = paste0("Correlazione di rango ", format(round(cor(forza$grado, forza$eth, method = "spearman"), 2), decimal.mark = ","),
                         ": con un solo collegamento si arriva fino a ", format(round(max(forza$eth[forza$grado == 1]), -3), big.mark = ".", decimal.mark = ","), " ETH"),
       x = "Grado", y = "ETH trasferiti") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(colour = "#e1e0d9", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines"),
    strip.text = element_text(face = "bold", hjust = 0, colour = "#0b0b0b", size = 12),
    text = element_text(colour = "#52514e"),
    plot.title = element_text(face = "bold", size = 14, colour = "#0b0b0b"),
    axis.text = element_text(colour = "#898781")
  )

#i 10 trasferimenti più grandi valgono il 15% di tutti gli ETH mossi, i primi 100 il 40,9%, i primi 1.000 il 73,1%
#il più grande è da Bitfinex 3 a Bitfinex 2, poi Binance verso Binance 2, 3 e 4
#spiega perché grado ed ETH sono poco legati, Bitfinex 2 ha 2 collegamenti ma muove 241.052 ETH

##########################################################################################
#resilienza rete B (rete A ha poco senso, gli hub sono solo contratti)
#se tolgo un nodo l'indirizzo smette di funzionare (exchange o pool di mining bloccato)

undirectedB <- as_undirected(graphB, mode = "collapse")     
ordineB <- names(sort(degree(graphB), decreasing = TRUE))     

componenteB <- function(nodi) max(components(delete_vertices(undirectedB, nodi))$csize) / vcount(undirectedB)

#tolgo i nodi più connessi contro lo stesso numero di nodi a caso (attacco vs guasto)
rimossi <- c(0, 5, 10, 25, 50, 100, 200) 
resilienza <- data.frame(rimossi = rimossi,
                         tolti_hub = sapply(rimossi, function(k) componenteB(ordineB[seq_len(k)])),
                         tolti_casuali = sapply(rimossi, function(k) componenteB(sample(vcount(undirectedB), k))))
print(resilienza)

ggplot(rbind(data.frame(rimossi, quota = resilienza$tolti_hub, scelta = "i più connessi"),
             data.frame(rimossi, quota = resilienza$tolti_casuali, scelta = "a caso")), aes(x = rimossi, y = quota, colour = scelta)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  annotate("text", x = max(rimossi), y = tail(resilienza$tolti_hub, 1), label = "si tolgono i più connessi", hjust = -0.08, size = 3.6, colour = "#3e5c8a", fontface = "bold") +
  annotate("text", x = max(rimossi), y = tail(resilienza$tolti_casuali, 1), label = "si tolgono a caso", hjust = -0.08, size = 3.6, colour = "#52514e") +
  scale_colour_manual(values = c("i più connessi" = "#3e5c8a", "a caso" = "#898781")) +
  scale_x_sqrt(breaks = rimossi, expand = expansion(mult = c(0.02, 0.38))) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(x = "Indirizzi tolti (scala radice quadrata)", y = "Quota di indirizzi nella componente maggiore",
       title = paste0("Togliendo i 100 indirizzi più connessi la componente scende al ", round(100 * resilienza$tolti_hub[rimossi == 100]),
                      "%;\ntogliendone 100 a caso resta al ", round(100 * resilienza$tolti_casuali[rimossi == 100]), "%")) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#e1e0d9", linewidth = 0.4),
    text = element_text(colour = "#52514e"),
    plot.title = element_text(face = "bold", size = 14, colour = "#0b0b0b"),
    axis.text = element_text(colour = "#898781"),
    legend.position = "none"
  )

#i primi 10 hub tolti uno alla volta: ricevono o pagano e quanti indirizzi restano senza più collegamento con il resto
quota10 <- sapply(1:10, function(k) componenteB(ordineB[1:k]))
hubresilienza <- data.frame(indirizzo = ordineB[1:10],
                            entranti = degree(graphB, mode = "in")[ordineB[1:10]],
                            uscenti = degree(graphB, mode = "out")[ordineB[1:10]],
                            quota_componente = quota10,
                            indirizzi_fuori = round((componenteB(character(0)) - quota10) * vcount(undirectedB)) - 1:10,     #oltre ai nodi tolti
                            row.names = NULL)
print(hubresilienza)

#tolgliere hub fa uscire indirizzi dalla componente maggiore in due modi: *N1* restano soli (arco singolo) *N2* si staccano in cluster minori che passavano da lui
    #pool mining: quasi solo N1, i miner ricevono un pagamento e basta (essendo giornaliero)
    #exchange: N1/N2 i clienti fanno anche altro quindi formano cluster che passano da lì 
    #se tolgo i 5 pool più connessi scc scende dal 75% al 63%
    #se tolgo i 10 exchange 66% 
    #se tolgo tutti insieme 53%
##########################################################################################

#Giornata scelta molto movimentata (crollo mercati) vedere se slot di 6 ore cambiano in qualche maniera la rete o solo attività

#creazione finestre
trnswindow <- transactions %>% mutate(ora = as.numeric(format(time, "%H")))
trnswindow <- trnswindow %>% mutate(window = cut(ora, breaks = c(-1, 5, 11, 17, 23), labels = c("00-06", "06-12", "12-18", "18-24")))

#attività per finestra: transazioni, gas medio (gwei) ed ETH mossi
windowdf <- trnswindow %>% group_by(window)
windowdf <- windowdf %>% summarise(tx = n(),
                                   media_gas = mean(gas_price_gwei),
                                   eth_tot = sum(value_eth))
print(windowdf)

#ditto costruzione rete (vedi sopra)
reti <- data.frame()
for (i in levels(trnswindow$window)) {
  edgefin <- trnswindow %>% dplyr::filter(window == i, !is.na(to), from != to)
  edgefin <- edgefin %>% summarise(n_tx = n(), eth = sum(value_eth), .by = c(from, to))
  g <- graph_from_data_frame(edgefin, directed = TRUE)
  compfin <- components(g, mode = "weak")
  reti <- rbind(reti, data.frame(finestra = i,
                                 nodi = vcount(g),
                                 archi = ecount(g),
                                 gradomean = mean(degree(g)),
                                 compmax = max(compfin$csize) / vcount(g)))
}
print(reti)

#gas mediano ora per ora
ore <- trnswindow %>% group_by(ora)
ore <- ore %>% summarise(gas_mediano = median(gas_price_gwei))

gasGRAPH <- ore %>%
  ggplot(aes(x = ora, y = gas_mediano, group = 1,
             text = paste0("", ora,":00", "<br>", "Gas mediano:", round(gas_mediano, 1), "gwei"))) +
  geom_line(colour = "#3e5c8a", linewidth = 0.9) +
  geom_point(colour = "#3e5c8a", size = 2) +
  annotate("text", x = ore$ora[which.max(ore$gas_mediano)] + 3.5, y = max(ore$gas_mediano), size = 3.6, colour = "#0b0b0b",
           label = paste0("Picco: ", round(max(ore$gas_mediano)), " gwei alle ", ore$ora[which.max(ore$gas_mediano)], ":00")) +
  labs(
    title = paste0("Il prezzo mediano del gas passa da ", round(min(ore$gas_mediano)), " a ", round(max(ore$gas_mediano)), " gwei:\nil picco è alle ", ore$ora[which.max(ore$gas_mediano)], ":00 UTC"),
    x = "Ora (UTC)",
    y = "mediana gwei"
  ) +
  scale_x_continuous(breaks = seq(0, 23, 3), labels = function(x) sprintf("%02d:00", x)) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.15))) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#e1e0d9", linewidth = 0.4),
    text = element_text(size = 10, colour = "#52514e"),
    plot.title = element_text(face = "bold", size = 14, colour = "#0b0b0b"),
    axis.text = element_text(colour = "#898781")
  )

ggplotly(gasGRAPH, tooltip = ("text"))


#non ci sono grandi cambi di struttura della rete tra le ore, soltanto un aumento di gwei che segnala movimenti
#Anche se struttura della rete cambia poco cambiano i momenti in cui agiscono gli hub (binance tutto il giorno, alcuni solo la mattina)


##########################################################################################
#approfondimenti suggeriti 
#dopo studio della rete confronto più realistico


#Hub (sia A che B): cosa sono effettivamente
#fonte etherscan

id <- tribble(~indirizzo, ~nome, ~categoria,
              "0xdac17f958d2ee523a2206206994597c13d831ec7", "USDT", "token",
              "0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48", "USDC", "token",
              "0x6b175474e89094c44da98b954eedeac495271d0f", "DAI", "token",
              "0x514910771af9ca656af840dff83e8264ecf986ca", "LINK", "token",
              "0x9e9801bace260f58407c15e6e515c45918756e0f", "WorldUnitedCoins", "token",
              "0x794e6e91555438afc3ccf1c5076a74f42133d08d", "OasisDEX", "scambio decentralizzato",
              "0x2a0c0dbecc7e4d658f48e01e3fa353f44050c208", "Exchange", "scambio decentralizzato",
              "0xd8a04f5412223f513dc55f839574430f5ec15531", "Maker Flip ETH A", "aste di liquidazione",
              "0xea674fdde714fd979de3edf0f56aa9716b898ec8", "Ethermine", "pool di mining",
              "0x52bc44d5378309ee2abf1539bf71de1b7d7be3b5", "Nanopool", "pool di mining",
              "0x8fd00f170fdf3772c5ebdcd90bf257316c69ba45", "Spark Pool", "pool di mining",
              "0x829bd824b016326a401d083b33d092293333a830", "F2Pool", "pool di mining",
              "0x4c549990a7ef3fea8784406c1eecc98bf4211fa5", "Hiveon", "pool di mining",
              "0x3f5ce5fbfe3e9af3971dd833d26ba9b5c936f0be", "Binance", "exchange",
              "0xd551234ae421e3bcba99a0da6d736074f22192ff", "Binance 2", "exchange",
              "0x564286362092d8e7936f0549571a803b203aaced", "Binance 3", "exchange",
              "0x0681d8db095565fe8a346fa0277bffde9c0edbbf", "Binance 4", "exchange",
              "0xa12431d0b9db640034b0cdfceef9cce161e62be4", "HitBTC", "exchange",
              "0xd7b9a9b2f665849c4071ad5af77d8c76aa30fb32", "Exmo", "exchange",
              "0xa7efae728d2936e78bda97dc267687568dd593f3", "OKX", "exchange",
              "0xc88f7666330b4b511358b7742dc2a3234710e7b1", "Blockchain.com", "exchange",
              "0xf5bec430576ff1b82e44ddb5a1c93f6f9d0884f3", "Yobit", "exchange",
              "0xfbb1b73c4f0bda4f67dca266ce6ef42f520fbb98", "Bittrex", "exchange",
              "0x8e870d67f660d95d5be530380d0ec0bd388289e1", "USDP", "token",
              "0x68e54af74b22acaccffa04ccaad13be16ed14eac", "WENI", "token",
              "0xbcf935d206ca32929e1b887a07ed240f0d8ccd22", NA, "token",
              "0x5e07b6f1b98a11f7e04e7ffa8707b63f1c177753", "MCH: Daily Action 3", NA)


#token = contratto di criptovaluta creata su eth, sposta gettoni USDT, USDC e DAI (stablecoin, valore agganciato al dollaro)
#scambio decentralizzato = contratto per scambiare token tra utenti senza intermediario, ogni chiamata è ordine o scambio
#aste di liquidazione = contratto di Maker (il sistema che crea la stablecoin DAI) dove si vende all'asta l'ETH dato in garanzia da chi ha preso DAI in prestito, succede quando il prezzo di ETH scende e la garanzia non basta più
#pool di mining = gruppo di miner che unisce la potenza di calcolo e si divide i premi dei blocchi, indirizzo paga ogni miner (tante uscite e nessuna entrata)
#exchange = piattaforma che compra e vende criptovalute, i suoi indirizzi ricevono i depositi dei clienti e pagano i prelievi (lo stesso exchange usa più indirizzi: Binance, Binance 2 ecc)
#non identificato = non trovato su Etherscan



#contratti (rete A): quanto ricevono e da quanti indirizzi diversi
trnscontratti <- transactions %>% dplyr::filter(tipo == "contratto")
contrattiHUB <- trnscontratti %>% summarise(chiamate = n(), chiamanti = n_distinct(from), .by = to)     #conto chiamate ricevute da nodo e quanti indirizzi le fanno
contrattiHUB <- contrattiHUB %>% mutate(quota = chiamate / sum(chiamate), chiamatemedie = chiamate / chiamanti)     #nchiamate medie per indirizzo
contrattiHUB <- contrattiHUB %>% arrange(desc(chiamate)) %>% head(10)   #top 10  
contrattiHUB <- contrattiHUB %>% left_join(id, by = c("to" = "indirizzo"))

#se etherscan non trova mostra indirizzo
contrattiHUB <- contrattiHUB %>% mutate(nome = coalesce(nome, paste0(substr(to, 1, 8), "...")),
                                        categoria = coalesce(categoria, "non identificato"),
                                        etichetta = paste0(nome, "\n", categoria),   #per ordine graph
                                        chiamanti_tipo = ifelse(chiamatemedie > 10, "pochi", "tanti")) #per fill graph

#top contratti identificati con analisi di chiamate
contrattiGRAPH <- contrattiHUB %>%
  ggplot(aes(x = quota, y = reorder(etichetta, quota), fill = chiamanti_tipo,
             text = paste0(nome, "<br>", "Chiamate: ", chiamate, " (", format(round(100 * quota, 1), decimal.mark = ","), "%)<br>",
                           "Indirizzi chiamanti: ", chiamanti, "<br>", "Chiamate per indirizzo: ", round(chiamatemedie, 1)))) +
  geom_col(width = 0.62) +
  geom_text(aes(x = quota + 0.014, label = paste0(format(round(100 * quota, 1), decimal.mark = ","), "%")), size = 3.4, colour = "#52514e") +
  annotate("text", x = 0.16, y = 8, label = "Chi chiama il contratto?", size = 3.6, colour = "#52514e", lineheight = 1) +
  annotate("text", x = 0.16, y = 7, label = "■ tanti indirizzi\npoche chiamate per indirizzo (< 5)", size = 3.6, colour = "#3e5c8a", lineheight = 1) +
  annotate("text", x = 0.16, y = 6, label = "■ pochi indirizzi\ntante chiamate per indirizzo (> 10)", size = 3.6, colour = "#a98448", lineheight = 1) +
  labs(
    title = paste0("Il ", round(100 * sum(contrattiHUB$quota)), "% delle chiamate va a 10 contratti\nsu ", format(n_distinct(trnscontratti$to), big.mark = ".", decimal.mark = ","), ": ",
                   contrattiHUB$nome[1], " da solo il ", round(100 * contrattiHUB$quota[1]), "%"),
    x = NULL,
    y = NULL
  ) +
  scale_fill_manual(values = c("tanti" = "#3e5c8a", "pochi" = "#a98448")) +
  scale_x_continuous(labels = percent, expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "#e1e0d9", linewidth = 0.4),
    text = element_text(size = 10, colour = "#52514e"),
    plot.title = element_text(face = "bold", size = 14, colour = "#0b0b0b"),
    axis.text = element_text(colour = "#52514e"),
    legend.position = "none"
  )

ggplotly(contrattiGRAPH, tooltip = ("text"))

#togliendo i contratti restano solo indirizzi (tipo graph B)
indirizziHUB <- data.frame(indirizzo = ordineB[1:15],
                           entranti = degree(graphB, mode = "in")[ordineB[1:15]],
                           uscenti = degree(graphB, mode = "out")[ordineB[1:15]],
                           row.names = NULL)
indirizziHUB <- indirizziHUB %>% mutate(grado = entranti + uscenti)
indirizziHUB <- indirizziHUB %>% left_join(id, by = "indirizzo")
indirizziHUB <- indirizziHUB %>% mutate(etichetta = ifelse(is.na(nome), paste0(substr(indirizzo, 1, 8), "... (non identificato)"), paste0(nome, " (", categoria, ")")))

indirizziGRAPH <- indirizziHUB %>%
  ggplot(aes(y = reorder(etichetta, grado))) +
  geom_col(aes(x = grado, fill = "invia a", text = paste0(etichetta, "<br>", "invia a: ", uscenti, " indirizzi")), width = 0.6, position = "identity") +     #barra intera, la parte blu la copre
  geom_col(aes(x = entranti, fill = "riceve da", text = paste0(etichetta, "<br>", "riceve da: ", entranti, " indirizzi")), width = 0.6, position = "identity") +
  labs(
    title = paste0("Dei ", nrow(indirizziHUB), " indirizzi più connessi\n", sum(indirizziHUB$categoria == "exchange", na.rm = TRUE), " sono exchange e ",
                   sum(indirizziHUB$categoria == "pool di mining", na.rm = TRUE), " pool di mining"),
    x = "Collegamenti",
    y = NULL,
    fill = NULL
  ) +
  scale_fill_manual(values = c("riceve da" = "#3e5c8a", "invia a" = "#a98448")) +
  scale_x_continuous(labels = label_number(big.mark = ".", decimal.mark = ","), expand = expansion(mult = c(0, 0.05))) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "#e1e0d9", linewidth = 0.4),
    text = element_text(size = 10, colour = "#52514e"),
    plot.title = element_text(face = "bold", size = 14, colour = "#0b0b0b"),
    axis.text = element_text(colour = "#52514e"),
    legend.position = "bottom"
  )

ggplotly(indirizziGRAPH, tooltip = ("text"))

#PRIMO GRAFICO
#principalmente usdt, i due non identificati sono bot (??) con pochi utenti che fanno chiamate ripetute
#4 posto è chiamata per confronto tra exchange Uniswap e Kyber per comprare/vendere dove conviene
#9 possibile portafoglio di bot ma non sicuro
#influenza il crollo, movimenti principali automatici

#SECONDO GRAFICO
#principalmente pool di mining che inviano, ed exchange misto
#2 exchange ricevono e basta? (solo un trasferimento per indirizzo, forse pagamenti su altro indirizzo non top 15?)
    #a quanto pare trasferisce gas a depositi per permettergli di spostare eth o token "gratuitamente"
#indirizzo non identificato tutti importi uguali (0,004eth, 18-38 gwei costante) mandati tutti a indirizzi diversi. (strano)
#binance ha indirizzi backup, migliora problema di attacchi teoria del grafo


##########################################################################################
#grafico a bolle per far vedere comunità, indirizzi più grandi (da usare all'inizio della presentazione?)
#dei top hub rete A bolle grandi quanto n collegamenti con parte piena n nodi che grado 1

hub <- head(sort(grado, decreasing = TRUE), 20)
vicini <- lapply(names(hub), function(h) neighbors(undirected, h)$name)     #indirizzi collegati a ogni hub
bbHUB <- data.frame(indirizzo = names(hub), collegati = as.vector(hub), g1 = sapply(vicini, function(v) sum(grado[v] == 1)))     #g1 grado 1
bbHUB <- bbHUB %>% left_join(id, by = "indirizzo")
bbHUB <- bbHUB %>% mutate(nome = coalesce(nome, paste0(substr(indirizzo, 1, 6), "...")),
                                categoria = coalesce(categoria, "non identificato"),
                                gruppo = ifelse(categoria %in% c("token", "scambio decentralizzato"), "token e scambi", categoria),
                                quota = g1 / collegati,
                                r = sqrt(collegati) / 100,
                                testo = paste0(nome, " (", categoria, ")<br>", format(collegati, big.mark = ".", decimal.mark = ",", trim = TRUE), " indirizzi collegati<br>",
                                               format(g1, big.mark = ".", decimal.mark = ",", trim = TRUE), " (", round(100 * quota), "%) hanno grado 1"))
#posizioni
bbHUB$x <- 0
bbHUB$y <- 0
for (i in 2:nrow(bbHUB)) {
  giro <- 0
  repeat {
    bbHUB$x[i] <- 0.06 * giro * cos(giro)
    bbHUB$y[i] <- 0.03 * giro * sin(giro)
    if (all(sqrt((bbHUB$x[i] - bbHUB$x[1:(i - 1)])^2 + (bbHUB$y[i] - bbHUB$y[1:(i - 1)])^2) >= bbHUB$r[i] + bbHUB$r[1:(i - 1)] + 0.3)) break
    giro <- giro + 0.02
  }
}

bbHUB$y <- bbHUB$y + 0.7 * (nchar(bbHUB$nome) > 12)   

cerchi <- bbHUB %>% cross_join(data.frame(a = seq(0, 2 * pi, length.out = 60))) 
colori <- c("pool di mining" = "#a98448", "exchange" = "#3e5c8a", "token e scambi" = "#7d97bb", "non identificato" = "#a3a19b")
toccati <- length(unique(c(bbHUB$indirizzo, unlist(vicini))))     

bbGRAPH <- ggplot() +
  geom_polygon(data = cerchi, aes(x = x + r * cos(a), y = y + r * sin(a), group = indirizzo, fill = gruppo, text = testo), alpha = 0.28) +     #cerchio sono collegati
  geom_polygon(data = cerchi, aes(x = x + r * sqrt(quota) * cos(a), y = y + r * sqrt(quota) * sin(a), group = indirizzo, fill = gruppo, text = testo), colour = "white", linewidth = 0.4) +     #cerchio pieno grado 1 
  geom_text(data = bbHUB, aes(x = x, y = y - r - 0.17 - 0.2 * (nchar(nome) > 12), label = paste0(str_wrap(nome, 12), "\n", round(100 * quota), "%")), size = 2.9, lineheight = 0.9, colour = "#52514e") +
  annotate("text", x = mean(range(bbHUB$x)) + diff(range(bbHUB$x)) * c(-0.2, 0.2, -0.2, 0.2), y = min(bbHUB$y - bbHUB$r) - c(0.85, 0.85, 1.25, 1.25), label = paste("●", names(colori)), size = 3.4, colour = unname(colori)) +
  annotate("text", x = mean(range(bbHUB$x)), y = min(bbHUB$y - bbHUB$r) - 1.75, label = "dimensione = n indirizzi collegati   |   pieno = n indirizzi collegati di grado 1", size = 3.2, colour = "#52514e") +
  scale_fill_manual(values = colori, guide = "none") +
  coord_equal() +
  labs(title = paste0("I 20 indirizzi più connessi toccano il ", round(100 * toccati / vcount(undirected)), "% della rete")) +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 14, colour = "#0b0b0b", margin = margin(b = 8)))

ggplotly(bbGRAPH, tooltip = ("text"))


primo1 <- function(x) sum(head(sort(x, decreasing = TRUE), ceiling(0.01 * length(x)))) / sum(x)

concentrazione <- data.frame(misura = c("collegamenti", "transazioni", "ETH"),
                             quota = c(primo1(degree(directed)), primo1(strength(directed, weights = E(directed)$n_tx)), primo1(strength(directed, weights = E(directed)$eth))))

concentrazioneGRAPH <- concentrazione %>%
  ggplot(aes(x = factor(misura, levels = misura), y = quota, fill = misura)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(100 * quota), "%")), vjust = -0.5, size = 4.5, colour = "#0b0b0b") +
  scale_fill_manual(values = c("collegamenti" = "#7d97bb", "transazioni" = "#3e5c8a", "ETH" = "#a98448")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = "Quota del totale",
       title = paste0("Il primo 1% degli indirizzi muove il ", round(100 * concentrazione$quota[3]), "% degli ETH"),
       subtitle = paste0("contro il ", round(100 * concentrazione$quota[2]), "% delle transazioni e il ", round(100 * concentrazione$quota[1]), "% dei collegamenti")) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(colour = "#e1e0d9", linewidth = 0.4),
    text = element_text(size = 10, colour = "#52514e"),
    plot.title = element_text(face = "bold", size = 14, colour = "#0b0b0b"),
    axis.text = element_text(colour = "#898781"),
    axis.text.x = element_text(colour = "#0b0b0b", size = 11),
    legend.position = "none"
  )

concentrazioneGRAPH


##########################################################################################



##########################################################################################
##########################################################################################
#Legge di potenza più visibile
#
#
##########################################################################################
##########################################################################################
