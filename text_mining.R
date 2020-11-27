# Cargamos las librerias
library(pdftools)
library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(quanteda)
library(dplyr)
library(qdap)
library(magrittr)
library(widyr)
library(tidyr)
library(topicmodels)
library(igraph)
library(ggraph)

# Modificar en función del sistema operativo correspondiente
platform <- "pc"

# FunciÃ³n para abrir ventanas y no se sobreescriban los plots en RStudio
OpenGraphDev <- function(width, height, platform) { 
  if (platform == "linux") { 
    x11(width = width, height = height) 
  } else if (platform == "pc") { 
    windows(width = width, height = height)
  } else if (platform == "mac") {
    quartz(width = width, height = height) 
  }
}

# Establezco directorio de trabajo donde tengo los PDFs
SOURCE_DIR <- "C:/Users/Usuario/Desktop/4º Ingenieria de la Salud/Biología de Sistemas/Ejercicio 1 Text Mining/PDFs"
setwd(SOURCE_DIR)

# Cargo los ficheros e imprimo sus nombres para verificar que están todos
files <- list.files(pattern = "pdf$")
print(files)

# Lista con los contenidos (texto) de cada pdf
opinions <- lapply(files, pdf_text)

# Revisamos que están todos
length(opinions)

# Lista de conceptos
v.corp <- VCorpus(VectorSource(opinions))

# Eliminamos de la anterior lista de conceptos aquellos que no superen
# las siguientes restricciones
options <- list(removePunctuation = TRUE, #.,;:
                stopwords = TRUE, # the, of, in 
                tolower = TRUE, 
                stemming = TRUE, # Get only root of the words
                removeNumbers = TRUE, 
                bounds = list(global = c(3, Inf))) # Frecuencia de las palabras

opinions.tdm = TermDocumentMatrix(v.corp, control = options) 
inspect(opinions.tdm)

# Visualizaciones de frecuencias de palabras PRE-FILTRADO
a <- as.matrix(opinions.tdm)

freq <- sort(rowSums(a), decreasing=TRUE)
names <- names(freq)
d <- data.frame(word=names, freq=freq)
print(d)

OpenGraphDev(7,7, platform)
wordcloud(words = d$word, freq = d$freq, min.freq = 10,
          max.words=500, random.order=FALSE, # rot.per=0.35, 
          colors=brewer.pal(10, "Paired"))

freq.pre.filter <- length(opinions.tdm$dimnames$Terms)

#### FILTRADO ####

## 1. Imponemos un umbral de frecuencia media mínima de aparición (HECHO)

ft <- findFreqTerms(opinions.tdm, lowfreq = 130, highfreq = Inf)
a.filt <- opinions.tdm[ft, ]
inspect(a.filt)

# Visualización de los datos filtrados
freq.a.filt <- sort(rowSums(as.matrix(a.filt)), decreasing=TRUE)
names.a.filt <- names(freq.a.filt)
data.frame.a.filt <- data.frame(word=names.a.filt, freq=freq.a.filt)

OpenGraphDev(8, 4, platform)
ggplot(data=data.frame.a.filt, aes(x=word, y=freq, group = 1)) +
  geom_point(aes(colour = freq), size = 0.7) +
  geom_line(stat = "identity", linetype = "dashed", size = 0.1, col = "steelblue") +
  theme_minimal() +  
  labs(title = "Freq filter data words") + 
  xlab("WORD") + ylab("FREQ") +
  theme(axis.title = element_text(face="bold", size = 8), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 5),
        plot.title = element_text(lineheight=2, face="bold", color="darkgray", size=12), 
        legend.title = element_blank())


## 2. Eliminamos palabras altamente específicas de ciertos documentos aislados

# Lista de valores distribuido de dicho porcentaje
sparce.values <- c(0.75, 0.5, 0.35, 0.25, 0.15)
freq.filt.no.sparce.total <- vector("list", length(sparce.values))

# Para cada valor calculamos el número de palabras que vamos obteniendo
for(i in 1:length(sparce.values)){
  freq.filt.no.sparce.total[[i]] <- length(removeSparseTerms(a.filt, sparce.values[i])$dimnames$Terms)
}

freq.filter <- length(a.filt$dimnames$Terms)
freq.total <- list(freq.filter, unlist(freq.filt.no.sparce.total))

data.frame.freq.total <- data.frame(FREQ = unlist(freq.total),
                           STAGES = c("min freq = 130", "sparce value = 0.75", 
                                      "sparce value = 0.5", "sparce value = 0.35", 
                                      "sparce value = 0.25", "sparce value = 0.15"))

OpenGraphDev(width = 7, height = 5, platform)
ggplot(data.frame.freq.total, aes(x = reorder(STAGES, -FREQ), y = FREQ, group = 1)) +
  geom_point(aes(y = FREQ, color = "FREQ"), col='steelblue') +
  geom_line(aes(y = FREQ, color = "FREQ"), linetype = "dashed", size = 0.3, col='steelblue') +
  
  labs(title = "Checking filter evolution") + theme_minimal() + 
  xlab("")+
  ylab("Nº PALABRAS")+
  theme(axis.title = element_text(face="bold", size = 6), 
        axis.text.x = element_text(size=7), 
        axis.text.y = element_text(size=10),
        plot.title = element_text(lineheight=2, face="bold", color="darkgray", size=12), 
        legend.title = element_blank())

# Observamos la necesidad de seleccionar un valor intermedio en 
# este filtrado para poder obtener resultados concluyentes
a.filt.no.sparse <- removeSparseTerms(a.filt, 0.3)
print(as.matrix(a.filt.no.sparse))

# Visualizaciones de frecuencias de palabras POST-FILTRADO
freq.filt.no.sparse <- sort(rowSums(as.matrix(a.filt.no.sparse)), decreasing=TRUE)
names.filt.no.sparse <- names(freq.filt.no.sparse)
d.filt.no.sparse <- data.frame(word=names.filt.no.sparse, freq=freq.filt.no.sparse)
print(d.filt.no.sparse[1:14, ], row.names = FALSE)

## Barplot final
OpenGraphDev(8, 3, platform)
ggplot(data=d.filt.no.sparse, aes(x=word, y=freq)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))



##### ANÁLISIS ALTERNATIVO CON QUANTEDA Y TOKEN ######

## Filtrado con tokens y dfm para eliminar las palabras no deseadas y especÃ­ficas de determinados documentos
token_filter <- tokens(corpus(v.corp), 
                       remove_punct = TRUE, 
                       remove_separators = TRUE, 
                       remove_numbers = TRUE, 
                       remove_symbols = TRUE) %>% 
  tokens_wordstem() %>%
  tokens_select(min_nchar=3L, selection = "remove", max_nchar = Inf) 

dfm_filter <- dfm(token_filter, 
                  # stem = TRUE, 
                  tolower = TRUE, 
                  remove = stopwords("english")) %>%
  dfm_trim(min_termfreq = 130, verbose = FALSE, min_docfreq = 0.8)

dfm_tm <- convert(dfm_filter, "tm")
dfm_df <- textstat_frequency(dfm_filter)
print(dfm_df[1:30])

OpenGraphDev(10, 7, platform)
ggplot(data=dfm_df, aes(x=feature, y=frequency)) +
  geom_bar(stat="identity", fill="steelblue", width = 0.5)+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5))

OpenGraphDev(7,7,platform)
textplot_wordcloud(dfm_filter, color = brewer.pal(10, "Paired"), random_order = FALSE)

## Creación de diccionario
dict <- dictionary(list(Computational = c("network", "method", "model", "function", "train", "deep", "algorithm", "control", "human", "test", "score"),
                        Biological = c("cell", "molecular", "gene", "cancer", "pathway", "receptor", "neuropeptid", "cellular", "protein", "pharmacolog", "inhibit"), 
                        General = c("drug", "target", "interact", "predict", "data", "ligand", "bind", "compound", "drug-target")))

dfm_dict <- dfm(token_filter, dictionary = dict)
dfm_dict_df <- convert(dfm_dict, to = "data.frame")

## Visualización de las palabras asociadas del diccionario
OpenGraphDev(9, 6, platform)
dfm_dict_df %>%
  gather("Type", "Value", -doc_id) %>%
  ggplot(aes(reorder(doc_id, -Value), Value, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) + 
  theme_minimal() + scale_fill_brewer(palette = "Paired", direction = -1) +
  xlab("Document") + labs(title = "Document classification") +
  theme(axis.title = element_text(face="bold", size = 8), 
        axis.text.x = element_text(size = 5), 
        axis.text.y = element_text(size = 5),
        plot.title = element_text(lineheight=2, face="bold", color="darkgray", size=12), 
        legend.title = element_blank(), 
        legend.text = element_text(face="bold", size = 8, color="darkgray"))


## Visualizamos la distribución de las palabras más representativas de los documentos
## Generamos dos gráficos para facilitar la visualización y las diferencias
OpenGraphDev(10, 6, platform)
theme_set(theme_bw())
g <- textplot_xray(
  kwic(token_filter, pattern = c("drug", "target", "protein", "interact", "predict", "network")))

g + aes(color = keyword) + 
  scale_color_brewer(palette = "Paired") +
  theme(legend.position = "none", legend.title = element_blank())

OpenGraphDev(10, 6, platform)
theme_set(theme_bw())
g <- textplot_xray(
  kwic(token_filter, pattern = c("data", "method", "cell", "model", "compound", "gene")))

g + aes(color = keyword) + 
  scale_color_brewer(palette = "Reds", direction = -1) +
  theme(legend.position = "none", legend.title = element_blank())

#### Generación de tópicos ####
lda_filter_7_topic <- LDA(convert(dfm_filter, to = "topicmodels"), k = 7)
get_terms(lda_filter_7_topic, 5)

## Asociaciones con las palabras que mÃ¡s se repiten
# Se ha probado tambiÃ©n con el opinion.dtm inicial y tampoco obtenemos resultados concluyentes
dfm_tm_no_filter <- convert(dfm(token_filter), "tm")
word.assoc <- findAssocs(dfm_tm_no_filter, c("drug", "target", "protein", "network"), rep(0.6, 4))
word.assoc.names <- c(names(word.assoc$drug), names(word.assoc$target), 
                      names(word.assoc$protein), names(word.assoc$network))

# OpenGraphDev(4,4,platform)
# wordcloud(words = word.assoc.names, 
          # freq = dfm_df$frequency, 
          # min.freq = 10,
          # max.words=500, 
          # random.order=FALSE, 
          # colors=brewer.pal(8, "Paired"))

###Red de correlación de documentos###

corpus.dfm=dfm(corpus(v.corp))
corpus.df=convert(corpus.dfm, "data.frame")

corpus.df_row <- corpus.df %>%
  gather("Word", "Value", -doc_id)

corpus.df_row_new<-corpus.df_row[corpus.df_row$Value>0,]

desc_word_pairs <- corpus.df_row_new %>% 
  pairwise_cor(doc_id, Word, sort = TRUE, upper = FALSE)

desc_word_pairs <- desc_word_pairs[desc_word_pairs$item2!="text19",] 
#Se ha eliminado el text19 del conjunto porque estaba rrepetido y 
#daba problemas a la hora de visualizar la red

OpenGraphDev(10, 9, platform)
set.seed(1234)
desc_word_pairs %>%
  filter(correlation > .22) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "circle") +
  geom_edge_link(aes(edge_alpha = correlation, color = "black"), show.legend = FALSE) +
  geom_node_point(color = "#C44237", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()



