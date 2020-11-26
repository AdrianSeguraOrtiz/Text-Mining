# Cargamos las librerias
library(pdftools)
library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(quanteda)
library(dplyr)
library(qdap)

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
