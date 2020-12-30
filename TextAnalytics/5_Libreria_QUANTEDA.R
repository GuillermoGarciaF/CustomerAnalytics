#Text Mining con la librería Quanteda

#Documentación y ejemplos: 
# https://cran.r-project.org/web/packages/quanteda/quanteda.pdf.pdf
# https://cran.r-project.org/web/packages/quanteda/vignettes/quickstart.html

#Instalamos la librería
#install.packages("quanteda")
#install.packages("readr") #función read_csv
#install.packages("readtext") #función readtext

#Llamamos la librería para poder acceder a sus funciones
library(quanteda)
library(readtext)
library(dplyr)

#Cargamos el fichero de reseñas de Trip Advisor
# ? readtext
reviews_tripadvisor <- readtext("datasets/Tripadvisor_Reviews.csv",
                                text_field= "Review", docvarnames= "Rating")

#Inspeccionamos la estructura y el contenido
dim(reviews_tripadvisor)
str(reviews_tripadvisor)
head(reviews_tripadvisor)

#Estudiamos los estadísticos principales
summary(reviews_tripadvisor)


# Creamos un corpus de texto utilizando el vector de caracteres de Reviews
# Un corpus es un conjunto de documentos originales, almacenado junto con información a nivel del corpus
# y de cada documento
reviews_tripadvisor_corpus <- corpus(reviews_tripadvisor)


# La función summary nos permite resumir las características principales del corpus, como el número de
# caracteres distintos en cada documento (Types), el número de caracteres total en cada documento (Tokens), etc
summary(reviews_tripadvisor_corpus, n=10)


# Podemos añadir variables al nivel de documento en el corpus, lo que en quanteda se denominan docvars
# Son variables que describen atributos de cada documento
docvars(reviews_tripadvisor_corpus, "Rating") %>% head(10)

# Utilizar esta información para seleccionar un subconjuto
summary(corpus_subset(reviews_tripadvisor_corpus, Rating == 5), n=10)

# y almacenar esta información en un dataframe y analizarlo
reviews_tripadvisor_corpus_summary <- summary(reviews_tripadvisor_corpus)

reviews_tripadvisor_corpus_summary %>%
  count(Rating) %>%
  arrange(desc(Rating))

# También podemos explorar el corpus, utilizando la función kwic() o keywords-in-context
# que realiza una búsqueda y muestra el contexto en el que ocurre
# (documento, índice numérico de aparición de la palabra, contexto de aparición el documento)
kwic(reviews_tripadvisor_corpus, pattern = "good") %>%
  head()

kwic(reviews_tripadvisor_corpus, pattern = "room*") %>%
  tail()

# para buscar patrones de más de una palabra utilizamos phrase()
kwic(reviews_tripadvisor_corpus, pattern = phrase("very good")) %>%
  head()


# Un corpus está diseñado para ser un contenedor de texto estático con respecto al procesamiento
# y análisis, una "librería" de documentos originales.
# Para procesar y analizar textos, los extraemos del corpus y los asignamos a nuevos objetos.
# Extraemos texto del corpus usando la función texts()
texts(reviews_tripadvisor_corpus)[1:3]

# Para realizar análisis estadísticos sobre el texto, podemos transformarlo utilizando
# - tokens(): tokeniza el texto, separándolo en unidades
# - dfm(): tokeniza el texto y lo organiza en una matriz de documentos por features (document-feature matrix)


#Tokenizamos, separando en unidades (por defecto palabras) utilizando la función tokens()
#Por defecto, realiza una transformación conservadora: no transforma en minúsculas,
# mantiene puntuación, URLs, tags de Social Media (#hashtags, @username) (ver ? tokens)
tokens(reviews_tripadvisor_corpus, remove_numbers = FALSE, remove_punct = TRUE) %>% head(5)

tokens(reviews_tripadvisor_corpus, remove_numbers = FALSE, remove_punct = TRUE) %>% head(5) %>% as.character()


#TIP: tokens_compound() permite concatenar expresiones formadas por más de una palabra, como "Nueva York",
# y mantenerlas como una única unidad.

#también podemos separar en frases
tokens(c("Quanteda es un paquete muy potente; facilita trabajar con texto. Es un paquete en R",
         "En el caso de que no puedas ir con ellos, ¿quieres ir con nosotros? Si. No."), what = "sentence")

#La función tokens_ngrams() nos permite separar en n-gramas (subsecuencia de n elementos de una secuencia dada)
tokens_ngrams(tokens(c("a b c d e", "c d e f g")), n = 2:3, concatenator= " ")

tokens_ngrams(tokens(reviews_tripadvisor_corpus), n = 2, concatenator= " ") %>% head(5)


# Tokenizar el texto es un paso intermedio, en muchas ocasiones queremos construir directamente un dfm
# Generamos un document-feature matrix usando la función dfm()
# Los documentos se organizan en filas, las features en columnas
#Por defecto convierte en minúsculas, pero no elimina puntuación.
reviews_tripadvisor_dfm <- dfm(reviews_tripadvisor_corpus, remove_punct=TRUE)
head(reviews_tripadvisor_dfm)

dfm(reviews_tripadvisor_corpus, stem=TRUE, remove_punct=TRUE) %>% head()

dfm(reviews_tripadvisor_corpus, stem=TRUE, remove_punct=TRUE,
    remove= stopwords("english")) %>% head()

stopwords("english")
stopwords("spanish")

length(stopwords("english"))
length(stopwords("spanish"))


#Inspeccionamos los 20 términos más frecuentes en el corpus
topfeatures(reviews_tripadvisor_dfm, 20)

#ordenamos el dfm por el número de features más frecuentes
head(dfm_sort(reviews_tripadvisor_dfm))
#ordenamos el dfm por el número de features más frecuentes (columnas) y frecuencia en cada documento (filas)
head(dfm_sort(reviews_tripadvisor_dfm, margin= "both"))

#Inspeccionamos los 20 términos menos frecuentes en el corpus
topfeatures(reviews_tripadvisor_dfm, 20, decreasing=FALSE)

#En los ejemplos anteriores, analizamos la frecuencia de cada término (feature) en el conjunto de textos (corpus)
#También podemos analizar el número de documentos que contiene (al menos una vez) cada término
topfeatures(reviews_tripadvisor_dfm, 20, decreasing=TRUE, scheme="docfreq")

#y los términos más frecuentes en cada documento
reviews_tripadvisor_dfm_subset <- reviews_tripadvisor_dfm[1:5, ]
topfeatures(reviews_tripadvisor_dfm_subset, n= 3, groups= docnames(reviews_tripadvisor_dfm_subset))


#Generamos un wordcloud
set.seed(100)
textplot_wordcloud(reviews_tripadvisor_dfm, min_count = 6, random_order = FALSE, #max_words = 50,
                   rotation = 0.25, 
                   color = RColorBrewer::brewer.pal(8, "Dark2"))


# La función dictionary() permite crear un objeto de tipo diccionario, que puede ser utilizado para
# crear un dfm de palabras definidas a priori.
dict <- dictionary(list(positive = c("good", "great", "nice", "wonderful", "excellent"),
                        negative = c("bad", "awful", "horrible", "terrible")))

reviews_tripadvisor_dict_dfm <- dfm(reviews_tripadvisor_corpus, remove_punct=TRUE,
                                    dictionary= dict)
head(reviews_tripadvisor_dict_dfm)
topfeatures(reviews_tripadvisor_dict_dfm)


#Quanteda incorpora algunos diccionarios, podemos encontrar otros en internet
dict_LSD2015 <- data_dictionary_LSD2015
View(dict_LSD2015)

reviews_tripadvisor_dictLSD2015_dfm <- dfm(reviews_tripadvisor_corpus, remove_punct=TRUE,
                                           dict=dict_LSD2015)
reviews_tripadvisor_dictLSD2015_dfm


# Quanteda contiene muchas otras funcionalidades, incluyendo análisis de similitud entre documentos o
# términos, calculo de distancias y clusteríng, clasificación de textos, modelado de tópicos, etc.
? dfm_tfidf()
? dfm_trim

dfm_tfidf(reviews_tripadvisor_dfm)

textstat_summary(reviews_tripadvisor_corpus[1:5]) #disponible a partir de la version 2.1.2 de quanteda
textstat_collocations(reviews_tripadvisor_corpus[1:5])

textplot_network(reviews_tripadvisor_dfm[1:3, 1:100], min_freq = 0.8)
#min_freq: a frequency count threshold or proportion for co-occurrence frequencies of features to be included


##### FIN. 
