## library
library("tm")
library("ngram")

# TASK2

## WD information
rm(list=ls())
scriptwd = "F:/Data Science/Curso/trabajoFinal/scripts/finalassignment/"
datawd ="F:/Data Science/Curso/trabajoFinal/Datos/final/en_US/"
twitterfile = "en_US.twitter.txt"
blogfile ="en_US.blogs.txt"
newsfile = "en_US.news.txt"

## loading text
linestoread <- 10000
con <- file(paste(datawd,twitterfile, sep = ""), "r")
linestoprocess <- readLines(con,linestoread)
close(con)

ptd <- PlainTextDocument(linestoprocess)




## 1.Some words are more frequent than others - what are the distributions of word frequencies?

wordfrecuency <-  termFreq(doc=ptd,
                           control = list(removeNumbers = TRUE,
                                          tolower = TRUE,
                                          removePunctuation = TRUE)
                          )

wordfrecuency <- wordfrecuency[order(wordfrecuency, decreasing = TRUE)]

##2. What are the frequencies of 2-grams and 3-grams in the dataset?
bigramtokenizer <-  function(x){
              unlist(lapply(ngrams(words(x), 2), 
                            paste, 
                            collapse = " "), 
                     use.names = FALSE)
}

trigramtokenizer <-  function(x){
  unlist(lapply(ngrams(words(x), 3), 
                paste, 
                collapse = " "), 
         use.names = FALSE)
}

twowordfrecuency <-  termFreq(doc=ptd,
                              control = list(removeNumbers = TRUE,
                                             tolower = TRUE,
                                             tokenize = bigramtokenizer,
                                             removePunctuation = TRUE)
                      )
twowordfrecuency <- as.data.frame(twowordfrecuency[order(twowordfrecuency,decreasing = TRUE)],
                                  stringsAsFactors = FALSE)

threewordfrecuency <-  termFreq(doc=ptd,
                                control = list(removeNumbers = TRUE,
                                             tolower = TRUE,
                                             tokenize = trigramtokenizer,
                                             removePunctuation = TRUE)
)
threewordfrecuency <- as.data.frame(threewordfrecuency[order(threewordfrecuency,decreasing = TRUE)],
                                    stringsAsFactors = FALSE)

##3. How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?
totalsinglewords <- length(wordfrecuency)
totaldocwords <- sum(wordfrecuency)
threshold.50 = 0.5
threshold.90 = 0.9
countedwords = 0
iteratorindex = 0

index <- NULL
while (countedwords <= threshold.50 * totaldocwords){
  iteratorindex <- iteratorindex + 1
  countedwords  <- countedwords + wordfrecuency[iteratorindex]
}
index$percent50 <-  iteratorindex

while (countedwords <= threshold.90 * totaldocwords){
  iteratorindex <- iteratorindex + 1
  countedwords  <- countedwords + wordfrecuency[iteratorindex]
}
index$percent90 <-  iteratorindex

##4. How do you evaluate how many of the words come from foreign languages?
##5. Can you think of a way to increase the coverage --
##identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases?


# TASK3

## Build basic n-gram model - using the exploratory analysis you performed, build a basic n-gram model for predicting the next word based on the previous 1, 2, or 3 words.
##Build a model to handle unseen n-grams - in some cases people will want to type a combination of words that does not appear in the corpora. Build a model to handle cases where a particular n-gram isn't observed.

## Questions to consider


##1. How can you efficiently store an n-gram model (think Markov Chains)?

'se puede guardar en una cadena de markov: de todos los 2-gramas, los agrupo por
la palabra que empiecen (estado inicial), y ahi veo como estado de transicion la segunda palabra.
para saber cual es el 2 estado sugerido, voy a considerar la frecuencia en el 
que van apareciendo los n-gramas"
'
##2. How can you use the knowledge about word frequencies to make your model smaller and more efficient?

'para definir el estado secundario, considero las segundas palabras que forman
el 2-grama que estan dentro de las 50 o 90% de palabras'

##3. How many parameters do you need (i.e. how big is n in your n-gram model)?

'2'

##4. Can you think of simple ways to "smooth" the probabilities (
## think about giving all n-grams a non-zero probability even if they aren't observed in the data) ?

'si un n-grama no es visto, considero las palabras mas frecuentes'

##5. How do you evaluate whether your model is any good?



##6. How can you use backoff models to estimate the probability of unobserved n-grams?




## Model creation
### me quedo con las palabras que forman hasta un 90% del total visto

wordfrecuency <- as.data.frame(wordfrecuency[1:index$percent90],
                               stringsAsFactors = FALSE)

### para cada palabra, busco los 2-grama que empiezan por palabras que se encuentran en wordfrecuency

findtext <- function(word, textframe ){
  tmp = NULL
  if (!is.null(word)){
      index = grep(paste('^',word,"+ +", sep=""),textframe)
      if(length(index)>0) {tmp <- textframe[index]}
  }
  return(tmp)
}

removeword <- function(word, textframe){
  tmp <- NULL
  if (!is.null(textframe)) tmp <- gsub(pattern = paste("^",word,"+ +",sep=""),
                                       x       = textframe,
                                       replacement = "")
  return(tmp)
  
}


a <- wordfrecuency[1:50,]
b <- twowordfrecuency[1:50,]
d <- threewordfrecuency[1:50,]

model <- list()
model[[1]] <- wordfrecuency[,1]

## recorro todas las palabras
for ( i in 1:index$percent90){
    modelitem <- NULL
    word = a[i,1]
    ## busco en el bi-grama
    reduced2gram <- findtext(word,b[,1])
    secondlevelwords <-  removeword(word,reduced2gram)   
    
    ## busco el bi-grama en el tri-grama
    for (j in 1:length(reduced2gram)){
        reduced3gram <- findtext(reduced2gram[j],d[,1])
        thirdlevelwords <- removeword(reduced2gram[j],reduced3gram)
    }  
} 
  
  
  
  
  








