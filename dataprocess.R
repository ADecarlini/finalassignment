## WD information
rm(list=ls())
scriptwd = "F:/Data Science/Curso/trabajoFinal/scripts/finalassignment/"
datawd ="F:/Data Science/Curso/trabajoFinal/Datos/final/en_US/"
outputdatawd ="F:/Data Science/Curso/trabajoFinal/Datos/Temp/"
twitterfile = "en_US.twitter.txt"
blogfile ="en_US.blogs.txt"
newsfile = "en_US.news.txt"
iterate <- TRUE
interatorindex <-0

## library
library("tm")
library("ngram")


# get connection to file and reset counters
getconnectiontofile <- function (file){
  iterate <<- TRUE 
  interatorindex <<- 0
  return(file(paste(datawd,file, sep = ""), "r"))
}

## write temporal data

writedata <- function(x,index="",file){
          write.csv(x = x,
                    file = paste(outputdatawd,file,index,".dat",sep=""))
}

readdata <- function(index,file,delete1col=TRUE){
  file <- paste(outputdatawd,file,index,".dat",sep="")
  tmp <- read.csv(file, stringsAsFactors = FALSE)
  if (delete1col) tmp <- tmp[,-1]
  return(tmp)
}

## 5- gram function definition
fivegramtokenizer <-  function(x){
          unlist(lapply(ngrams(words(x), 5),paste,collapse = " "), 
                 use.names = FALSE)
}



## 1: recorro ficheros -> genero 5gramas
########### por el tiempo que esa demorando, veo que pasa con 3-gram

## 5gram function definition
ngramtokenizer <-  function(x){
  unlist(lapply(ngrams(words(x), 5),paste,collapse = " "), 
         use.names = FALSE)
}
  
linestoread <- 10000
con <- getconnectiontofile(twitterfile)


while (iterate){
  interatorindex <- interatorindex + 1
  print(paste("iterator= ",interatorindex))
  linestoprocess <- readLines(con,linestoread)
  lengthofbulklines <- length(linestoprocess)
  ptd <- PlainTextDocument(linestoprocess)
  
  dictionary <-  termFreq(doc=ptd,
                          control = list(removeNumbers = TRUE,
                                         tolower = TRUE,
                                         removePunctuation = TRUE))
  
  dictionary <- as.data.frame(dictionary[order(dictionary,decreasing = TRUE)])
  
  ngram <- termFreq(doc=ptd,control = list(removeNumbers = TRUE,
                                           tolower = TRUE,
                                           tokenize = ngramtokenizer,
                                           removePunctuation = TRUE))
  
  ngram <- as.data.frame(ngram[order(ngram,decreasing = TRUE)],
                         stringsAsFactors = FALSE)  
  
  writedata(dictionary, interatorindex,"dictwitter")
  writedata(ngram, interatorindex,"ngramtwitter")
  iterate <- (lengthofbulklines == linestoread)
}

close(con)


## 2: Merging files --> confeccionar el diccionario

## twitter 

Maxfiles = 182

dictionary <- readdata(1,"dictwitter")


for (i in 2:Maxfiles){
    temp  <- readdata(i,"dictwitter")
    
    dictionary <- merge(x=dictionary,y=temp, by="Var1", all= TRUE)
    
    ## quito los valores NA, los sumo, y quito columnas parciales
    dictionary[which(is.na(dictionary[,2])),2] <- 0
    dictionary[which(is.na(dictionary[,3])),3] <- 0
    dictionary$Freq <- as.numeric(dictionary$Freq.x) + as.numeric(dictionary$Freq.y)
    dictionary <- dictionary[,-c(2,3)]
    print(i)
}

writedata(x=dictionary,file="dictionary_twitter.dat")

### 3: Clean dictionary

dictionary <- dictionary[order(dictionary[,2], decreasing=TRUE),]

final_dictionary<- dictionary[dictionary$Freq >2,]
wordstodeleteNgram <- dictionary[dictionary$Freq <=2,]

writedata(x=final_dictionary, file="final_dictionary_twitter.dat")
writedata(x=wordstodeleteNgram, file="wordstodeleteNgram_twitter.dat")

## 4: limpiar los Ngramas - quito ngramas con freq=1

for (i in 1:Maxfiles){
  print(i)
  tmp <- readdata(i,"fivegramtwitter")
  tmp <-tmp[tmp$Freq>1,]
  writedata(tmp,index = i,"finalfivegram")
}

## 5: merge de los ngramas

mergedngram <- readdata(1,"finalfivegram")


for (i in 2:Maxfiles){
  print(i)
  temp  <- readdata(i,"finalfivegram")
  
  mergedngram <- merge(x=mergedngram,y=temp, by="Var1", all= TRUE)
  
  ## quito los valores NA, los sumo, y quito columnas parciales
  mergedngram[which(is.na(mergedngram[,2])),2] <- 0
  mergedngram[which(is.na(mergedngram[,3])),3] <- 0
  mergedngram$Freq <- as.numeric(mergedngram$Freq.x) + as.numeric(mergedngram$Freq.y)
  mergedngram <- mergedngram[,-c(2,3)]
}

writedata(x=mergedngram,file="twitter_merged5gram.dat")





