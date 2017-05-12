## WD information
rm(list=ls())
scriptwd = "F:/Data Science/Curso/trabajoFinal/scripts/finalassignment/"
datawd ="F:/Data Science/Curso/trabajoFinal/Datos/final/en_US/"
twitterfile = "en_US.twitter.txt"
blogfile ="en_US.blogs.txt"
newsfile = "en_US.news.txt"


'Questions
---------
1) The en_US.blogs.txt  file is how many megabytes?
2) The en_US.twitter.txt has how many lines of text?
3) What is the length of the longest line seen in any of the three en_US data sets?
4) In the en_US twitter data set, if you divide the number of lines where the word "love" (all lowercase) occurs by the number of lines the word "hate" 
(all lowercase) occurs, about what do you get?

5) The one tweet in the en_US twitter data set that matches the word "biostats" says what? 
6) How many tweets have the exact characters "A computer once beat me at chess, but it was no match for me at kickboxing". (I.e. the line matches those characters exactly.)
' 

## matrix where the information shall be added
lenghtmatrix <- matrix(data = rep(0,9),
                       ncol=3, 
                       nrow=3,
                       dimnames = list( c("en_US twitter",
                                          "en_US.blogs",
                                          "en_US.news"),
                                        c("textlines",
                                          "charlongestline",
                                          "matchphrase_6")
                                      )
                       )



twitter_word_hate_4 = "hate"
twitter_word_love_4 = "love" 
twitter_word_5 = "biostats"
phrase_6 = "A computer once beat me at chess, but it was no match for me at kickboxing"


### variable instantiation
linestoread <- 100000 
iterate <- NULL 
interatorindex <- NULL


## get connection to file and reset counters
getconnectiontofile <- function (filepath){
  iterate <<- TRUE 
  interatorindex <<- 0
  return(file(paste(datawd,filepath, sep = ""), "r"))
}



### TWITTER FILE ANALYSIS 
twitterlovenumber <- 0
twitterhatenumber <- 0
phrasewithbiostat <- NULL


con <- getconnectiontofile(twitterfile)

timestamp()
while (iterate){
    linestoprocess <- readLines(con,linestoread)
    lengthofbulklines <- length(linestoprocess)
    ## analysis q2
    lenghtmatrix[1,1] <- lenghtmatrix[1,1] + lengthofbulklines
    ## analysis q3
    lenghtmatrix[1,2] <- max(lenghtmatrix[1,2],
                             max(nchar(linestoprocess)))
    ## analysis q4
    twitterlovenumber <- twitterlovenumber +
                          length(grep(twitter_word_love_4,
                                      linestoprocess, 
                                      ignore.case = FALSE))
    
    twitterhatenumber <- twitterhatenumber +
                          length(grep(twitter_word_hate_4,
                                      linestoprocess, 
                                      ignore.case = FALSE))
    
    ## analysis q5
    wordindex <- grep(twitter_word_5,
                      linestoprocess, 
                      ignore.case = TRUE)
    if (length(wordindex)>0){phrasewithbiostat <- linestoprocess[wordindex]}
                               
    ## analysis q6
    lenghtmatrix[1,3] <- lenghtmatrix[1,3] +
                            length(grep(phrase_6,
                                        linestoprocess, 
                                        ignore.case = FALSE))
    
    ## check: closing loop condition
        ##interatorindex <- interatorindex + 1
        ##iterate <- (interatorindex <=5) ##
    iterate <- (lengthofbulklines == linestoread)
}
close(con) 
timestamp()

## write  values whether crushes
write.table( x = lenghtmatrix,
             file = paste(datawd,"lenghtmatrix.dat"))
write.table( x= twitterlovenumber/twitterhatenumber,
             file = paste(datawd,"love_hate.dat"))

write.table( x= phrasewithbiostat,
             file = paste(datawd,"biostatprhase.dat"))


### blog file analysis 
con <- getconnectiontofile(blogfile)
timestamp()
while (iterate){
  linestoprocess <- readLines(con,linestoread)
  lengthofbulklines <- length(linestoprocess)
  ## analysis q3
  lenghtmatrix[2,2] <- max(lenghtmatrix[2,2],
                           max(nchar(linestoprocess)))
  
  ## check: closing loop condition
  iterate <- (lengthofbulklines == linestoread)
}
close(con) 
timestamp()


### news file analysis 
con <- getconnectiontofile(newsfile)

timestamp()
while (iterate){
  linestoprocess <- readLines(con,linestoread)
  lengthofbulklines <- length(linestoprocess)
  ## analysis q3
  lenghtmatrix[3,2] <- max(lenghtmatrix[3,2],
                           max(nchar(linestoprocess)))
 
  ## check: closing loop condition
  iterate <- (lengthofbulklines == linestoread)
}
close(con) 
timestamp()

