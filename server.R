server <- shinyServer <- function(input,output, session){
    wc_data <- reactive({
        input$submit
        isolate({
            withProgress({
                setProgress(message = "Generating Song Lyrics...")
                artistName <- input$artist
                songName <- input$song
                wc_text <- genius_lyrics(artist = artistName,song = songName )
                wc_text2 <- wc_text$lyric
                image <- input$image
                ext <- tools::file_ext(image$datapath)
                
                #wc_text <- wc_text2
                justLyrics <- wc_text2
                
                
                # if(!is.null(artistName)){
                #   wc_text <- "Enter Artist to get started"
                # }
                
                
                ######
                lyrics_tm <- tm::Corpus(tm::VectorSource(justLyrics))
                # Remove punctuation from lyrics
                lyrics_tm <- tm::tm_map(lyrics_tm, tm::removePunctuation, preserve_intra_word_contractions = FALSE)
                # Remove numbers from lyrics
                lyrics_tm <- tm::tm_map(lyrics_tm, tm::removeNumbers)
                
                
                # Parse only song lyrics from original dataframe
                songTexts <- justLyrics
                
                # Some songs/albumns will have an 'na' as the song switches. 
                songTexts <- na.omit(songTexts)
                
                #### Create Quote DF
                # Create quotes from any line longer than 3 and cleans lines
                songMatrix <- RTextTools::create_matrix(songTexts,ngramLength=3)
                for(i in 1:length(songMatrix$dimnames$Docs)){
                    songMatrix[["dimnames"]][["Docs"]][i] <- tm::stripWhitespace(songMatrix[["dimnames"]][["Docs"]][i])
                }
                for(i in 1:length(songMatrix$dimnames$Docs)){
                    songMatrix[["dimnames"]][["Docs"]][i] <- tm::removePunctuation(songMatrix[["dimnames"]][["Docs"]][i])
                }
                # Create freq of quotes
                quoteCount <- 0
                for(i in 1:length(songMatrix$dimnames$Docs)){
                    quoteCount[i] <- length(which(songMatrix$dimnames$Docs == songMatrix$dimnames$Docs[i]))
                }
                qoutesCountDF <- data.frame(word = songTexts, freq=quoteCount)
                
                #### Create Singe words DF
                tokenize_ngrams <- function(x, n=100) return(rownames(as.data.frame(unclass(textcnt(x,method="string",n=n)))))
                singleWordsMatrix <- tm::TermDocumentMatrix(lyrics_tm,
                                                            control=list(
                                                                tokenize=tokenize_ngrams,
                                                                wordLengths = c(3, Inf)))
                singleWords <- singleWordsMatrix[["dimnames"]][["Terms"]]
                singleWords <- tm::removeWords(singleWords, tm::stopwords("english"))
                singleWordsCount <- as.matrix(singleWordsMatrix)
                singleWordsCount <- rowSums(singleWordsCount)
                singleCountsDF <- data.frame(freq =singleWordsCount, word=singleWords)
                
                
                # combines the two dataframes into one
                allCounts <- rbind(qoutesCountDF, singleCountsDF)
                # remove dups
                allCounts <- allCounts[which(duplicated(allCounts) == FALSE),]
                print(allCounts)

                # Tidy data for illustation purposes
                loggedCounts <- allCounts
                loggedCounts$freq <- log(loggedCounts$freq)
                
                data <- loggedCounts
                wc_text <- data
                
            })
        })
    })
    create_wordcloud <- function(data, 
                                 num_words = 500, 
                                 background = "rgba(255, 0, 0, 0.01)",
                                 figPath = ext,#input$figPath,
                                 size = input$sizeSlider, 
                                 minSize = input$minSlider, 
                                 gridSize =input$gridSize,
                                 fontWeight = input$fontWeight,
                                 rotateRatio = input$rotateRatioSlider,
                                 minRotation = input$minRotationSlider,
                                 maxRotation = input$minRotationSlider,
                                 ellipticity = input$ellipticitySlider)#, #size=input$sizeSlider, 
        #minSize = input$minSlider, gridSize =input$gridSize,
        #fontWeight = fontWeight)
        #figPath = input$figPath)
        wordcloud_rep <- repeatable(wordcloud2)
    output$wcplot <- renderWordcloud2({
        wordcloud2(wc_data())
                  
    })
}
