# Load the packages
suppressWarnings(library(devtools))
suppressWarnings(library(stringr))
suppressWarnings(library(shiny))
suppressWarnings(library(shinyapps))
suppressWarnings(library(tm))

# Loading the n-gram data
pairs_sep <- readRDS("pairs_sep.rds")
triplets_sep <- readRDS("triplets_sep.rds")
quads_sep <- readRDS("quads_sep.rds")
fives_sep <- readRDS("fives_sep.rds")
sixes_sep <- readRDS("sixes_sep.rds")


shinyServer(
  function(input, output, session) {
    
    # cleaning up the input string using the same method used for cleaning up the original data
    
    clean_UG <- reactive({
      UG <- paste(input$input_text, collapse=" ")
      UG <- VectorSource(UG)
      UG <- Corpus(UG)
      UG <- tm_map(UG, content_transformer(tolower))
      UG <- tm_map(UG, removePunctuation)
      UG <- tm_map(UG, stripWhitespace)
      UG <- tm_map(UG, removeNumbers)
      UG <- as.character(UG[[1]])
      UG <- paste("^", UG, sep="")
    })
    
    # Extracting the length of the input string
    UG_len <- reactive({
      UG <- clean_UG()
      UG_len <- as.numeric(length(unlist(strsplit(UG, split=" "))))
    })
    
    
    # Search in the six n-gram if the input string is greater than or equal to 5 and then back off to the smaller 
    # n-grams until you find a match
    
    UG_Pred <- reactive({
      
      if (UG_len() >= 5){
        UG6 <- word(clean_UG(), -5:-1)
        UG6 <- paste(UG6, collapse=" ")
        UG6 <- as.character(UG6[[1]])
        UG6 <- paste("^", UG6, sep="")
        UG6 <- grep(UG6, sixes_sep[,2])
        UG6 <- sixes_sep[UG6,]
        UG6 <- as.character(UG6[1:5,3])
        
        UG5 <- word(clean_UG(), -4:-1)
        UG5 <- paste(UG5, collapse=" ")
        UG5 <- as.character(UG5[[1]])
        UG5 <- paste("^", UG5, sep="")
        UG5 <- grep(UG5, fives_sep[,2])
        UG5 <- fives_sep[UG5,]
        UG5 <- as.character(UG5[1:5,3])
        
        UG4 <- word(clean_UG(), -3:-1)
        UG4 <- paste(UG4, collapse=" ")
        UG4 <- as.character(UG4[[1]])
        UG4 <- paste("^", UG4, sep="")
        UG4 <- grep(UG4, quads_sep[,2])
        UG4 <- quads_sep[UG4,]
        UG4 <- as.character(UG4[1:5,3])
        
        UG3 <- word(clean_UG(), -2:-1)
        UG3 <- paste(UG3, collapse=" ")
        UG3 <- as.character(UG3[[1]])
        UG3 <- paste("^", UG3, sep="")
        UG3 <- grep(UG3, triplets_sep[,2])
        UG3 <- triplets_sep[UG3,]
        UG3 <- as.character(UG3[1:5,3])
        
        UG2 <- word(clean_UG(), -1:-1)
        UG2 <- paste(UG2, collapse=" ")
        UG2 <- as.character(UG2[[1]])
        UG2 <- paste("^", UG2, sep="")
        UG2 <- grep(UG2, pairs_sep[,2])
        UG2 <- pairs_sep[UG2,]
        UG2 <- as.character(UG2[1:5,3])
        
        UG <- c(UG6, UG5, UG4, UG3, UG2, "the", "and", "that", "for", "with")
        UG <- UG[!is.na(UG)]
        UG <- UG[1:5]
      }
      
      else if (UG_len() == 4){
        UG5 <- clean_UG()
        UG5 <- grep(UG5, fives_sep[,2])
        UG5 <- fives_sep[UG5,]
        UG5 <- as.character(UG5[1:5,3])
        
        UG4 <- word(clean_UG(), -3:-1)
        UG4 <- paste(UG4, collapse=" ")
        UG4 <- as.character(UG4[[1]])
        UG4 <- paste("^", UG4, sep="")
        UG4 <- grep(UG4, quads_sep[,2])
        UG4 <- quads_sep[UG4,]
        UG4 <- as.character(UG4[1:5,3])
        
        UG3 <- word(clean_UG(), -2:-1)
        UG3 <- paste(UG3, collapse=" ")
        UG3 <- as.character(UG3[[1]])
        UG3 <- paste("^", UG3, sep="")
        UG3 <- grep(UG3, triplets_sep[,2])
        UG3 <- triplets_sep[UG3,]
        UG3 <- as.character(UG3[1:5,3])
        
        UG2 <- word(clean_UG(), -1:-1)
        UG2 <- paste(UG2, collapse=" ")
        UG2 <- as.character(UG2[[1]])
        UG2 <- paste("^", UG2, sep="")
        UG2 <- grep(UG2, pairs_sep[,2])
        UG2 <- pairs_sep[UG2,]
        UG2 <- as.character(UG2[1:5,3])
        
        UG <- c(UG5, UG4, UG3, UG2, "the", "and", "that", "for", "with")
        UG <- UG[!is.na(UG)]
        UG <- UG[1:5]
      }
      
      else if (UG_len() == 3){
        UG4 <- clean_UG()
        UG4 <- grep(UG4, quads_sep[,2])
        UG4 <- quads_sep[UG4,]
        UG4 <- as.character(UG4[1:5,3])
        
        UG3 <- word(clean_UG(), -2:-1)
        UG3 <- paste(UG3, collapse=" ")
        UG3 <- as.character(UG3[[1]])
        UG3 <- paste("^", UG3, sep="")
        UG3 <- grep(UG3, triplets_sep[,2])
        UG3 <- triplets_sep[UG3,]
        UG3 <- as.character(UG3[1:5,3])
        
        UG2 <- word(clean_UG(), -1:-1)
        UG2 <- paste(UG2, collapse=" ")
        UG2 <- as.character(UG2[[1]])
        UG2 <- paste("^", UG2, sep="")
        UG2 <- grep(UG2, pairs_sep[,2])
        UG2 <- pairs_sep[UG2,]
        UG2 <- as.character(UG2[1:5,3])
        
        UG <- c(UG4, UG3, UG2, "the", "and", "that", "for", "with")
        UG <- UG[!is.na(UG)]
        UG <- UG[1:5]
      } 
      
      else if (UG_len() == 2){
        UG3 <- clean_UG()
        UG3 <- grep(UG3, triplets_sep[,2])
        UG3 <- triplets_sep[UG3,]
        UG3 <- as.character(UG3[1:5,3])
        
        UG2 <- word(clean_UG(), -1:-1)
        UG2 <- paste(UG2, collapse=" ")
        UG2 <- as.character(UG2[[1]])
        UG2 <- paste("^", UG2, sep="")
        UG2 <- grep(UG2, pairs_sep[,2])
        UG2 <- pairs_sep[UG2,]
        UG2 <- as.character(UG2[1:5,3])
        
        UG <- c(UG3, UG2, "the", "and", "that", "for", "with")
        UG <- UG[!is.na(UG)]
        UG <- UG[1:5]
      } 
      
      else if (UG_len() == 1){
        UG2 <- clean_UG()
        UG2 <- grep(UG2, pairs_sep[,2])
        UG2 <- pairs_sep[UG2,]
        UG2 <- as.character(UG2[1:5,3])
        
        UG <- c(UG2, "the", "and", "that", "for", "with")
        UG <- UG[!is.na(UG)]
        UG <- UG[1:5]
      } 
      
    })
    
    
    
    
    
    
    # Output the to the shiny app 
    
    output$pred <- renderText({
      paste(UG_Pred())
    })
    
    output$len <- renderText({
      paste("Your string length was", UG_len())
    })
    
    output$UG <- renderText({
      paste("Your input was", clean_UG())
    })
    
    output$text_UG <- reactive({input$input_text})
    output$def_sep <- renderPrint(head(def_sep))
    
  }
)

