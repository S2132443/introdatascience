library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(stringr)

IMDB <- read.csv("./Data/imdb_top_1000.csv")
poster <- as.data.frame((str_c('<img src="',IMDB$Poster_Link, '"height="150"></img>')), stringsAsFactors=FALSE)
IMDB <- cbind(IMDB, poster)
names(IMDB)[17] <- 'IMG_Poster'
IMDB <- arrange(IMDB, desc(IMDB_Rating), desc(Meta_score))

movies <- IMDB %>% select(IMG_Poster,Series_Title, IMDB_Rating, Meta_score, Genre, Overview, Director, Star1, Star2, Star3, Star4, Runtime, Released_Year)

genres <- as.data.frame(trimws(unlist(strsplit(sort(IMDB$Genre), split = ","))))
genres <- genres %>% unique
names(genres)[1] <- 'Genres'

actors <- as.data.frame(trimws(unlist(strsplit(str_c(sort(IMDB$Star1),",", sort(IMDB$Star2),",", sort(IMDB$Star3),",",sort(IMDB$Star4)), split = ","))))
actors <- actors %>% unique
names(actors)[1] <- 'Actors'

directors <- as.data.frame(sort(IMDB$Director), stringsAsFactors=FALSE)
directors <- directors %>% unique
names(directors)[1] <- 'Directors'


shinyServer(function(input, output) {
  
  
  moviesr<-eventReactive(input$go,{if (all(c("Genre", "Actor","Director") %in% input$check)){
    return(filter(movies,grepl(input$genres,Genre),grepl(input$actors, Star1)|grepl(input$actors, Star2)|grepl(input$actors, Star3)|grepl(input$actors, Star4),grepl(input$directors, Director)))
  }
    else if(all(c("Genre", "Actor") %in% input$check)){
      return (filter(movies,grepl(input$genres,Genre),grepl(input$actors, Star1)|grepl(input$actors, Star2)|grepl(input$actors, Star3)|grepl(input$actors, Star4)))
    }
    else if (all(c("Genre", "Director") %in% input$check)){
      return(filter(movies,grepl(input$genres,Genre),grepl(input$directors, Director)))
    }
    else if (all(c("Actor","Director") %in% input$check)){
      return(filter(movies,grepl(input$actors, Star1)|grepl(input$actors, Star2)|grepl(input$actors, Star3)|grepl(input$actors, Star4),grepl(input$directors, Director)))
      
    }
    else if (all(c("Genre") %in% input$check)){
      return(filter(movies,grepl(input$genres,Genre)))
    }
    else if (all(c("Actor") %in% input$check)){
      return(filter(movies,grepl(input$actors, Star1)|grepl(input$actors, Star2)|grepl(input$actors, Star3)|grepl(input$actors, Star4)))
    }
    else if (all(c("Director") %in% input$check)){
      return(filter(movies,grepl(input$directors, Director)))
    }
  })
  
  
  output$table<-DT::renderDataTable({
    DT::datatable(moviesr(), escape=FALSE, options = list(searching=F,ordering=F)) %>% 
      formatStyle( c(0:13), backgroundColor = "#f5de50")
  })
  
  
  
})
