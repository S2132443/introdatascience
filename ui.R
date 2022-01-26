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

shinyUI(fluidPage(
  
  
  
  tags$style('.container-fluid {
                             background-color:  #f5de50;
              }',
             'body {
                background-color:  #f5de50;
              }'
  ),
  
  
  # Application title
  
  navbarPage("The IMDB Movie Recommender",
             tabPanel("About",
                      fluidRow(
                        
                               h2("Welcome to The IMDB Movie Recommender"),
                               p("This app takes the users input and recommends movies based on what the user is looking for."),
                               h3("How to use:"),
                               h4("Step 1: Go to the app panel to access the app"),
                               h4("Step 2: Choose what genre, actor or director you prefer from the drop down tabs"),
                               h4("Step 3: Tick the categories you wish to use for your recommendation in the checkboxes"),
                               h4("Step 4: Hit the search button and the app will recommend movies based on your preferences sorted by the highest IMDB rating and Metascore")
                               
                        )),
             
             
             tabPanel( "App",
                       fluidRow(
                         column(4, h4("Enter or search your preferred genre:"),
                                wellPanel(
                                  selectizeInput(
                                    "genres",
                                    "Genre:",
                                    choices = genres,
                                    multiple = TRUE,
                                    options = list(maxItems = 1)
                                  ),
                                  tags$style(".well {background-color: #6d6d6d;}"),
                                  
                                )),
                         
                         column(4, h4("Enter or search your preferred actor/actress:"),
                                wellPanel(
                                  selectizeInput(
                                    "actors",
                                    "Actor:",
                                    choices = actors,
                                    multiple = TRUE,
                                    options = list(maxItems = 1)
                                  )
                                )),
                         
                         column(4, h4("Enter or search your preferred movie director:"),
                                wellPanel(
                                  selectizeInput(
                                    "directors",
                                    "Director:",
                                    choices = directors,
                                    multiple = TRUE,
                                    options = list(maxItems = 1)
                                  )
                                )),
                         column(1, checkboxGroupInput("check", "Choose category:", choices = c("Genre", "Actor", "Director"),
                         ),
                         actionButton("go","SEARCH", icon("search"),class = "btn-primary")
                         ),
                         verbatimTextOutput("text"),
                         DT::dataTableOutput("table"),
                         
                         
                       )
             ),
             
             
  )
)
)
