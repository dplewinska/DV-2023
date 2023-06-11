library(shiny)
library(plotly)

ui <- fluidPage(
  
  # Add custom CSS styles
  tags$head(
    tags$style(HTML("
    pre {
      background-color: #ffffff;
    }
    .title {
      text-align: center;
      font-size: 24px;
      font-weight: bold;
      margin-bottom: 20px;
    }
    .logo {
      display: block;
      margin: 20px auto;
      width: 80%;
      max-width: 400px;
      height: auto;
    }
    .pokemon-info {
      padding: 20px;
      border: 1px solid #ddd;
      border-radius: 5px;
      margin-bottom: 20px;
      background-color: #fbe2ef;
    }
    .battle-buttons {
      text-align: center;
      margin: 20px 0;
    }
    .pokemon-image {
      display: block;
      margin: 0 auto;
      height: auto;
    }
    .winner-text {
      font-weight: bold;
      text-align: center;
      margin-top: 20px;
      font-size: 50px;
    }
    .generation-slider {
      margin: 20px 0;
    }
    .plot-container {
      padding: 10px;
      border: 1px solid #ddd;
      border-radius: 5px;
      background-color: #fbe2ef;
      margin-bottom: 20px;
    }
  "))
  )
  ,
  
  titlePanel("Pokemon Dashboard"),
  
  tabsetPanel(
    
    tabPanel("About",
             fluidRow(
               column(width = 8,
                      div(class = "pokemon-info", verbatimTextOutput("description"))
               ),
               column(width = 4,
                      imageOutput("logo", width = "100%")
               )
             )
    ),
    
    tabPanel("Pokemon Statistics",
             fluidRow(
               column(width = 5, 
                      verticalLayout(
                        div(class = "pokemon-info", selectInput("name", "Choose Pokemon", data.frame(read.csv("pokemon.csv", sep = ","))$name)),
                        div(class = "pokemon-image", imageOutput("pokemon_image", width = "100%")))
               ),
               column(width = 7, div(class = "plot-container", plotlyOutput("statsPlot", width = "100%", height = "400px"))),
             )
    ),
    
    tabPanel("Battle Royale",
             fluidRow(
               column(width = 12, div(class = "battle-buttons", actionButton("battle", "Battle")))
             ),
             
             fluidRow(
               column(width = 6,
                      div(class = "pokemon-info",
                          actionButton("randomize1", "Randomize"),
                          selectInput("name1", "Choose Pokemon", data.frame(read.csv("pokemon.csv", sep = ","))$name),
                          div(class = "pokemon-image", imageOutput("pokemon_image1", width = "100%")),
                          div(class = "winner-text", textOutput("is_winner1"))
                      )
               ),
               column(width = 6,
                      div(class = "pokemon-info",
                          actionButton("randomize2", "Randomize"),
                          selectInput("name2", "Choose Pokemon", data.frame(read.csv("pokemon.csv", sep = ","))$name),
                          div(class = "pokemon-image", imageOutput("pokemon_image2", width = "100%")),
                          div(class = "winner-text", textOutput("is_winner2"))
                      )
               )
             )
    ),
    
    tabPanel("Type Distribution",
             fluidRow(
               column(width = 4,
                      div(class = "generation-slider",
                          sliderInput("x", "Generation", min = 1, max = 7, value = 1)
                      )
               ),
               
               column(width = 8,
                      div(class = "plot-container", plotlyOutput("distPlot", width = "100%", height = "400px"))
               )
             )
    ),
    
    tabPanel("Type Effectiveness Chart",
             fluidRow(
               column(width = 12,
                      div(class = "plot-container", plotlyOutput("typeef", width = "100%", height = "800px"))
               )
             )
    )
  )
)

