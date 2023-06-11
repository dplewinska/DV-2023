library(shiny)
library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(plotly)
library(magick)
library(png)
addResourcePath('output', 'output')

shinyServer(function(input, output, session) {
  
  pokemon <- read.csv("pokemon.csv", sep = ",")
  chart <- read.csv("chart.csv", sep = ",")
  
  output$distPlot <- renderPlotly({
    x <- input$x
    df_generation <- subset(pokemon, generation == x)
    type_counts <- table(df_generation$type1)
    
    plot_ly(
      data = df_generation,
      x = names(type_counts),
      y = type_counts,
      type = "bar",
      color = I("hotpink"),
      colors = I("hotpink")
    ) %>%
      layout(
        title = sprintf("Number of Instances of Each Type in Generation %d", x),
        xaxis = list(title = "Type"),
        yaxis = list(title = "Count")
      )
  })
  
  output$statsPlot <- renderPlotly({
    pokemon_name <- input$name
    name_row <- subset(pokemon, name == pokemon_name)
    name_row <- subset(name_row, select = c(attack, defense, hp, sp_attack, sp_defense, speed))
    
    data <- data.frame(Category = colnames(name_row), Value = as.vector(t(name_row)))
    
    plot_ly(data, y = ~Category, x = ~Value, type = 'bar', orientation = 'h', color = I('hotpink')) %>%
      layout(
        title = "Statistics",
        xaxis = list(title = "Value"),
        yaxis = list(title = "Category")
      )
  })
  
  output$typeef <- renderPlotly({
    type_effectiveness <- as.matrix(chart[2:19])
    
    types <- c(
      "Normal", "Fire", "Water", "Electric", "Grass", "Ice", "Fighting",
      "Poison", "Ground", "Flying", "Psychic", "Bug", "Rock", "Ghost",
      "Dragon", "Dark", "Steel", "Fairy"
    )
    
    df <- melt(type_effectiveness)
    df$Attacker <- types[df$Var1]
    df$Defender <- types[df$Var2]
    colnames(df) <- c("Var1", "Var2", "Effectiveness", "Attacker", "Defender")
    
    colors <- brewer.pal(11, "PiYG")
    
    p <- ggplot(df, aes(x = Defender, y = Attacker, fill = Effectiveness, text = paste("Attacker: ", Attacker, "Defender: ", Defender))) +
      geom_tile(color = "white") +
      scale_fill_gradientn(colors = colors) +
      labs(title = "Pokemon Type Effectiveness Chart", x = "Defender", y = "Attacker") +
      theme_minimal() +
      theme(
        axis.text = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "right"
      )
    p <- ggplotly(p, tooltip = "text")
    p
  })
  
  output$pokemon_image <- renderImage({
    name <- tolower(input$name)
    if (name == "nidoran♂") {
      name = "nidoran-m"
    }
    if (name == "nidoran♀") {
      name = "nidoran-f"
    }
    file_name <- sprintf("output/%s.png", name)
    
    list(src = file_name,
         alt = input$name,
         width = "80%",
         height = "auto")
  }, deleteFile = FALSE)
  
  output$pokemon_image1 <- renderImage({
    name <- tolower(input$name1)
    if (name == "nidoran♂") {
      name = "nidoran-m"
    }
    if (name == "nidoran♀") {
      name = "nidoran-f"
    }
    file_name <- sprintf("output/%s.png", name)
    
    list(src = file_name,
         alt = input$name1,
         width = "60%",
         height = "auto")
  }, deleteFile = FALSE)
  
  output$pokemon_image2 <- renderImage({
    name <- tolower(input$name2)
    if (name == "nidoran♂") {
      name = "nidoran-m"
    }
    if (name == "nidoran♀") {
      name = "nidoran-f"
    }
    file_name <- sprintf("output/%s.png", name)
    
    list(src = file_name,
         alt = input$name2,
         width = "60%",
         height = "auto")
  }, deleteFile = FALSE)
  
  winner <- reactiveVal("")
  loser <- reactiveVal("")
  
  observeEvent(input$name1, {
    winner("")
    loser("")
  })
  
  observeEvent(input$name2, {
    winner("")
    loser("")
  })
  
  observeEvent(input$battle, {
    pokemon1 <- input$name1
    pokemon2 <- input$name2
    
    name_row1 <- subset(pokemon, name == pokemon1)
    stats1 <- sum(name_row1$attack, name_row1$defense, name_row1$hp, name_row1$sp_attack, name_row1$sp_defense)
    
    name_row2 <- subset(pokemon, name == pokemon2)
    stats2 <- sum(name_row2$attack, name_row2$defense, name_row2$hp, name_row2$sp_attack, name_row2$sp_defense)
    
    type11 <- name_row1$type1
    type12 <- name_row1$type2
    type21 <- name_row2$type1
    type22 <- name_row2$type2
    
    stats1 <- stats1 * name_row2[[sprintf("against_%s", type11)]]
    if (type12 != "") {
      stats1 <- stats1 * name_row2[[sprintf("against_%s", type12)]]
    }
    stats2 <- stats2 * name_row1[[sprintf("against_%s", type21)]]
    if (type22 != "") {
      stats2 <- stats2 * name_row1[[sprintf("against_%s", type22)]]
    }
    stats1 <- stats1 * runif(1)
    stats2 <- stats2 * runif(1)
    
    if (stats1 > stats2) {
      winner("WINNER")
      loser("LOSER")
    } else if (stats2 > stats1) {
      winner("LOSER")
      loser("WINNER")
    } else {
      winner("TIE")
      loser("TIE")
    }
  })
  
  output$is_winner1 <- renderText({
    winner()
  })
  
  output$is_winner2 <- renderText({
    loser()
  })
  
  observeEvent(input$randomize1, {
    random_pokemon <- sample(pokemon$name, 1)
    updateSelectInput(session, "name1", selected = random_pokemon)
  })
  
  observeEvent(input$randomize2, {
    random_pokemon <- sample(pokemon$name, 1)
    updateSelectInput(session, "name2", selected = random_pokemon)
  })
  
  output$logo <- renderImage({
    list(src = "PP_znak_konturowy_RGB.png",
         width = "100%",
         height = "auto")
  }, deleteFile = FALSE)
  
  output$description <- renderText({
    paste("The Pokemon Dashboard project is an interactive dashboard providing an engaging and informative interface", "for exploring Pokemon statistics, simulating battles, and understanding type interactions.", "The main dataset used was downloaded here: https://www.kaggle.com/datasets/rounakbanik/pokemon", "",
          "The Pokemon Statistics tab allows users to select a specific Pokemon from a dropdown menu.", "Upon selection, the tab displays an image of the chosen Pokemon along with its key statistical attributes", "- attack, defense, HP, special attack, special defense, and speed.", "",
          "The Battle Royale tab allows users to simulate a battle between two Pokemon.", "Users can manually select two Pokemon from dropdown menus or click the Randomize button", "to randomly select them. After initiating the battle, a simple algorithm determines the winner", "based on a combination of the Pokemon's statistics, type effectiveness and luck.", "",
          "The Type Distribution tab presents a slider input for selecting a specific Pokemon generation.", "Upon selection, a bar chart shows the count of each Pokemon type within the chosen generation.", "",
          "The Type Effectiveness Chart tab visualizes the type effectiveness matrix for different Pokemon types.", "The chart displays a grid of tiles representing the effectiveness of one type against another,", "with color gradients indicating the strength or weakness of the interaction.", "",
          "Dominika Plewińska",
          "Jędrzej Pacanowski", sep="\n")
  })
  
})
