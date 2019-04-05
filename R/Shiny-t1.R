# network d3 

library(tidyverse)
library(shiny)
library(networkD3)
library(ggplot2)
library(plotly)

links_1
nodes_1

plot1 <- nodes_1 %>%
  select(modularity_class) %>%
  group_by(modularity_class) %>%
  count(modularity_class, sort = TRUE) %>%
  ggplot(aes(modularity_class, n)) + geom_col() + 
  ggtitle("Jumlah akun per modularity class")

ggplotly(plo1)

  
#### Server ####
server <- function(input, output) {
  
  output$simple <- renderSimpleNetwork({
    src <- c("A", "A", "A", "A", "B", "B", "C", "C", "D")
    target <- c("B", "C", "D", "J", "E", "F", "G", "H", "I")
    networkData <- data.frame(src, target)
    simpleNetwork(networkData, opacity = input$opacity)
  })
  
  output$force <- renderForceNetwork({
    forceNetwork(Links = links_1, Nodes = nodes_1, 
                 Source = "Source",
                 Target = "Target", 
                 Value = "Weight",
                 NodeID = "Label",
                 Nodesize = "betweenesscentrality",
                 arrows = TRUE, legend = TRUE,
                 Group = "modularity_class", 
                 opacity = input$opacity,
                 fontSize = 15, 
                 zoom = TRUE)
  })
  
}

#### UI ####

ui <- shinyUI(fluidPage(
  
  titlePanel("Network Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("opacity", "Opacity network value", 0.6, min = 0.1,
                  max = 1, step = .1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Nodes Atribute", simpleNetworkOutput("simple")),
        tabPanel("Network", forceNetworkOutput("force"))
      )
    )
  )
))

#### Run ####
shinyApp(ui = ui, server = server)
