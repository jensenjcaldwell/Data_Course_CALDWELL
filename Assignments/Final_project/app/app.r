library(tidyverse)
library(shiny)
library(gridlayout)
library(ggplot2)

ui <- grid_page(
  layout = c(
    "header  header   ",
    "sidebar linePlots",
    "sidebar linePlots"
  ),
  row_sizes = c(
    "70px",
    "1.03fr",
    "0.97fr"
  ),
  col_sizes = c(
    "250px",
    "1fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "sidebar",
    title = "Settings",
    item_gap = "12px",
    checkboxInput(
      inputId = "gender",
      label = "Separate By Gender?",
      value = FALSE,
      width = "100%"
    ),
    checkboxInput(
      inputId = "bystate",
      label = "Filter by state?",
      value = FALSE,
      width = "100%"
    ),
    textInput(
      inputId = "stateid",
      label = "State",
      value = ""
    ),
    selectInput(
      inputId = "xvar",
      label = "Compare",
      choices = list(
        `3` = "myValue",
        `Total deaths by year` = "year",
        `Deaths by race` = "b"
      )
    ),
    checkboxInput(
      inputId = "se",
      label = "Standard error shading?",
      value = FALSE,
      width = "100%"
    )
  ),
  grid_card_text(
    area = "header",
    content = "Chick Weights",
    alignment = "center",
    is_title = FALSE
  ),
  grid_card_plot(area = "linePlots")
)

server <- function(input, output) {
  output$linePlots <- renderPlot({
    
    
  df <-  read_csv("../Data/dataset.csv")
  
  
  if(input$bystate == TRUE){
    df <- filter(df,state == str_to_sentence(input$stateid))
  }
      

    
    
  ggplot(df,aes(x=year, y=total_by_age  , group = state))+
    geom_point(color = "steelblue")+
    geom_smooth(se = input$se)+
    theme_minimal()+
    if (input$gender == TRUE){
      facet_wrap(~gender)
    }
    
  
  
  })

 
}

shinyApp(ui, server)
