library(plotly)
library(tidyverse)
library(shiny)
library(gridlayout)
library(ggplot2)

ui <- grid_page(
  layout = c(
    "header  header",
    "sidebar area3 ",
    "sidebar area3 "
  ),
  row_sizes = c(
    "70px",
    "1.42fr",
    "0.58fr"
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
      inputId = "comp",
      label = "Compare",
      choices = list(
        age = "ten_year_age_groups",
        state = "state",
        race = "race"
      )
    ),
    checkboxInput(
      inputId = "se",
      label = "Standard error shading?",
      value = FALSE,
      width = "77%"
    ),
    selectInput(
      inputId = "fittype",
      label = "Fit Line Type",
      choices = list(
        Loess = "loess",
        `Third level polynomial` = "lm"
      )
    ),
    sliderInput(
      inputId = "startyear",
      label = "Start Year",
      min = 1999L,
      max = 2020L,
      value = 1999L,
      step = 1L,
      width = "100%"
    ),
    sliderInput(
      inputId = "endyear",
      label = "End Year",
      min = 1999L,
      max = 2020L,
      value = 2020L,
      step = 1L,
      width = "100%"
    )
  ),
  grid_card_text(
    area = "header",
    content = "Suicides Over Time",
    alignment = "center",
    is_title = TRUE
  ),
  grid_card(
    area = "area3",
    plotlyOutput(
      outputId = "plotly",
      width = "100%",
      height = "400px"
    ),
    textOutput(outputId = "citation")
  )
)

server <- function(input, output) {
  
  output$citation <-renderText("Data is from wonder.cdc.gov")
  
  output$plotly <- renderPlotly({
    
    
    master <- read_delim("./final.txt") %>% janitor::clean_names() %>% mutate(statel = str_squish(str_to_lower(state)))
    
    df <- master %>% mutate(ten_year_age_groups = str_replace_all(ten_year_age_groups,"5-14","05-14")) %>% filter(input$startyear <= year_code) %>% filter(input$endyear >= year_code)
    
       if(input$bystate == TRUE){
    df <- filter(df,statel == str_squish(str_to_lower(input$stateid)))
  }
    
if(input$gender == TRUE){
  
  if(input$comp == "race"){
    df <- df %>% group_by(race,year,gender) %>% mutate("suicides" = sum(deaths))
  }
    if(input$comp == "ten_year_age_groups"){
      df <- df %>% group_by(ten_year_age_groups,year,gender) %>% mutate("suicides" = sum(deaths))
    }
    if(input$comp == "state"){
      df <- df %>% group_by(state,year,gender) %>% mutate("suicides" = sum(deaths))
    }
}
  
  else {
 
    
     if(input$comp == "race"){
  df <- df %>% group_by(race,year) %>% mutate("suicides" = sum(deaths))
  }
    if(input$comp == "ten_year_age_groups"){
      df <- df %>% group_by(ten_year_age_groups,year) %>% mutate("suicides" = sum(deaths))
    }
    if(input$comp == "state"){
      df <- df %>% group_by(state,year) %>% mutate("suicides" = sum(deaths))
    }
 
  }
    
    
    
#  df <-  read_csv("../Data/dataset.csv")
  
  

    if(input$fittype == "lm"){form <- "y ~ poly(x, 3)"}
    if(input$fittype == "loess") {form <- NULL}
    
    
    
    
plot <-  ggplot(df,aes(x=year_code, y=suicides, color= df[[input$comp]]))+#if(input$comp == race){color = race},if(input$comp == state){group = state},if(input$comp == age){group = ten_year_age_groups}))+
    geom_point()+
    geom_smooth(se = input$se, method = input$fittype, formula = form)+
    labs(x="Year", y= "Suicides", color = str_to_sentence(str_replace_all(input$comp, "_", " ")))+
    theme_minimal()+
    if (input$gender == TRUE){
      facet_wrap(~gender)
    }
    
plotly::ggplotly(plot,dynamicTicks = FALSE)
 
  
  })

 
}

shinyApp(ui, server)
