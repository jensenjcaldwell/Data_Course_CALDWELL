library(shiny)
library(gridlayout)
library(DT)
library(plotly)

ui <- grid_page(
  layout = c(
    ".       area3  header",
    "sidebar plotly plotly",
    "sidebar plotly plotly",
    "sidebar plotly plotly"
  ),
  row_sizes = c(
    "420px",
    "1fr",
    "1fr",
    "1fr"
  ),
  col_sizes = c(
    "240px",
    "0.28fr",
    "1.72fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "sidebar",
    title = "Settings",
    item_gap = "12px",
    checkboxInput(
      inputId = "gender",
      label = "Include Gender",
      value = FALSE,
      width = "100%"
    ),
    selectInput(
      inputId = "x_axis",
      label = "X axis variable",
      choices = list(
        Age = "age",
        Urbanization = "x2013 urbanization",
        Race = "race",
        `Place Of Death` = "myValue"
      )
    )
  ),
  grid_card_text(
    area = "header",
    content = "Suicide Statistics",
    alignment = "center",
    is_title = FALSE
  ),
  grid_card(
    area = "plotly",
    title = "Interactive Plot",
    tabsetPanel(
      tabPanel(
        title = "Bar",
        plotlyOutput(
          outputId = "distPlot",
          width = "100%",
          height = "100%"
        )
      ),
      tabPanel(
        title = "Trend line",
        plotlyOutput(
          outputId = "Frequency",
          width = "100%",
          height = "400px"
        )
      )
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    plot_ly(x = ~ faithful[, 2], type = "histogram")
  })

  output$bluePlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = "steelblue", border = "white")
  })


  output$myTable <- renderDT({
    head(faithful, input$numRows)
  })
}

shinyApp(ui, server)
