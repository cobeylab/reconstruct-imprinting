library(shiny)
library(imprinting)

ui <- fluidPage(
  headerPanel("Imprinting Probabilities"),
  tabsetPanel(
    tabPanel(
      title = "Plots",
      mainPanel(width = 12,align = "center",
                tags$h3("Single Country-Year"),
                div(style = "float:left;width:36%;",plotOutput("country_year_bar_plot"))
      )
    ),
    tabPanel(
      title = "Data",
      mainPanel(width = 12,align = "center",
                tags$h3("TBD")
      )
    )))

server <- function(input, output, session) {
  output$country_year_bar_plot = renderPlot({plot_one_country_year(imprinting_df = get_imprinting_probabilities(2007, 'Brazil'))})
}

shinyApp(ui, server)
