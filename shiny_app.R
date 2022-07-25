library(shiny)
library(reconstructimprinting)

ui <- fluidPage(
  headerPanel("Imprinting Probabilities"),
  tabsetPanel(
    tabPanel(
      title = "Plots",
      mainPanel(width = 12,align = "left",
                selectInput("country","Select country",choices=show_available_countries(), selected = "United States"),
                selectInput("year","Select year",choices=rev(show_available_years())),
                submitButton("Plot"),
                tags$h3("Single Country-Year"),
                div(style = "float:left;width:50%;",plotOutput("country_year_bar_plot"))
      )
    ),
    tabPanel(
      title = "Data",
      mainPanel(width = 12,align = "center",
                tags$h3("TBD")
      )
    )))

server <- function(input, output, session) {
  one_country_year = reactive({
    plot_one_country_year(imprinting_df = get_imprinting_probabilities(as.numeric(input$year), input$country))
  })
  output$country_year_bar_plot = renderPlot({one_country_year()})
}

shinyApp(ui, server)
