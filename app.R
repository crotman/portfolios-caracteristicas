#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DBI)
library(sqliteutils)

conexao <- dbConnect(RSQLite::SQLite(), "dados/db.db")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("portfolios por quintil"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            width = 2,
            selectInput("caracteristica",
                        "Tipo de Característica",
                        choices = c("Value" = "l_p", "Qualidade" = "f_score")            
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot_cotas")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot_cotas <- renderPlot({
      
      
      cotas <- tbl(conexao, "resultados") |> 
        filter(
          caracteristica == local(input$caracteristica)
        ) |> 
        collect() |> 
        mutate(
          data = slu_date_to_r(data)
        )
      
    

            
      ggplot(cotas) +
        geom_line(
          aes(
            x = data,
            y = cota_portfolio,
            color = factor(quintil),
            group = quintil
          ),
          line_width = 1
        ) +
        theme_minimal() +
        scale_colour_manual(
          values = c(
            "1" = "darkred",
            "2" = "orange",
            "3" = "darkgray",
            "4" = "darkblue",
            "5" = "darkgreen"
          )
        ) +
        theme(
          legend.position = "top"
        ) +
        labs(
          color = "Quintil"
        )
      
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
