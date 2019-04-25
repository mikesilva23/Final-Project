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

run_pass_data <- read_csv("boxplot_data.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Harvard Football Defense Yardage Statistics"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "call",
                    label = "Choose Defensive Call",
                    choices = run_pass_data$cover_call,
                    selected = "DOME"
        ) 
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("boxPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   
   output$boxPlot <- renderPlot({
     subset_data <- run_pass_data %>%
       filter(cover_call == input$call)
     
     ggplot(data = subset_data, aes(x = subset_data$opponent, y = subset_data$gain)) + geom_boxplot() + coord_flip() +
       labs(title = "Yardage Allowed by Defensive Huddle Call",
            subtitle = "The Harvard Defense Allowed ____ yards on 675 snaps",
            caption = "Source: Harvard Football") + theme(panel.grid = element_blank(),
                                                                        axis.ticks = element_blank()) +
       xlab("Gain (yards)")
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

