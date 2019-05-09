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
library(DT)
library(ggplot2)
library(plotly)

run_pass_data <- read_csv("full_data.csv")

# Define UI for application that draws a histogram
ui <- navbarPage("Harvard Football Defense Analysis",
    
      tabPanel("About",

        fluidPage(
   
   # Application title
        titlePanel("Exploring the 2018 Season for the Harvard Football Defense"),
        
        p(paste("In the 2018 season, the Harvard Defense allowed 21.8 points per game in 10 games. This placed them 4th in 
                the league standings in total defense. This project analyzes data from every single play that the 
                defense saw throughout the entire season. Analyzing this data allows our coaches to view tendencies
                in their play calling and the opponents' play calling.")),
        br(),
        br(),
        br(),
        p(paste("Warning: This project uses terminology they will not make sense to the casual viewer. The audience
                intended for this project is strictly the Harvard Football coaches."))
        )
      ),
   
   tabPanel("Yardage",
            
            fluidPage(
              
              titlePanel("Yardage Summaries by Play Call"),
              
   # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "call",
                    label = "Choose Defensive Call",
                    choices = run_pass_data$cover_call,
                    selected = "DOME"
        ),
        selectInput(inputId = "opponent",
                    label = "Choose Opponent for Data Table",
                    choices = run_pass_data$opponent ,
                    selected = "Brown University")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        br(),
        br(),
        plotOutput("boxPlot"),
        
      br(),
      p(paste("Select an opponent on the left to see the five most frequent plays that each opponent ran.")),
      br(),
      DTOutput("linetable"),
      br(),
      br()
      )
)
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
            subtitle = "The Harvard Defense Allowed 3,502 yards on 675 snaps",
            caption = "Source: Harvard Football",
            y = "Gain (yards)",
            x = "Opponent") + theme(panel.grid = element_blank(),
                                        axis.ticks = element_blank())
      
   })
   
   
   output$linetable <- renderDT({
     opponent_table <- run_pass_data %>%
                      filter(opponent == input$opponent)
     
     datatable((opponent_table %>%
                  filter(!is.na(action)) %>%
                  count(action) %>%
                  arrange(desc(n)) %>%
                  head(5)))
     
     #divide by total number to get percentages then table or plot
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

