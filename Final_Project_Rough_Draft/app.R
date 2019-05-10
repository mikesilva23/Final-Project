#load in all packages that I will need throughout the app

library(shiny)
library(tidyverse)
library(DT)
library(ggplot2)
library(plotly)
library(shinythemes)

#In a separate rmd file, I shrunk down my dataset to include the variables that I will need in
#this project. I then wrote a new csv out of that data set, and read in that data here. 

run_pass_data <- read_csv("full_data.csv")

# Define UI for application that draws a histogram
ui <- navbarPage("Harvard Football Defense Analysis",theme = shinytheme("simplex"),
    
      #My first panel is my about page where I simply describe what I hope to 
      #do with this project. I make a warning that many people may not be able to understand
      #some of the information being displayed, but that is because they are not supposed to!
                 
      tabPanel("About",

        fluidPage(
   
   # Application title
        titlePanel("Exploring the 2018 Season for the Harvard Football Defense"),
        
        p(paste("In the 2018 season, the Harvard Defense allowed 21.8 points per game in 10 games. This placed them 4th in 
                the league standings in total defense. Our coaches have always prided themselves on being the toughest 
                and best defense in the league, so 4th place is not close to where they would like to be. How can they 
                figure out what went wrong and where they can be better? Using this web app!This project analyzes data
                from every single play that the defense saw throughout the entire season. Analyzing this data allows
                our coaches to view tendencies in their play calling and the opponents' play calling. This may be the
                key to putting the Harvard defense back in the number 1 spot in the Ivy League.")),
        br(),
        br(),
        p(paste("Warning: This project uses terminology they will not make sense to the casual viewer. The audience
                intended for this project is strictly the Harvard Football coaches."))
        )
      ),
   
   #This tab explores the coverage calls that our coaches call every single play of the game.
   #Reviewing summaries of their playcalling will help them get an idea on their own tendencies.
   
   tabPanel("Play Call Summaries",
            
            fluidPage(
              
              titlePanel("Summary Statistics by Play Call"),
              
   # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        
        #The first input I use allows the users to choose the specific play calls that our
        #defense uses. This input will connect to a plot that is later created.
        
        selectInput(inputId = "call",
                    label = "Choose Defensive Call",
                    choices = run_pass_data$cover_call,
                    selected = "DOME"
        ),
        
        #This second input allows users to select one of the ten opponents that Harvard
        #played against this season. When they select an opponent, a data table created below
        #will be displayed. 
        
        selectInput(inputId = "opponent",
                    label = "Choose Opponent for Data Table",
                    choices = run_pass_data$opponent ,
                    selected = "Brown University")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
        #br() creates white space throughout the webpage. This
        #is for aesthetic purposes and making the page cleaner.
        #This will be repeated several times throughout the app.
        
        br(),
        br(),
        
        #This p(paste()) function allows me to insert text into
        #the app. This process will also be repeated several times
        #throughout the app.

        p(paste("Hovering over the plot will allow you to view summary statistics about the chosen coverage call.
                allows our coaches to view what defenses were successful against certain teams, and which were not.
                Coaches can then make a better decision on what calls to use in the future against those teams.
                Using the data table below, the coaches will see which calls they used the most against certain
                teams, then review how successful those calls were in this plot.")),
        
        #I chose to quickly discuss what this page is going to do first, then input
        #the plot and data table below it. This allows the users to understand what's
        #going on and avoid confusion.
        
        plotlyOutput("boxPlot"),
      br(),
      p(paste("Select an opponent on the left to see the five most frequent plays that each opponent ran.")),
      br(),
      DTOutput("linetable"),
      br(),
      br(),
      
      #I chose to adjust the width so that the plot takes up the entire page
      #instead of only a section of the page.
      
      width = 12
      )))),
    
   #The second tab on the app discusses game trends. This allows the 
   #coaches to see how the flow of the games went and refer back to 
   #game film to see when the defense let up big plays or made big
   #plays
   
   tabPanel("Game by Game",
             
             fluidPage(
               
               titlePanel("Opponent Yardage per Play"),
               sidebarLayout(
                 sidebarPanel(
                   
                   #This input allows the user to select the opponent once again.
                   #When the opponent is selected, a plot will appear that is created
                   #below.
                   
                   selectInput(inputId = "Opponent",
                               label = "Choose Opponent",
                               choices = run_pass_data$opponent,
                               selected = "Brown University")
                 ),
                 
                 #Once again, I decided to describe what the plot shows beforehand so that 
                 #users have a better understanding of what they are looking at.
                 
                 mainPanel(
                   br(),
                   br(),
                   p(paste("Hovering over data points allows you to view which play number the data point is in
                           relation to film breakdown. This allows our coaches to find outlying data points, go
                           into the film, and see what exactly happened on that play. The second to last number in
                           the hover info relates to which play number it is in the film, and the final number is the
                           yardage gained on that specific play.")),
                   plotlyOutput("game_plot"),
                   br(),
                   br(),
                  
                   #Once again I adjust the width so that the plot takes up the full page 
                  
                   width = 12
                 )
               )
             )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #I generate the boxplot for the play call summaries tab here
  #I felt that using a boxplot was the best choice to display this
  #data because it allows the coaches to get a better understanding
  #of how successful each call was. Hopefully they understand 
  #quartiles!
   
  output$boxPlot <- renderPlotly({
     subset_data <- run_pass_data %>%
       filter(cover_call == input$call)
     
     
     #I felt that using plotly was the best for these plots because they
     #allow the coaches to see specific summaries instead of guessing.
     #In football, there is a big difference between a 9 yard gain 
     # and 11 yard gain, and using plotly allows the coaches to be
     #sure about those numbers. 
     
     plot_ly(x = ~subset_data$opponent, y = ~subset_data$gain) %>% add_boxplot() %>% 
       layout(xaxis = list(title = "Opponent"),
              yaxis = list(title = "Gain")) %>%
       
       #The toolguide in ploty is pointless for this plot so I simply
       #removed it using this code. 
       
       config(displayModeBar = FALSE)

      
   })
   
   
  #I construct the data table on the play call summaries here. This data table
  #takes the user's choice and constructs a table displaying the coverage calls
  #against the chosen opponent in order byy frequency.
  
  output$linetable <- renderDT({
     opponent_table <- run_pass_data %>%
                      filter(opponent == input$opponent)
     
     datatable((opponent_table %>%
                  group_by(cover_call) %>%
                  count(cover_call) %>%
                  arrange(desc(n))), 
                  colnames = c("Coverage Call", "Frequency"))

   })
   
  #I create the scatter plot for the game by game tab here 
  
  output$game_plot <- renderPlotly({
     game_data <- run_pass_data %>%
       filter(opponent == input$Opponent)
     
     #this scatter plot displays every single play against the opponent that
     #the user selected and the yardage gained on that play. Using plotly on
     #this graph allows the coaches to hover over outlying values and see the
     #play number for that play. They can then turn to the film and go directly
     #to that play and see what happened on that play, which allows them to fix 
     #mistakes quickly.
     
     plot_ly(x = ~game_data$name, y = ~game_data$gain) %>% add_markers() %>% 
       layout(xaxis = list(title = "Opponent"),
              yaxis = list(title = "Gain")) %>% 
       
       #removed the plotly toolguide again 
       
       config(displayModeBar = FALSE)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

