#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(aesthetic)
flights <- read_csv("flights.csv", col_types = cols(FL_DATE = col_date(format = "%Y-%m-%d")))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
   # Background music
    tags$audio(src = "song.mp3", type = "audio/mp3", autoplay = NA, controls = NA),
  
   # Application title
    titlePanel("Flight Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectizeInput("Origin",
                     "Select your origin:", unique(flights$ORIGIN)),
         
         selectizeInput("Destination",
                        "Select your destination:", unique(flights$DEST))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     
     #Cancellation plot
      flights %>%
        group_by(UNIQUE_CARRIER) %>%
        summarise(pct_cancelled = mean(CANCELLED)) %>%
        ggplot(aes(x = UNIQUE_CARRIER, y = pct_cancelled)) +
         geom_bar(aes(fill = UNIQUE_CARRIER),stat = "identity") + 
         xlab("Carriers") +
         ylab("Percent of flight cancelled") + 
         scale_y_continuous(labels = scales::percent) +
         ggtitle("Percent of flights cancelled by carrier") +
         theme_bw() +
         theme(panel.border = element_blank()) +
         guides(fill = FALSE) +
         scale_fill_manual(values=rep(aesthetic(name="crystalpepsi"), times=4)) 
     
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

