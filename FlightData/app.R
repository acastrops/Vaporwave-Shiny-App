#
# This is a vaporwaveTM Shiny web application for visualizing flight
# data time series. 
#


#Packages
  library(shiny)
  library(readr)
  library(ggplot2)
  library(dplyr)
  library(aesthetic)

#Data
  flights <- read_csv("flights.csv", col_types = cols(FL_DATE = col_date(format = "%Y-%m-%d")))
  
  #Defining columns we will be using
    # carriers <- factor(flights$UNIQUE_CARRIER)
    # year <- as.Date(as.character(flights$FL_DATE), format="%Y-%m-%d")
    # ddelay <- 
    # adelay <- 
    # cancelled <- 
  
  
# Define UI for application that makes the graphs
ui <- fluidPage(
  
   # Background music
    tags$audio(src = "song.mp3", type = "audio/mp3", autoplay = FALSE, controls = NA),
  
   # Application title
    titlePanel("Flight Data"),
   
   # Sidebar with a dropdown to select input
   sidebarLayout(
      sidebarPanel(
         selectizeInput("Origin",
                     "Select your origin:", 
                     sort(unique(flights$ORIGIN)),
                     multiple = TRUE,
                     selected = c("MIA")),
         
         selectizeInput("Destination",
                        "Select your destination:", 
                        NA,
                        multiple = TRUE)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("totalFlightsPlot"),
         plotOutput("cancelledPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

     update_destination_choices <- reactive(flights %>%
                                              filter(flights$ORIGIN %in% input$Origin) %>%
                                              group_by(DEST) %>%
                                              summarize(count = n()) %>%
                                              arrange(desc(count)))
  
     observe({updateSelectizeInput(session = session, 
                                   inputId ="Destination", 
                                   choices = sort(update_destination_choices()$DEST),
                                   )})
     
     # use filtered_flights_by_carrier() instead of 
     # always copying/pasting the filtering in graphs
     
     filtered_flights_by_carrier <- reactive(flights %>%
                                              filter(ORIGIN %in% input$Origin & 
                                                     DEST %in% input$Destination) %>%
                                              group_by(UNIQUE_CARRIER)
                                            )
     
     # Create a single Vaporwave Theme
     vaporwave_theme <- theme_bw() +
       theme(panel.border = element_blank())
     
     # If you get "RHS" errors, add the command to the list like this one
     vaporwave_theme <- list(vaporwave_theme, 
                             scale_fill_manual(values=rep(aesthetic(name="crystalpepsi"), times=4)),
                             guides(fill = FALSE))
     
     output$totalFlightsPlot <- renderPlot({
       filtered_flights_by_carrier() %>%
         summarise(num_flights = n()) %>%
         ggplot(aes(x = UNIQUE_CARRIER, y = num_flights)) +
         geom_bar(aes(fill = UNIQUE_CARRIER),stat = "identity") + 
         xlab("Carriers") +
         ylab("Number of scheduled flights") + 
         ggtitle("Number of total flights by carrier, 2017") +
         vaporwave_theme 
     })
     
     output$cancelledPlot <- renderPlot({ #Cancellation plot
        filtered_flights_by_carrier() %>%
        summarise(pct_cancelled = mean(CANCELLED)) %>%
        ggplot(aes(x = UNIQUE_CARRIER, y = pct_cancelled)) +
         geom_bar(aes(fill = UNIQUE_CARRIER),stat = "identity") + 
         xlab("Carriers") +
         ylab("Percent of flight cancelled") + 
         scale_y_continuous(labels = scales::percent) +
         ggtitle("Percent of flights cancelled by carrier") +
         vaporwave_theme 
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

