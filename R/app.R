# This is a vaporwaveTM Shiny web application for visualizing flight
# data time series. 
#

#Packages
library(install.load)
install_load('shiny')
install_load('tidyverse')
install_load('here')
if (!require(aesthetic)) {
  install_load('devtools')
  devtools::install_github("mackenziedg/aesthetic")
  library(aesthetic)
}



#Data
#flights <- read_csv("flights.csv", col_types = cols(FL_DATE = col_date(format = "%Y-%m-%d")))
flights.path <- here("data", "flights.tbl")
flights <- read_rds(flights.path) # Loading a serialized, compressed version of the dataset

# Define UI for application that makes the graphs
ui <- fluidPage(
  
  # External style sheets 
  includeCSS(here("www", "bootstrap.min.css")),
  includeCSS(here("www", "aesthetic.css")),
  
  # Application title
  titlePanel("ＦＬＩＧＨＴ　ＤＡＴＡ　遅延便"),
   
  fluidRow(
    column(width = 6,
    selectizeInput("Origin",
                   "Select your origin:", 
                   sort(unique(flights$ORIGIN)),
                   multiple = TRUE,
                   selected = c("MIA", "FLL"))),
    
    column(width = 6,
      selectizeInput("Destination",
                   "Select your destination:", 
                   NA,
                   multiple = TRUE))
  ),
  fluidRow(
    column(width = 6, 
   plotOutput("totalFlightsPlot")),
   column(width = 6,
   plotOutput("cancelledPlot")
   )
  ),
  fluidRow(
    column(width = 6, 
           plotOutput("totalFlightsTSPlot")),
    column(width = 6,
           plotOutput("cancelledTSPlot")
    )
  ),
  fluidRow(
    column(width = 6, 
           plotOutput("averageDepDelayPlot")),
    column(width = 6,
           plotOutput("averageArrDelayPlot")
    )
  ),
  
  
  # Background music
  tags$audio(src = "song.mp3", type = "audio/mp3", autoplay = FALSE, controls = NA)
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
                                   selected = {
                                     if(is.null(input$Destination)){
                                       update_destination_choices()$DEST[1]
                                     } else {
                                       input$Destination
                                     }
                                   }
                                   )})
     
     
     # Create a single Vaporwave Theme
     vaporwave_theme <- theme_bw() + 
       theme(text = element_text(color = "white",
                                 family = "Helvetica"),
             rect = element_rect(fill="white"),
             axis.text = element_text(color = "white"),
             plot.background = element_rect(fill = "black"),
             panel.background = element_rect(fill = "black"),
             panel.border = element_blank(),
             
             title = element_text(family = "Times", size = 18))
     
     # If you get "RHS" errors, add the command to the list like this one
     vaporwave_theme <- list(vaporwave_theme, 
                             scale_fill_manual(values=rep(aesthetic(name="jazzcup"), times=4)),
                             #scale_color_manual(values=rep(aesthetic(name="jazzcup"), times=4)),
                             guides(fill = FALSE))
     
     
     # use reactive functions instead of 
     # always copying/pasting the filtering in graphs
     filtered_flights_by_carrier <- reactive(flights %>%
                                              filter(ORIGIN %in% input$Origin & 
                                                     DEST %in% input$Destination) %>%
                                              group_by(UNIQUE_CARRIER)
                                            )
     filtered_flights_by_carrier_date <- reactive(flights %>%
                                               filter(ORIGIN %in% input$Origin & 
                                                        DEST %in% input$Destination) %>%
                                               group_by(FL_DATE, UNIQUE_CARRIER)
                                                  )
    
     
     output$totalFlightsPlot <- renderPlot({
       filtered_flights_by_carrier() %>%
         summarise(num_flights = n()) %>%
         ggplot(aes(x = UNIQUE_CARRIER, y = num_flights)) +
         geom_bar(aes(fill = UNIQUE_CARRIER),stat = "identity") + 
         labs(title="ＴＯＴＡＬ　ＦＬＩＧＨＴＳ　流畝ンど",
              x="Carrier", y="Number of scheduled flights") +
         vaporwave_theme 
     })
     
     output$totalFlightsTSPlot <- renderPlot({
       filtered_flights_by_carrier_date() %>%
         summarise(num_flights = n()) %>%
         ggplot(aes(x = FL_DATE, y = num_flights, group=UNIQUE_CARRIER, color=UNIQUE_CARRIER)) +
         geom_line() + 
         labs(title='',
              x='', y="Number of scheduled flights") +
         vaporwave_theme
     })
     
     output$cancelledPlot <- renderPlot({ #Cancellation plot
        filtered_flights_by_carrier() %>%
        summarise(pct_cancelled = mean(CANCELLED)) %>%
        ggplot(aes(x = UNIQUE_CARRIER, y = pct_cancelled)) +
         geom_bar(aes(fill = UNIQUE_CARRIER),stat = "identity") + 
         labs(title="ＣＡＮＣＥＬＬＥＤ　ＦＬＩＧＨＴＳ",
              x="Carrier", y="Percent of flights cancelled") +
         scale_y_continuous(labels = scales::percent) +
         vaporwave_theme 
   })
     
     output$cancelledTSPlot <- renderPlot({ #Cancellation plot
       filtered_flights_by_carrier_date() %>%
         summarise(pct_cancelled = mean(CANCELLED)) %>%
         ggplot(aes(x = FL_DATE, y = pct_cancelled, group=UNIQUE_CARRIER, color=UNIQUE_CARRIER)) +
         geom_line() +
         labs(title='',
              x='', y="Percent of flights cancelled") +
         scale_y_continuous(labels = scales::percent) +
         vaporwave_theme
     })

     output$averageDepDelayPlot <- renderPlot({
       filtered_flights_by_carrier() %>% 
         summarize(avg_delay = mean(DEP_DELAY,na.rm = TRUE)) %>% 
         arrange(desc(avg_delay)) %>% 
         ggplot(aes(x = UNIQUE_CARRIER, y = avg_delay)) +
             geom_bar(aes(fill = UNIQUE_CARRIER),stat = "identity") +
             labs(title="ＡＶＥＲＡＧＥ　ＤＥＰＡＲＴＵＲＥ　ＤＥＬＡＹ　憶ゖ゛",
                  x="Carrier", y="Average delay (minutes") +
             vaporwave_theme

     })

      output$averageArrDelayPlot <- renderPlot({
        filtered_flights_by_carrier() %>% 
          summarize(avg_delay = mean(ARR_DELAY,na.rm = TRUE)) %>% 
          arrange(desc(avg_delay)) %>% 
          ggplot(aes(x = UNIQUE_CARRIER, y = avg_delay)) +
          geom_bar(aes(fill = UNIQUE_CARRIER),stat = "identity") +
          labs(title="ＡＶＥＲＡＧＥ　ＡＲＲＩＶＡＬ　ＤＥＬＡＹ　亜ピ逸",
               x="Carrier", y="Average delay (minutes)") +
          vaporwave_theme
      })
     
}

# Run the application 
shinyApp(ui = ui, server = server)
