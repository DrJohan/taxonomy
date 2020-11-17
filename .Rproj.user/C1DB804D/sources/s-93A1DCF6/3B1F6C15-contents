#
# This shiny app generates random data in the form of time series patterns
# it also matches data to some of the patterns
#

library(shiny)
library(lubridate)
library(tsibble)
library(magrittr)
library(dplyr)
library(ggplot2)
library(plotly)
library(fable)
library(feasts)

alpha = 0.1
rho = 0.5
DOW_effects <- c(-0.5, 1, 1.25, 1.5, 1.3, 1.1, -0.75)
month_effects <- c(-1,-0.5, -0.25, 1, 1.25, 1.5, 1.25, 1, -0.25, -0.5, -0.75, -1)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Exploring features of time series"),
    
    sidebarPanel(
             radioButtons("features",
                        "Choose data features:",
                        choices = c("trend",
                                    "autocorrelation",
                                    "daily seasonal patterns",
                                    "annual seasonal patterns")),
             
             sliderInput("n_obs",
                         "Choose the number of years",
                         min = 1,
                         max = 50,
                         value = 1),
             sliderInput("alpha",
                         "Strength of trend",
                         min = -0.3,
                         max = 0.3, value = 0, step = 0.01),
             sliderInput("rho",
                         "Strength of autocorrelation",
                         min = -1.1,
                         max = 1.1, 
                         value = 0, step = 0.1),
             radioButtons("frequency",
                          "Choose the frequency of the data:",
                          choices = c("day",
                                      "month",
                                      "year")),
             numericInput("year", "Choose year to begin", value = 2015),
             
              
                 width = 3   )
                ,
            mainPanel(
    tabsetPanel(
        
        
                tabPanel("Data", plotOutput("data_plot"), width = "100%"),
                tabPanel("Decomposition", plotOutput("stl"), width = "100%"),
                tabPanel("ACF", plotOutput("acf"), width = "100%")
            )
          
           
            
            )
        

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    gen_data <- reactive({
        
              start <- as.Date(paste(input$year, "-01-01", sep = ""), format = "%Y-%m-%d")
            finish <-  as.Date(paste(input$n_obs + input$year, "-01-01", sep = ""), format = "%Y-%m-%d")
            dates <- seq(start, finish, by= "days")
            
            
            y <- 0
            
            for (i in 2:length(dates)){
                
                
                if (input$features == "trend"){
                    new_y <- input$alpha * i +rnorm(1, 0, 1)
                } else if (input$features == "autocorrelation"){
                    new_y <- input$rho*y[i-1] +rnorm(1,0.1)
                } else if (input$features == "daily seasonal patterns"){
                    day <- lubridate::wday(dates[i])
                    
                    new_y <- DOW_effects[day] +rnorm(1, 0, 1)
                } else if (input$features == "annual seaosnal patterns"){
                    month <- lubridate::month(dates[i])
                    new_y <- month_effects[month] + rnorm(1,0,1)
                }
                y <- c(y, new_y)
            }
            
            gen_data_y <- bind_cols(dates, y) 
            colnames(gen_data_y) <- c("dates", "y")
            if(input$frequency == "month"){
                gen_data_y <- gen_data_y %>%
                    mutate(month = yearmonth(dates))%>%
                    group_by(month)%>%
                    summarise(y = sum(y, na.rm = TRUE))%>%
                    as_tsibble(index = month) %>%
                    mutate(dates = as.Date(month))
            } else if (input$frequency == "year"){
                gen_data_y <- gen_data_y %>%
                    mutate(year = lubridate::year(dates))%>%
                    group_by(year)%>%
                    summarise(y = sum(y, na.rm = TRUE))%>%
                    as_tsibble(index = year)%>%
                    mutate(dates = as.Date(paste(year, "-01-01", sep = ""), format = "%Y-%m-%d"))
            } else {
                gen_data_y <- gen_data_y %>% as_tsibble(index = dates)
            }
            
           
        
        
        
       
         gen_data_y
    })

    output$data_plot <- renderPlot({
        df <- gen_data()
        
        df <- df[2:nrow(df)-1,]

        plot <- ggplot(df)+
            geom_line(aes(dates, y), colour = "steelblue4", alpha = 0.5)+
            theme_light()+
            xlab("Date")+
            ylab("y")
        
       plot
    }, height = 900, width = 1300)
    
    output$stl <- renderPlot({
        df <- gen_data()
        
     x <- df %>% model(STL(y ~ season(window = "periodic") + trend(window = Inf)) )%>%
                components() 
     
     if(input$frequency == "month"){
         x <- x %>%
             tidyr::pivot_longer(-c(.model, month),values_to = "value", names_to = "component")%>%
             mutate( date = month)
     } else if (input$frequency == "year"){
         x <- x %>%
             tidyr::pivot_longer(-c(.model, year),values_to = "value", names_to = "component")%>%
             mutate( date = year)
     } else if (input$frequency == "day"){
         x <- x %>%
             tidyr::pivot_longer(-c(.model, dates),values_to = "value", names_to = "component")%>%
             mutate( date = dates)
     }
                x <- x %>%
                    mutate(component = factor(component, levels = c("y", "trend", "season_year", 
                                                                    "remainder", "season_adjust"))) %>%
                filter(component != "season_adjust")
     
     
                ggplot(x)+
                facet_wrap(component ~., ncol = 1, scales = "free")+
                geom_line(aes(date, value), colour = "steelblue4", alpha = 0.5)+
                theme_light()+
                theme(text = element_text(size = 16))
                
    }, height = 900, width = 1300)
    
    output$acf <- renderPlot({
        df <- gen_data()
        
        x <- df %>% ACF() 
        
        ggplot(x)+
            geom_col(aes(lag, acf), fill = "steelblue4", alpha = 0.5)+
            theme_light()+
            theme(text = element_text(size = 16))
                
        
    }, height = 900, width = 1300)
}

# Run the application 
shinyApp(ui = ui, server = server)
