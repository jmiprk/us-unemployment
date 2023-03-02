# understanding america's unemployment

library(shiny)
library(tidyverse)
library(scales)
library(sf)
library(maps)
library(dplyr)
library(viridis)
library(tools)
library(readxl)
library(ggiraph)
library(plotly)
library(htmlwidgets)

# data wrangling us_unemployment to
# contain region (state), month/year, and unemployment rate
us_unemployment <- read_csv("data/unemployment_rates.csv") %>% 
  mutate_all(as.character) %>% 
  pivot_longer(cols = -State, 
               names_to = "date", 
               values_to = "rate") %>% 
  rename(region = State) %>% 
  summarize(region = tolower(region), 
            date = date, 
            rate = rate)

unemployment_line <- read_csv("data/unemployment.csv")

unemployment_line$type[unemployment_line$type == 'actual'] <- 'Actual'

unemployment_line$type[unemployment_line$type == 'predicted'] <- 'Predicted\n(Without COVID-19)'

ui <- fluidPage(
  
  # title panel
  titlePanel("Understanding America's Unemployment"),
  tabsetPanel(
    tabPanel("Map",
             br(),
             br(),
             sidebarLayout(
               sidebarPanel(
                 # slider for month/year
                 sliderInput(inputId = "date",
                                       label = "Select a date:",
                                       min = as.Date(paste("2015-01","-01",sep="")),
                                       max = as.Date(paste("2022-10","-01",sep="")),
                                       value = as.Date(paste("2015-01","-01",sep="")),
                                       timeFormat = "%B %Y")
                 ),
               mainPanel(
                 # map of US with state unemployment rates
                 plotlyOutput("map", height = 450),
                 
                 # data citation
                 p("The ",
                   a("data",  href = "https://data.bls.gov/apps/covid-dashboard/home.htm"),
                   "used in this graphic was retrieved from the U.S. Bureau of Labor Statistics.")
                 )
               )
             ),
    tabPanel("ARIMA",
             br(),
             br(),
             div(
               plotlyOutput('arima_plot', width=1200),
               align='center'
               )
             )
    )
  )

server <- function(input, output) {
  
  output$map <- renderPlotly({
    
    # creating us_data
    # contains unemployment rate for each state for selected month/year
    us_data <- us_unemployment %>% 
      filter(date == format(input$date, "%B %Y")) %>% 
      full_join(map_data("state"), us_unemployment, by = "region")
    
    # creating US map with state unemployment rates
    # for selected month/year
    us <- ggplot(data=us_data, aes(x=long, y=lat, group=group,
                                   fill=as.numeric(rate), 
                                   text=paste(str_to_title(region), scales::percent(as.numeric(rate)/100, .1), sep=": "))) +
      geom_polygon(color="white") + 
      scale_fill_stepsn("Unemployment Rate",
                        colors=c("#f6bdc0", "#f1959b", "#f07470", "#ea4c46", "#dc1c13"), 
                        breaks=seq(2.5,12.5,by=2.5),
                        limits=c(2.5,12.5), 
                        guide='colorbar') +
      theme_classic() +
      labs(title = (paste("State Unemployment Rates in", format(input$date, "%B %Y")))) +
      theme(plot.title = element_text(size = 15, 
                                      vjust = 0.5,
                                      hjust = 0.5), 
            axis.title = element_blank(), 
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            legend.title = element_text(size = 11, 
                                        face='bold'),
            legend.text = element_text(size = 10))
    
    ggplotly(us, tooltip='text') %>% 
      highlight(
        "plotly_hover"
      ) %>%
      config(displayModeBar = F) %>% 
      layout(hoverlabel = list(bordercolor='black',
                               font=list(color='black')))
    
  })

  output$arima_plot <- renderPlotly({
    
    # line graph for labor_data
    arima_line <- ggplot(unemployment_line, aes(x=date, y=value, color=type, group=1,
                                                text=paste(format(as.Date(date), "%B %Y"),
                                                            paste(str_to_title(type),
                                                                  scales::percent(as.numeric(sprintf(value, fmt = '%#.1f'))/100), sep=": "),
                                                            sep='\n'))) +
      geom_line(size = 1) +
      theme_minimal() +
      scale_color_manual(name='',
                         values=c("#f07470", "#b65fcf")) +
      labs(x='Year', 
           y='Rate',
           title='Impact of COVID-19 Pandemic on Unemployment') +
      theme(plot.title = element_text(size=15, 
                                      vjust=0.5,
                                      hjust=0.5),
            axis.text = element_text(size=10),
            axis.title = element_text(size=10),
            legend.text = element_text(size=10))

    ggplotly(data = arima_line, tooltip='text') %>% 
      highlight(
        'plotly_hover',
      ) %>%
      config(displayModeBar = F) %>% 
      layout(hoverlabel = list(bordercolor='transparent',
                               font=list(color='white')))
  })
}

# running the application 
shinyApp(ui = ui, server = server)
