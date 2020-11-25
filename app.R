#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(plotly)
library(tidyverse)
library("rnaturalearth")
library("rnaturalearthdata")

### HELPER FUNCTIONS ###
my_map_theme <- function(){
    theme(panel.background=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank())
}

to_label <- function(inputstat) {
    if (as.character(inputstat) == "total_cases_per_million"){
        return("Total Cases per Million")
    }else if (as.character(inputstat) == "new_cases_per_million"){
        return("New Cases per Million")
    }else if (as.character(inputstat) == "total_deaths_per_million"){
        return("Total Deaths per Million")
    }else{
        return("Total Tests per Thousand")
    }
}

rate_switch <- function(inputstat){
    if(as.character(inputstat) == "total_cases_per_million" 
       || as.character(inputstat) == "new_cases_per_million"
       || as.character(inputstat) == "total_deaths_per_million"){
        
        return("per million")
        
        }else{
            
            return("per thousand")
            
        }
}
### ---------------- ###

world <- ne_countries(scale = "medium", returnclass = "sf")

covidstats <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv",
                        col_types = cols(date = col_date(format = "%Y-%m-%d"), total_tests_per_thousand = col_double()))

covidmap <- function(stat, mydate){
    my_stat <- enquo(stat)
    covid_oneday <- covidstats %>%
        filter(date==mydate) %>%
        select(location, iso_code, mystat = !!my_stat)
    mapdata <- left_join(world, covid_oneday, by=c("iso_a3"="iso_code"))
    m <- mapdata %>%
        mutate(text = paste("<b>",location,"</b>\nRate:", mystat, rate_switch(stat))) %>%
        ggplot() +
        geom_sf(aes(fill=mystat, text=text), color="black")+
        scale_fill_continuous(to_label(stat), low="white",high="red") +
        my_map_theme()
    ggplotly(m, tooltip="text") %>%
        style(hoveron="fill")
    
}  

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Worldwide COVID Statistics by Country"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("mystat",
                        "Statistic to View:",
                        choices = list("Total Cases Per Million"="total_cases_per_million",
                                       "New Cases Per Million"="new_cases_per_million",
                                       "Total Deaths Per Million"="total_deaths_per_million",
                                       "Total Tests per Thousand"="total_tests_per_thousand"),
                        selected = "total_cases_per_million"),
            sliderInput("mydate",
                        "Select a date to display:",
                        min = as.Date("2019-12-31", "%Y-%m-%d"),
                        max = Sys.Date(),
                        value = as.Date("2020-11-23"),
                        timeFormat="%m-%d")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("map")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$TitleText <- renderText(paste(input$mystat, "On", input$mydate))
    output$SubtitleText <- renderText(paste("Map shows the rate of", input$mystat,
                                            "on the given date."))
    output$map <- renderPlotly({
        covidmap(input$mystat, input$mydate)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
