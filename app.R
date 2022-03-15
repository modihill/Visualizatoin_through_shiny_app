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
library(plotly)
library(tidyr)
library(tidyverse)
library(dplyr)

# import datasets
gr_development <- read_csv("population-growth-rate-by-level-of-development.csv")

fertility_rate <- read_csv("fertility rate.csv", 
                           skip = 3)

life_expectancy <- read_csv("life_expactancy.csv", 
                            skip = 3)

infant_mortality_rate <- read_csv("mortality rate.csv", 
                                  skip = 3)

population_growth <- read_csv("population-growth-the-annual-change-of-the-population.csv")

deaths <- read_csv("number-of-deaths-per-year.csv")

births <- read_csv("number-of-births-per-year.csv")

# Data Wrangling
life_expectancy <- life_expectancy[-c(3,4)]
life_expectancy <- life_expectancy %>% filter(`Country Name` == "India" | `Country Name` == "Poland" |
                                                  `Country Name` == "China" |
                                                  `Country Name` == "Nigeria" | `Country Name` == "Niger" | 
                                                  `Country Name` == "Japan" | `Country Name` == "World")
life_expectancy <- pivot_longer(life_expectancy, `1960`:`2020`, 
                                names_to = "year", 
                                values_to = "life_expectancy")
life_expectancy <- life_expectancy[-c(3)]


fertility_rate <- fertility_rate[-c(3,4)]
fertility_rate <- fertility_rate %>% filter(`Country Name` == "India" | `Country Name` == "Poland" |
                                                `Country Name` == "China" |
                                                `Country Name` == "Nigeria" | `Country Name` == "Niger" | 
                                                `Country Name` == "Japan" | `Country Name` == "World")
fertility_rate <- pivot_longer(fertility_rate, `1960`:`2020`, 
                               names_to = "year", 
                               values_to = "fertility_rate")
fertility_rate <- fertility_rate[-c(3)]


colnames(gr_development) <- c("country", "code", "year", "Estimate", "prediction")
growth_rate_country <- gr_development %>% filter(country == "India" | country == "Poland" |
                                                     country == "China" |
                                                     country == "Nigeria" | country == "Niger" | 
                                                     country == "Japan" | country == "World" )

##---------------------------------------------------------------------------------##

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("What key factors are affecting the world's Annual Population Growth rate?"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            tags$div(
                tags$h4("What does article say about Population Growth "),
                tags$p("According to ", tags$a(href="https://en.wikipedia.org/wiki/Population_decline", "Population Decline"), "and ", 
                       tags$a(href="https://ourworldindata.org/world-population-growth", "Our World in Data"), "the fertility rate, life expectancy and income are the most affecting factors, which leads to decline in absolute population change."),
            ),
            
            tags$div(
                tags$h4("Reason for choosing following Countries to compare"),
                tags$p("The developed and high income countries like Japan and Poland, undeveloped and lower income countries like Nigeria and Niger along with middle income country India has been chosen for the Analysis. "),
                tags$p("While China is chosen because it has shown tremendous economic and infrastructure growth over past few decades and convert itself into develop and higher income country from undeveloped country. "),
            ),
            tags$div(
                tags$h4("Conclusion"),
                tags$p("The factors, like life-expectancy and fertility rate plays major role in deciding population growth. For example, Japan has highest life-expectancy and is gradually increased from 1970, while for the same time period its population growth was declining for same time period. Fertility rate of china was around 6.5 in 7th decade, then it dropped to 2.5 due to child policy, then slightly rose again till 1990, which reflects in lower annual population growth in the next years."),
                tags$p("In a nutshell, from the observation of plots, it is depicts that, lower the life expectancy and higher the fertility rate, higher the annual population growth and vice versa. That’s the one major reason why developed and high income countries like Japan, Poland has lowest or negative annual population growth compared to lower income countries like Nigeria and Niger."),
            ),
            tags$div(
                tags$h4("Data Source"),
                tags$p("Our World in Data. Absolute Increase in global population per year. Available ", tags$a(href="https://ourworldindata.org/grapher/absolute-increase-global-population?country=~OWID_WRL", "Here"),"as of 26th October, 2021."),
                tags$p("The World Bank|Data. Fertility rate, total (births per woman). Available ", tags$a(href="https://data.worldbank.org/indicator/SP.DYN.TFRT.IN", "Here"),"as of 26th October, 2021."),
                tags$p("The World Bank|Data. Life expectancy at birth, total (years). Available ", tags$a(href="https://data.worldbank.org/indicator/SP.DYN.LE00.IN", "Here"),"as of 26th October, 2021."),
            ),
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Annual Population Growth Rate", tags$p(" "), plotlyOutput("p3"),
                                 tags$p(" "),
                                 tags$p(" "),
                                 tags$h4("Observation"),
                                 tags$p("The plot suggests that after the peak of World's absolute population change in the year of 1987, and little rise in 2014, the population change is declining and it is estimated that by the end of this century, absolute change will be minimal if current pace of world population of growth continues."),
                                 tags$p("While, the world’s annual population change will become zero, the annual growth rate of first world countries like Japan and Poland is already negative and middle income country like India will join them in begining of 7th decade. Even though, Nigeria and Niger’s population growth will decline, but it is still positive."),
                        ),
                                                
                        tabPanel("Fertility",  tags$p(" "), plotlyOutput("p6"),
                                 tags$p(" "),
                                 tags$p(" "),
                                 tags$h4("Observation"),
                                 tags$p("The Fertility plot depicts that, developed and high income countries like Japan, Poland has lowest fertility rate throughout the years, while lower-middle income country like china joined them around 1995. However, lower income countries Nigeria and Niger has higher fertility rate (6.5 and 7.5 children per woman) throughout the year."),
                        ),
                        tabPanel("Life-Expectancy", tags$p(" "), plotlyOutput("p4"),
                                 tags$p(" "),
                                 tags$p(" "),
                                 tags$h4("Observation"),
                                 tags$p("The life-expectancy plot shows that, first world countries has higher life-expectancy compared to third world countries. However, life expectancy is increasing through-out the years in world."),
                        )
            ),
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    p3 <- plot_ly(growth_rate_country, x = ~year, y = ~Estimate,
                  type = 'scatter', 
                  color=~country, mode="lines")
    
    p3 <- p3 %>% add_trace(y = ~prediction, group=~country, type = 'scatter', 
                           color=~country, mode="markers", showlegend = FALSE)
    p3 <- p3 %>% layout(title = "Annual Population Growth Rate of countries", 
                        xaxis = list(title = "Year",
                                     showgrid = TRUE),
                        yaxis = list(title = "Growth Rate",
                                     showgrid = TRUE))
    
    output$p3 <- renderPlotly({p3})
    
    # Life-expectancy
    p4 <- plot_ly(life_expectancy, x = ~year, y = ~life_expectancy, group=~`Country Name`,
                  type = 'scatter', 
                  color=~`Country Name`, mode="lines")
    
    p4 <- p4 %>% layout(title = "Life Expectancy of different countries over years", 
                        xaxis = list(title = "Year",
                                     showgrid = TRUE,
                                     zeroline = FALSE),
                        yaxis = list(title = "Life Expectancy in years",
                                     showgrid = TRUE))
    
    output$p4 <- renderPlotly({p4})
    
    # firtility-rate
    p6 <- plot_ly(fertility_rate, x = ~year, y = ~fertility_rate, group=~`Country Name`,
                  type = 'scatter', 
                  color=~`Country Name`, mode="lines")
    
    p6 <- p6 %>% layout(title = "Fertility rate (births per woman) of different countries over years", 
                        xaxis = list(title = "Year",
                                     showgrid = TRUE,
                                     zeroline = FALSE),
                        yaxis = list(title = "Births per woman",
                                     showgrid = TRUE))
    output$p6 <- renderPlotly({p6})    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
