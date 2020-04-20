library(shiny)
library(shinythemes)
library(ggplot2)
library(readr)
library(tidyverse)
library(usmap)

aco <- readRDS(file = "data/aco.RDS")
county <- readRDS(file = "data/county.RDS")

# Define UI for application that draws a histogram
ui <- navbarPage(
    "2018 Accountable Care Organizations",
    
    # Intro -------------------------------------------------------------------
    
    
    tabPanel("Intro",
            includeHTML("intro.html")
    ),
    
    
    # Results -----------------------------------------------------------------
    
    tabPanel("Results",
            includeHTML("results.html")
    ),
    
    # Model -----------------------------------------------------------------
    
    tabPanel("Model",
            includeHTML("model.html")
    ),
    
    
    # Data Exploration --------------------------------------------------------
    
    tabPanel("Data Exploration",
             fluidPage(theme = shinytheme("paper"),
                       
                       titlePanel("Accountable Care Organizations"),
                       
                       p(""),
                       h3("What does the ACO landscape look like?"), 
                       p("Let's start by look at ACOs nationwide by counties."),
                       
                       
                       sidebarLayout(
                           sidebarPanel(
                               
                               selectInput(inputId = "beneficiary_2",
                                           label = "Beneficiary Type",
                                           choices = county$Beneficiary),
                               
                               selectInput(inputId = "category_2",
                                           label = "Category", 
                                           choices = county$Category),
                           ),
                           
                           mainPanel(
                               plotOutput("usa")
                           )
                       ),
                       
                       h3("ACOs by State"),
                       p("Here, we can take a look state by state."),
                       
                       sidebarLayout(
                           
                           sidebarPanel(
                               
                               selectInput(inputId = "state",
                                           label = "State",
                                           choices = county$state_name), 
                               
                               selectInput(inputId = "beneficiary",
                                           label = "Beneficiary Type",
                                           choices = county$Beneficiary),
                               
                               selectInput(inputId = "category",
                                           label = "Category", 
                                           choices = county$Category)
                           ),
                           mainPanel(
                               plotOutput("state")
                               
                           )
                       )
             )
    ),
    
    # Discussion --------------------------------------------------------------
    
    
    tabPanel(
        "Discussion",
        fluidRow(
            column(2, ""),
            
            column(8, 
                   titlePanel("Discussion Title"),
                   p("What to say, what to say?")),
            
            column(2, "")
        )
        
    ),
    
    
    # About -------------------------------------------------------------------
    
    
    tabPanel(
        "About",
        
        fluidRow(
            column(2, ""),
            
            column(8, 
                   titlePanel("About"),
                   includeHTML("about.html")
            ),
            
            column(2, "")
        )
    )
    
)


# Server ------------------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$usa <- renderPlot({
        c <- county %>% 
            filter(Beneficiary == input$beneficiary_2,
                   Category == input$category_2)
        
        plot_usmap(data = c,
                   regions = "counties",
                   values = "Values",
                   color = "white") +
            scale_fill_continuous(low = "#DFF0EA", high = "#2A7886") +
            theme(legend.position = "bottom",
                  legend.direction = "horizontal")
        
    })
    
    output$state <- renderPlot({
        d <- county %>%
            filter(Beneficiary == input$beneficiary,
                   Category == input$category) 
        
        plot_usmap(data = d,
                   regions = "counties",
                   include = input$state,
                   values = "Values",
                   color = "white") +
            scale_fill_continuous(low = "#DFF0EA", high = "#2A7886") +
            theme(legend.position = "bottom",
                  legend.direction = "horizontal") + 
            labs(
                title = paste(input$beneficiary, "Beneficiaries in ", input$state),
                subtitle = paste("by ", input$category),
                fill = paste("Count of ", input$category)
            )
    },
    )
    
}

# Run the application
shinyApp(ui = ui, server = server)