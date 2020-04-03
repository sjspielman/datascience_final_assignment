#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Load all libraries ------------------------------------------------------------------------
library(shiny)
library(shinythemes)
library(tidyverse)
library(viridis)

source("covid_data_load.R") ## This line runs the Rscript "covid_data_load.R", which is expected to be in the same directory as this shiny app file!
# The variables defined in `covid_data_load.R` are how fully accessible in this shiny app script!!

# UI --------------------------------
ui <- shinyUI(
        navbarPage(theme = shinytheme("flatly"), ### Please choose your own favorite theme from these options: https://rstudio.github.io/shinythemes/
                   title = "YOUR VERY INTERESTING TITLE", ### Replace title with something reasonable
            
            ## All UI for NYT goes in here:
            tabPanel("NYT data visualization", ## do not change this name
            
                    # All user-provided input for NYT goes in here:
                    sidebarPanel(
                        
                        selectInput(inputID  = "nyt_viridis_scheme", # name for internal use as variable: input$nyt_viridis_scheme
                                    label    = "What viridis color scheme should be used?", # label that users see
                                    choices  = viridis_scheme_options,  # defined in covid_load_data.R
                                    selected = "viridis")              # default color scheme. 
                        
    
                    ), # closes NYT sidebarPanel. Note: we DO need a comma here, since the next line opens a new function     
                    
                    # All output for NYT goes in here:
                    mainPanel(
                        
                    )# closes NYT mainPanel. Note: we don't need a comma here, since the next line closes a previous function  
            ), # closes tabPanel for NYT data
            
            
            ## All UI for JHU goes in here:
            tabPanel("JHU data visualization", ## do not change this name
                     
                     # All user-provided input for JHU goes in here:
                     sidebarPanel(

                         selectInput(inputID  = "jhu_viridis_scheme", # name for internal use as variable: input$jhu_viridis_scheme
                                     label    = "What viridis color scheme should be used?", # label that users see
                                     choices  = viridis_scheme_options,  # defined in covid_load_data.R
                                     selected = "viridis")              # default color scheme. 
                         
                     ), # closes JHU sidebarPanel     
                     
                     # All output for JHU goes in here:
                     mainPanel(
                         
                     )# closes JHU mainPanel     
            ), # closes tabPanel for JHU data
    ) # closes navbarPage
) # closes shinyUI

# Server --------------------------------
server <- function(input, output) {

    ## PROTIP!! Don't forget, all reactives and outputs are enclosed in ({}). Not just parantheses or curly braces, but BOTH! Parentheses on the outside.
    
    

    
    ## All server logic for NYT goes here ------------------------------------------
    
    
    
    ## All server logic for JHU goes here ------------------------------------------
    
}





# Do not touch below this line! ----------------------------------
shinyApp(ui = ui, server = server)
