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
library(colourpicker)

source("covid_data_load.R") ## This line runs the Rscript "covid_data_load.R", which is expected to be in the same directory as this shiny app file!
# The variables defined in `covid_data_load.R` are how fully accessible in this shiny app script!!

# UI --------------------------------
ui <- shinyUI(
        navbarPage(theme = shinytheme("superhero"), 
                   title = "My Final Senior Project on Covid: thanks 2020", 
                   
            
            ## All UI for NYT goes in here:
            tabPanel("NYT data visualization", 
            
                    # All user-provided input for NYT goes in here:
                    sidebarPanel(
                        
                        colourpicker::colourInput("nyt_color_cases", "Color for plotting COVID cases:", value = "deeppink"),
                        colourpicker::colourInput("nyt_color_deaths", "Color for plotting COVID deaths:", value = "darkorchid"),
                        
                        #for the audience to select their state 
                        selectInput("state_choice", ##input$state_choice
                                    "Select a State to View",
                                    choices = usa_states,
                                    selected = "North Carolina"),
                        
                        #for audiences to select if they want to see pooled counties, yes or no
                        radioButtons("facet_county",
                                     "Choose to view pooled counties",
                                     choices = c("No", "Yes"),
                                     selected = "No"),
                        
                        #for the audience to determine how they view the graph 
                        radioButtons("y_scale",
                                     "Choose the scale for the y-axis",
                                     choices = c("Linear", "Log"),
                                     selected = "Linear"),
                        
                        #for the audience to choose their theme of view 
                        selectInput("theme_choice",
                                    "Choose a ggplot theme to view",
                                    choices = c("Grey","Light", "Classic", "Minimal"),
                                    selected = "Grey")
                        
                    ), # closes NYT sidebarPanel. Note: we DO need a comma here, since the next line opens a new function     
                    
                    # All output for NYT goes in here:
                    mainPanel(
                        plotOutput("nyt_plot")
                    ) # closes NYT mainPanel. Note: we DO NOT use a comma here, since the next line closes a previous function  
            ), # closes tabPanel for NYT data
            
            
            ## All UI for JHU goes in here:
            tabPanel("JHU data visualization", 
                     
                     # All user-provided input for JHU goes in here:
                     sidebarPanel(

                         colourpicker::colourInput("jhu_color_cases", "Color for plotting COVID cases:", value = "midnightblue"),
                         colourpicker::colourInput("jhu_color_deaths", "Color for plotting COVID deaths:", value = "springgreen4")
                         
                     ), # closes JHU sidebarPanel     
                     
                     # All output for JHU goes in here:
                     mainPanel(
                        plotOutput("jhu_plot")
                     ) # closes JHU mainPanel     
            ) # closes tabPanel for JHU data
    ) # closes navbarPage
) # closes shinyUI

# Server --------------------------------
server <- function(input, output, session) {

    ## PROTIP!! Don't forget, all reactives and outputs are enclosed in ({}). Not just parantheses or curly braces, but BOTH! Parentheses on the outside.
    
    

    
    ## All server logic for NYT goes here ------------------------------------------
    
    ## Define a reactive for subsetting the NYT data
    nyt_data_subset <- reactive({
        
        nyt_data %>%
            filter(state == input$state_choice) -> nyt_state
        
        if(input$facet_county == "No"){
            
            #combine county data to get single point per for cases/deaths
            nyt_state %>%
                group_by(date, covid_type) %>%
                summarize(y = sum(cumulative_number)) -> final_nyt_state
        }
        if(input$facet_county == "Yes"){
            nyt_state %>%
                rename(y=cumulative_number) -> final_nyt_state
        }
        final_nyt_state
            
    })
    
    ## Define your renderPlot({}) for NYT panel that plots the reactive variable. ALL PLOTTING logic goes here.
    output$nyt_plot <- renderPlot({
        nyt_data_subset() %>%
            ggplot(aes(x = date, y = y, color = covid_type, group = covid_type))+
                 geom_point()+
                 geom_line()+
                scale_color_manual(values = c(input$nyt_color_cases, input$nyt_color_deaths))+
             labs(title = paste(input$state_choice, "cases and deaths")) ->my_nytplot
        
        
        ##Option for Input$y_scale choice
        if (input$y_scale == "Log") {
            my_nytplot <- my_nytplot + scale_y_log10()
        }
        
        #Code to differienate theme choice 
        if(input$theme_choice == "Grey")my_nytplot <-my_nytplot +theme_grey()
        if(input$theme_choice == "Light")my_nytplot <-my_nytplot +theme_light()
        if(input$theme_choice == "Classic")my_nytplot <-my_nytplot +theme_classic()
        if(input$theme_choice == "Minimal")my_nytplot <-my_nytplot +theme_minimal()
        
        
        
        #Code for the plot to be plotted with either Linear or Log scale on y-axis
        my_nytplot + theme(legend.position = "bottom", #position for the plot label 
        axis.text = element_text(size = 13, color = "gray0", face = "bold")) #enhancing plot theme for easier viewing 
        
        
    })
    
  
    
    
    ## All server logic for JHU goes here ------------------------------------------

    
    
    ## Define a reactive for subsetting the JHU data
    jhu_data_subset <- reactive({})
    #had one last bug in my code, code found in untitled tab, didn't include here because it stopped the app from running - m 
    
    ## Define your renderPlot({}) for JHU panel that plots the reactive variable. ALL PLOTTING logic goes here.
    output$jhu_plot <- renderPlot({})
    
}





# Do not touch below this line! ----------------------------------
shinyApp(ui = ui, server = server)
