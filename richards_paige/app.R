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
themes_options <- c("Classic", "Minimal", "Grey", "Black and White", "Dark", "Light")

source("covid_data_load.R") ## This line runs the Rscript "covid_data_load.R", which is expected to be in the same directory as this shiny app file!
# The variables defined in `covid_data_load.R` are how fully accessible in this shiny app script!!
#source is super cool! You can use it for Rmarkdowns too! Keep this in mind for the future 

# UI --------------------------------
ui <- shinyUI(
        navbarPage(theme = shinytheme("cyborg"), ### Uncomment the theme and choose your own favorite theme from these options: https://rstudio.github.io/shinythemes/
                   title = "COVID-19 Shiny App", ### Replace title with something reasonable
            
            ## All UI for NYT goes in here:
            tabPanel("NYT data visualization", ## do not change this name
            
                    # All user-provided input for NYT goes in here:
                    sidebarPanel(
                        
                        colourpicker::colourInput("nyt_color_cases", "Color for plotting COVID cases:", value = "darkorchid"),
                        colourpicker::colourInput("nyt_color_deaths", "Color for plotting COVID deaths:", value = "darkturquoise"), #need comma because we are putting another one
                        selectInput("which_state", ## input$which_state,
                                    "What state would you like to see COVID-19 data for?",
                                    choices = usa_states,
                                    selected = "Wyoming"), #chose this because there is a meme that Wyoming does not exist, closes selectInput
                        radioButtons("facet_county",
                                     "Do you want to see all the Counties of this State individually or all pooled together?",
                                     choices = c("Individually", "Together"),
                                     selected = "Together"),
                        radioButtons("y_scale",
                                     "What scale do you want to see for the Y axis?",
                                     choices = c("Linear", "Log"),
                                     selected = "Linear"), #closes radio button
                        selectInput("which_theme",
                                    "What theme are you interested in seeing?",
                                    choices = themes_options)
                    ), # closes NYT sidebarPanel. Note: we DO need a comma here, since the next line opens a new function     
                    
                    # All output for NYT goes in here:
                    mainPanel(
                        plotOutput("nyt_plot", height = "1000px", width = "2000px")
                    ) # closes NYT mainPanel. Note: we DO NOT use a comma here, since the next line closes a previous function  
            ), # closes tabPanel for NYT data
            
            
            ## All UI for JHU goes in here:
            tabPanel("JHU data visualization", ## do not change this name
                     
                     # All user-provided input for JHU goes in here:
                     sidebarPanel(

                         colourpicker::colourInput("jhu_color_cases", "Color for plotting COVID cases:", value = "lightseagreen"),
                         colourpicker::colourInput("jhu_color_deaths", "Color for plotting COVID deaths:", value = "slateblue4")
                         
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
    nyt_data_subset <- reactive({nyt_data %>% #need to use this to make the data cleaner
            filter(state == input$which_state) -> nyt_state
        
        if(input$facet_county == "Together"){
            nyt_state %>% #can't have lines going down. and pooled county data together
            group_by(date, covid_type) %>%
            summarize(y = sum(cumulative_number)) -> final_nyt_state
        }
        
        if(input$facet_county == "Individually"){
            nyt_state %>%
                rename(y = cumulative_number) -> final_nyt_state
        }
        
        final_nyt_state
        }) #this closes nyt_data_subset
    
    ## Define your renderPlot({}) for NYT panel that plots the reactive variable. ALL PLOTTING logic goes here.
    output$nyt_plot <- renderPlot({
        nyt_data_subset() %>%
            ggplot(aes(x = date, y = y, color = covid_type, group = covid_type)) +
                geom_point() +
                geom_line() +
                scale_color_manual(values = c(input$nyt_color_cases, input$nyt_color_deaths)) +
            labs(x = "Date",
                 y = "Cumulative Count",
                 color = "Covid Type",
                 title = paste(input$which_state, "cases and deaths")) -> myploot
        
    ##If they decided to use log    
       if ( input$y_scale == "Log"){
           myploot <- myploot + scale_y_log10()
       } #closes if statement
    ##otherwise 
    
    ##Input for facet_county
        if(input$facet_county == "Individually") myploot <- myploot + facet_wrap(~county)
        
    ##Which theme choice
        if (input$which_theme == "Classic") myploot <- myploot + theme_classic()
        if (input$which_theme == "Minimal") myploot <- myploot + theme_minimal()
        if (input$which_theme == "Grey") myploot <- myploot + theme_grey()
        if (input$which_theme == "Black and White") myploot <- myploot + theme_bw()
        if (input$which_theme == "Dark") myploot <- myploot + theme_dark()
        if (input$which_theme == "Light") myploot <- myploot + theme_light()
        myploot + theme(axis.title = element_text(size = 15),
                        axis.text = element_text(size = 12),
                        plot.title = element_text(size = 30))
        
    })
    
 
    #radioButtons("facet_county",
                 #"Do you want to see all the Counties of this State individually or all pooled together?",
                 #choices = c("Individually", "Together"),
                 #selected = "Together")
    
    ## All server logic for JHU goes here ------------------------------------------

    
    ## Define a reactive for subsetting the JHU data
    jhu_data_subset <- reactive({})
    
    ## Define your renderPlot({}) for JHU panel that plots the reactive variable. ALL PLOTTING logic goes here.
    jhu_plot <- renderPlot({})
    
}





# Do not touch below this line! ----------------------------------
shinyApp(ui = ui, server = server)
