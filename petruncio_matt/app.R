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
        navbarPage( theme = shinytheme("flatly"), ### Uncomment the theme and choose your own favorite theme from these options: https://rstudio.github.io/shinythemes/
                   title = "YOUR VERY INTERESTING TITLE", ### Replace title with something reasonable
            
            ## All UI for NYT goes in here:
            tabPanel("NYT data visualization", ## do not change this name
            
                    # All user-provided input for NYT goes in here:
                    sidebarPanel(
                        
                        colourpicker::colourInput("nyt_color_cases", "Color for plotting COVID cases:", value = "blue"),
                        colourpicker::colourInput("nyt_color_deaths", "Color for plotting COVID deaths:", value = "red"),
                        selectInput("which_state", ##input$which_state
                                    "which state plot?",
                                    choices = usa_states,
                                    selected = "Pennsylvania"),
                        radioButtons("facet_county",
                                     "Show counties across panels or pool?",
                                     choices = c("No","Yes"),
                                     selected = "No"),
                        radioButtons("y_scale",
                                     "Scale for Y-axis?",
                                     choices = c("Linear","Log"),
                                     selected = "Linear"),
                        selectInput("which_theme",
                                    "which ggplot theme to use?",
                                    choices = c("Classic","Minimal"),
                                    selected = "Classic")
                        
                    ), # closes NYT sidebarPanel. Note: we DO need a comma here, since the next line opens a new function     
                    
                    # All output for NYT goes in here:
                    mainPanel(
                        plotOutput("nyt_plot", height = "1000px")
                    ) # closes NYT mainPanel. Note: we DO NOT use a comma here, since the next line closes a previous function  
            ), # closes tabPanel for NYT data
            
            
            ## All UI for JHU goes in here:
            tabPanel("JHU data visualization", ## do not change this name
                     
                     # All user-provided input for JHU goes in here:
                     sidebarPanel(

                         colourpicker::colourInput("jhu_color_cases", "Color for plotting COVID cases:", value = "purple"),
                         colourpicker::colourInput("jhu_color_deaths", "Color for plotting COVID deaths:", value = "orange")
                         
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
            filter(state == input$which_state) -> nyt_state

        if(input$facet_county == "No"){
            #combine county data to get single point per day for case/death
            group_by(date, covid_type) %>%
            summarize(y = sum(cumulative_number))->final_nyt_state
    }
    if(input$facet_county == "Yes"){
    nyt_state %>%
            rename(y= cumulative_number)->final_nyt_state
    }

final_nyt_state
    })
    
    ## Define your renderPlot({}) for NYT panel that plots the reactive variable. ALL PLOTTING logic goes here.
    output$nyt_plot <- renderPlot({
         nyt_data_subset()%>%
            ggplot(aes(x=date, y=y, color=covid_type,group=covid_type))+
            geom_point()+
            geom_line()+
            scale_color_manual=c(input$nyt_color_cases, input$nyt_color_deaths)->myplot
            if (input$y_scale == "Log"){
                myplot<-myplot+scale_y_log10
            }
            #deal with pnput$facetcounty
            if (input$facet_county == "Yes") myplot <- myplot +facet_wrap(~county)
            #deal with input$which_theme
            if (input$which_theme == "Classic") myplot <- myplot + theme_classic()
            if (input$which_theme == "Minimal") myplot <- myplot + theme_minimal()
            #return plot
            myplot + theme(legend.position = "bottom")
    })
    
    #selectInput("which_theme",
     #           "which ggplot theme to use?",
     #           choices = c("Classic","Minimal"),
      #          selected = "Classic")
    
    
    ## All server logic for JHU goes here ------------------------------------------

    
    ## Define a reactive for subsetting the JHU data
    jhu_data_subset <- reactive({})
    
    ## Define your renderPlot({}) for JHU panel that plots the reactive variable. ALL PLOTTING logic goes here.
    jhu_plot <- renderPlot({
      nyt_data%>%
        filter(state =="Alabama")%>%
        group_by(date, covid_type)%>%
        summarize(total_county_day=sum(cumulative_number))%>%
        ggplot(aes(x=date, y=total_county_day, color=
                     covid_type, group=covid_type))+
        geom_point()+
        geom_line()+
        scale_color_manual= c(input$nyt_color_cases, input$nyt_color_deaths)
        
    })
    
}





# Do not touch below this line! ----------------------------------
shinyApp(ui = ui, server = server)
