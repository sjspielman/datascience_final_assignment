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

source("covid_data_load.R") ## This line runs the Rscript "covid_data_load.R", which is expected to be in the same directory as this shiny app file! #gives us full access to data in .R 
# The variables defined in `covid_data_load.R` are how fully accessible in this shiny app script!!

# UI --------------------------------
ui <- shinyUI(
        navbarPage( theme = shinytheme("simplex"), ### Uncomment the theme and choose your own favorite theme from these options: https://rstudio.github.io/shinythemes/
                   title = "YOUR VERY INTERESTING TITLE", ### Replace title with something reasonable
            
            ## All UI for NYT goes in here:
            tabPanel("NYT data visualization", ## do not change this name
            
                    # All user-provided input for NYT goes in here:
                    sidebarPanel(
                        
                        colourpicker::colourInput("nyt_color_cases", "Color for plotting COVID cases:", value = "pink"),
                        colourpicker::colourInput("nyt_color_deaths", "Color for plotting COVID deaths:", value = "blue"), 
                        
                    selectInput("which_state", 
                                "Which state would you like to plot?",
                                choices=usa_states, 
                                selected="Pennsylvania"), 
                    radioButtons("y_scale",
                                 "Y axis scale?", 
                                 choices=c("Linear","Log"), 
                                 selected="Linear"), 
                    selectInput("which_theme", 
                                "Which ggplot theme woulld youk like to use?",
                                choices=c("Classic", "Minimal", "Dark", "Grey"), 
                                selected="Classic"), 
                    
                    ),# closes NYT sidebarPanel. Note: we DO need a comma here, since the next line opens a new function     
                    
                    # All output for NYT goes in here:
                    mainPanel(
                        plotOutput("nyt_plot")
                    ) # closes NYT mainPanel. Note: we DO NOT use a comma here, since the next line closes a previous function  
            ), # closes tabPanel for NYT data
            
            
            ## All UI for JHU goes in here:
            tabPanel("JHU data visualization", ## do not change this name
                     
                     # All user-provided input for JHU goes in here:
                     sidebarPanel(

                         colourpicker::colourInput("jhu_color_cases", "Color for plotting COVID cases:", value = "pink"),
                         colourpicker::colourInput("jhu_color_deaths", "Color for plotting COVID deaths:", value = "blue"), 
                         selectInput("which_country", 
                                     "Which country or region would you like to plot?",
                                     choices=country_or_region, 
                                     selected="Canada"), 
                         radioButtons("y_scale",
                                      "Y axis scale?", 
                                      choices=c("Linear","Log"), 
                                      selected="Linear"), 
                         selectInput("which_theme", 
                                     "Which ggplot theme woulld youk like to use?",
                                     choices=c("Classic", "Minimal", "Dark", "Grey"), 
                                     selected="Classic"), 
                         
                         
                         
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
    nyt_data_subset <- reactive({ #output is stored in nyt_data_subst(a reactive ) 
        nyt_data%>%
            filter(state ==input$which_state)%>% #we want the user to be able to pick whatever state they want 
            group_by(date, covid_type)%>%
            summarize(total_county_day=sum(cumulative_number)) #we want to add up all the counties on each day 
    })
    
    ## Define your renderPlot({}) for NYT panel that plots the reactive variable. ALL PLOTTING logic goes here.
    output$nyt_plot <- renderPlot({
        nyt_data_subset()%>% #have to put () bc its a reactive
            ggplot(aes(x=date, y=total_county_day, color=covid_type, group=covid_type))+
            geom_point()+ 
            geom_line()+
            scale_color_manual(values =c(input$nyt_color_cases , input$nyt_color_deaths)) +
            labs(title=paste(input$which_state, "cases and deaths"), y="Cumulative count")->nyt_myplot
        if(input$y_scale=="Log"){
            nyt_myplot<-nyt_myplot+scale_y_log10() #if user picks log use log scale 
            
        }
        
        if(input$which_theme=="Classic"){
            nyt_myplot<-nyt_myplot+theme_classic()}
        if(input$which_theme=="Dark"){
            nyt_myplot<-nyt_myplot+theme_dark()}
        if(input$which_theme=="Minimal"){
            nyt_myplot<-nyt_myplot+theme_minimal()}
        if(input$which_theme=="Grey"){
                nyt_myplot<-nyt_myplot+theme_grey()}
        
        nyt_myplot+theme(text = element_text(size=16)) #change axis sizes 
        
    })
    
   
    
    ## All server logic for JHU goes here ------------------------------------------

    
    ## Define a reactive for subsetting the JHU data
    jhu_data_subset <- reactive({
         jhu_data%>%
            filter(country_or_region==input$which_country)%>% #we want the user to be able to pick whatever state they want 
            group_by(date, covid_type)%>%
            summarize(total_county_day=sum(cumulative_number))
        
        
        
    })
    
    ## Define your renderPlot({}) for JHU panel that plots the reactive variable. ALL PLOTTING logic goes here.
    output$jhu_plot <- renderPlot({
        jhu_data_subset()%>% #have to put () bc its a reactive
            ggplot(aes(x=date, y=total_county_day, color=covid_type, group=covid_type))+
            geom_point()+ 
            geom_line()+
            scale_color_manual(values =c(input$nyt_color_cases , input$nyt_color_deaths)) +
            labs(title=paste(input$which_country, "cases and deaths"), y="Cumulative count")->jhu_myplot
        if(input$y_scale=="Log"){
            jhu_myplot<-jhu_myplot+scale_y_log10() #if user picks log use log scale 
            
        }
        
        if(input$which_theme=="Classic"){
            nyt_myplot<-jhu_myplot+theme_classic()}
        if(input$which_theme=="Dark"){
            nyt_myplot<-jhu_myplot+theme_dark()}
        if(input$which_theme=="Minimal"){
            nyt_myplot<-jhu_myplot+theme_minimal()}
        if(input$which_theme=="Grey"){
            nyt_myplot<-jhu_myplot+theme_grey()}
        
        jhu_myplot+theme(text = element_text(size=16)) #change axis sizes 
        
        
        
        
    })
    
}





# Do not touch below this line! ----------------------------------
shinyApp(ui = ui, server = server)
