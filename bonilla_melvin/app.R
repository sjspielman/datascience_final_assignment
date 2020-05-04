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
        navbarPage(theme = shinytheme("sandstone"),
                   title = "Worldwide Cases of Covid-19", 
            
            ## All UI for NYT goes in here:
            tabPanel("NYT data visualization", ## do not change this name
            
                    # All user-provided input for NYT goes in here:
                    sidebarPanel(
                        
                        colourpicker::colourInput("nyt_color_cases", "Color for plotting COVID cases:", value = "blue"),
                        colourpicker::colourInput("nyt_color_deaths", "Color for plotting COVID deaths:", value = "purple"), 
                        selectInput("which_state_nyt", ## input$which_state
                                    "Select a State/U.S Terirtory",
                                    choices=usa_states,
                                    selected = "Massachusetts"),
                        
                        radioButtons("facet_county_nyt",
                                     "Display Counties for Selected State/Territory",
                                     choices = c("No","Yes"),
                                     selected = "No"),
                        
                        selectInput("y_scale_nyt", ## input$y_scale
                                    "Select a Scale for Y-axis",
                                    choices=c("Linear","Log"),
                                    selected = "Linear"),
                        
                        selectInput("which_theme_nyt", ##input$which_theme
                                    "Select a Theme to Use",
                                    choices=c("Classic","Light","Dark","Minimal"),
                                    selected = "Classic")
                        
                    ), # closes NYT sidebarPanel. Note: we DO need a comma here, since the next line opens a new function     
                    
                    # All output for NYT goes in here:
                    mainPanel(
                        plotOutput("nyt_plot",height="600px")
                    ) # closes NYT mainPanel. Note: we DO NOT use a comma here, since the next line closes a previous function  
            ), # closes tabPanel for NYT data
            
            
            ## All UI for JHU goes in here:
            tabPanel("JHU data visualization", ## do not change this name
                     
                     # All user-provided input for JHU goes in here:
                     sidebarPanel(

                         colourpicker::colourInput("jhu_color_cases", "Color for plotting COVID cases:", value = "#0064A6"),
                         colourpicker::colourInput("jhu_color_deaths", "Color for plotting COVID deaths:", value = "black"),
                         selectInput("which_country_jhu", 
                                     "Select a Country/Region",
                                     choices=world_countries_regions,
                                     selected = "China"),
                         
                         
                         selectInput("y_scale_jhu", 
                                     "Select a Scale for Y-axis",
                                     choices=c("Linear","Log"),
                                     selected = "Linear"),
                         
                         selectInput("which_theme_jhu", 
                                     "Select a Theme to Use",
                                     choices=c("Classic","Light","Dark","Minimal"),
                                     selected = "Classic")
                         
                     ), # closes JHU sidebarPanel     
                     
                     # All output for JHU goes in here:
                     mainPanel(
                        plotOutput("jhu_plot",height="600px")
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
            filter(state==input$which_state_nyt)->nyt_state
            
        if(input$facet_county_nyt=="No"){
        nyt_state %>%
            group_by(date,covid_type) %>%
            summarize(y=sum(cumulative_number))->final_nyt_state
        
    }
    if(input$facet_county_nyt=="Yes"){
        nyt_state %>%
            rename(y=cumulative_number)->final_nyt_state
    }
        final_nyt_state
})
    
    ## Define your renderPlot({}) for NYT panel that plots the reactive variable. ALL PLOTTING logic goes here.
    output$nyt_plot <- renderPlot({
        nyt_data_subset() %>%
            ggplot(aes(x=date, y=y, color=covid_type,group=covid_type))+
            geom_point()+
            geom_line()+
            scale_color_manual(values=c(input$nyt_color_cases, input$nyt_color_deaths))+
            labs(title=paste(input$which_state_nyt,"Cases and Deaths Over Time"))+
            labs(x="Date",y="Total Amount",color="Category") ->Output_NYT
       
        ## Choice for Y-Scale
         if (input$y_scale_nyt=="Log"){
            Output_NYT<- Output_NYT + scale_y_log10()
         }
        
        ## Choice for Theme
        if (input$which_theme_nyt == "Classic") Output_NYT<- Output_NYT +theme_classic()
        if (input$which_theme_nyt == "Light") Output_NYT<- Output_NYT +theme_light()
        if (input$which_theme_nyt == "Dark") Output_NYT<- Output_NYT +theme_dark()
        if (input$which_theme_nyt == "Minimal") Output_NYT<- Output_NYT +theme_minimal()
    
        ##Choice of facet by counties
        if(input$facet_county_nyt=="Yes") Output_NYT<-Output_NYT+facet_wrap(~county, scales="free_y")+ 
                theme(axis.text.x = element_text(angle = 90))
        
        #Returned Result
        Output_NYT
        
    })
    
    
    
    
    ## All server logic for JHU goes here ------------------------------------------

    
    ## Define a reactive for subsetting the JHU data
    jhu_data_subset <- reactive({
        jhu_data %>% 
            filter(`Country/Region`==input$which_country_jhu) %>%
            group_by(`Country/Region`,date,covid_type) %>%
            summarize(y=sum(cumulative_number))->jhu_country
            
        jhu_country
        
    })
    
    ## Define your renderPlot({}) for JHU panel that plots the reactive variable. ALL PLOTTING logic goes here.
    output$jhu_plot <- renderPlot({
        jhu_data_subset() %>%
            ggplot(aes(x=date, y=y, color=covid_type,group=covid_type))+
            geom_point()+
            geom_line()+
            scale_color_manual(values=c(input$jhu_color_cases, input$jhu_color_deaths))+
            labs(title=paste(input$which_country_jhu,"Cases and Deaths Over Time"))+
            labs(x="Date",y="Total Amount",color="Category") ->Output_JHU
        
        ## Choice for Y-Scale
        if (input$y_scale_jhu=="Log"){
            Output_JHU<- Output_JHU + scale_y_log10()
        }
        
        ## Choice for Theme
        if (input$which_theme_jhu == "Classic") Output_JHU<- Output_JHU +theme_classic()
        if (input$which_theme_jhu == "Light") Output_JHU<- Output_JHU +theme_light()
        if (input$which_theme_jhu == "Dark") Output_JHU<- Output_JHU +theme_dark()
        if (input$which_theme_jhu == "Minimal") Output_JHU<- Output_JHU +theme_minimal()
        
    
        
        #Returned Result
        Output_JHU
        
        
        })
    
}





# Do not touch below this line! ----------------------------------
shinyApp(ui = ui, server = server)
