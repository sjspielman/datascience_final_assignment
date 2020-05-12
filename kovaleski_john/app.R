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
        navbarPage(  theme = shinytheme("cyborg"), ### Uncomment the theme and choose your own favorite theme from these options: https://rstudio.github.io/shinythemes/
                   title = "The Kovaleski Covid-19 Tracker", ### Replace title with something reasonable
            
            ## All UI for NYT goes in here:
            tabPanel("NYT data visualization", ## do not change this name
            
                    # All user-provided input for NYT goes in here:
                    sidebarPanel(
                        
                        colourpicker::colourInput("nyt_color_cases", "Color for plotting COVID cases:", value = "navy"),
                        colourpicker::colourInput("nyt_color_deaths", "Color for plotting COVID deaths:", value = "firebrick"),
                        selectInput("which_state",
                                    "Which state would you like to plot?",
                                    choices = usa_states,
                                    selected = "New Jersey"),
                        radioButtons("facet_county",
                                     "Show individual counties by faceting?",
                                     choices = c("No","Yes"),
                                     selected = "No"),
                        radioButtons("y_scale",
                                     "Scale for Y-axis?",
                                     choices = c("Linear","Log"),
                                     selected = "Linear"),
                        selectInput("which_theme",
                                    "Which theme would you like to use?",
                                    choices = c("Classic", "Minimal", "Light", "Dark"),
                                    selected = "Classic")
                        
                    ), # closes NYT sidebarPanel. Note: we DO need a comma here, since the next line opens a new function     
                    
                    # All output for NYT goes in here:
                    mainPanel(
                        plotOutput("nyt_plot", height = "800px")
                    ) # closes NYT mainPanel. Note: we DO NOT use a comma here, since the next line closes a previous function  
            ), # closes tabPanel for NYT data
            
            
            ## All UI for JHU goes in here:
            tabPanel("JHU data visualization", ## do not change this name
                     
                     # All user-provided input for JHU goes in here:
                     sidebarPanel(

                         colourpicker::colourInput("jhu_color_cases", "Color for plotting COVID cases:", value = "pink"),
                         colourpicker::colourInput("jhu_color_deaths", "Color for plotting COVID deaths:", value = "green"),
                         selectInput("which_country",
                                     "Which country would you like to plot?",
                                     choices = world_countries_regions,
                                     selected = "US"),
                         radioButtons("y_scale_jhu",
                                      "Scale for Y-axis?",
                                      choices = c("Linear","Log"),
                                      selected = "Linear"),
                        # dateRangeInput("dateRange_jhu",
                       #                 "Select Date Range",
                         #               start = "2020-01-22",
                       #                 end   = Sys.Date()-2),
                         selectInput("which_theme_jhu",
                                     "Which theme would you like to use?",
                                     choices = c("Classic", "Minimal", "Light", "Dark"),
                                     selected = "Dark")
                         
                     ), # closes JHU sidebarPanel     
                     
                     # All output for JHU goes in here:
                     mainPanel(
                        plotOutput("jhu_plot", height = "800px")
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
            
            nyt_state %>%
                group_by(date,covid_type) %>%
                summarize(y = sum(cumulative_number)) -> final_nyt_state
        }
        if(input$facet_county == "Yes"){
            nyt_state %>%
                rename(y = cumulative_number) -> final_nyt_state
        }
        
        final_nyt_state
        
    }) 
    
    ## Define your renderPlot({}) for NYT panel that plots the reactive variable. ALL PLOTTING logic goes here.
    output$nyt_plot <- renderPlot({
        nyt_data_subset() %>%
            ggplot(aes(x=date, y = y, color = covid_type, group = covid_type)) +
            geom_point() +
            geom_line() +
            scale_color_manual(values = c(input$nyt_color_cases, input$nyt_color_deaths)) +
            labs(title = paste(input$which_state, "cases and deaths"), x = "Date", color = "Covid-19 Type", y= "Count") -> myplot_nyt
        #Choices for y scale
        if(input$y_scale == "Log"){
            myplot_nyt <- myplot_nyt + scale_y_log10()
        }
        
        #faceting counties
        if(input$facet_county == "Yes") myplot_nyt <- myplot_nyt + facet_wrap(~county)
        
        
        # Choices for theme MAKE SURE THERES 4
        if(input$which_theme == "Classic") myplot_nyt <- myplot_nyt + theme_classic()
        if(input$which_theme == "Minimal") myplot_nyt <- myplot_nyt + theme_minimal()
        if(input$which_theme == "Light") myplot_nyt <- myplot_nyt + theme_light()
        if(input$which_theme == "Dark") myplot_nyt <- myplot_nyt + theme_dark()
        
        myplot_nyt + theme(legend.position = "bottom")
        
    })
    
    
    
    
    ## All server logic for JHU goes here ------------------------------------------

    
    ## Define a reactive for subsetting the JHU data
    jhu_data_subset <- reactive({
        
        jhu_data %>%
            filter(country_or_region == input$which_country) -> jhu_country
        
        jhu_country %>%
            group_by(date, covid_type) %>%
            summarize(y = sum(cumulative_number)) -> final_jhu_country
        
        final_jhu_country
        
        
    })
    
    ## Define your renderPlot({}) for JHU panel that plots the reactive variable. ALL PLOTTING logic goes here.
    output$jhu_plot <- renderPlot({
    
    
        jhu_data_subset()%>%
            ggplot(aes(x=date, y= y, color= covid_type, group= covid_type))+
            geom_point() +
            geom_line() +
            scale_color_manual(values= c(input$jhu_color_cases, input$jhu_color_deaths))+
            labs(title = paste(input$which_country, "cases and deaths"), x = "Date", color = "Covid-19 Type", y= "Count") -> myplot_jhu
        
        if(input$y_scale_jhu == "Log"){
            myplot_jhu <- myplot_jhu + scale_y_log10()
        }
        if(input$which_theme_jhu == "Classic") myplot_jhu <- myplot_jhu + theme_classic()
        if(input$which_theme_jhu == "Minimal") myplot_jhu <- myplot_jhu + theme_minimal()
        if(input$which_theme_jhu == "Light") myplot_jhu <- myplot_jhu + theme_light()
        if(input$which_theme_jhu == "Dark") myplot_jhu <- myplot_jhu + theme_dark()

        
        myplot_jhu
        
        
        
        
        
    })
    
}





# Do not touch below this line! ----------------------------------
shinyApp(ui = ui, server = server)
