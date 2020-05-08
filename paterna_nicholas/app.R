#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Load all libraries -----------------
library(shiny)
library(shinythemes)
library(tidyverse)
library(colourpicker)
library(shinyWidgets)

source("covid_data_load.R") ## This line runs the Rscript "covid_data_load.R", which is expected to be in the same directory as this shiny app file!
# The variables defined in `covid_data_load.R` are how fully accessible in this shiny app script!!

# UI --------------------------------
ui <- shinyUI(
    navbarPage(theme = shinytheme("yeti"), 
               title = "Visualizing COVID-19",
               
               ## All UI for NYT goes in here:
               tabPanel("NYT data visualization", ## do not change this name
                        
                        # All user-provided input for NYT goes in here:
                        sidebarPanel(
                            
                            colourpicker::colourInput("nyt_color_cases",
                                                      "Color for plotting COVID cases:",
                                                      value = "#A62D45"),
                            
                            colourpicker::colourInput("nyt_color_deaths",
                                                      "Color for plotting COVID deaths:",
                                                      value = "#495BE3"),
                            
                            
                            pickerInput(inputId = "which_state",
                                label = "Which state would you like to plot?", 
                                choices = usa_states,
                                options = list(style = "btn-primary")),
                            
                            radioGroupButtons(
                                inputId = "facet_county",
                                label = "Show counties across panels?",
                                choices = c("No", "Yes"),
                                individual = TRUE,
                                checkIcon = list(
                                    yes = tags$i(class = "fa fa-circle"),
                                    no = tags$i(class = "fa fa-circle-o"))),
                            
                            
                            radioGroupButtons(
                                inputId = "y_scale",
                                label = "Scale for Y-Axis?",
                                choices = c("Linear", "Log"),
                                individual = TRUE,
                                checkIcon = list(
                                    yes = tags$i(class = "fa fa-circle"),
                                    no = tags$i(class = "fa fa-circle-o"))),
                            
                            selectInput("which_theme",
                                        "Choose a ggplot theme to use:",
                                        choices = c("Black & White",
                                                    "Classic",
                                                    "Dark",
                                                    "Minimal"),
                                        selected = "Classic") ## input$which_theme
                            
                        ), # closes NYT sidebarPanel. Note: we DO need a comma here, since the next line opens a new function     
                        
                        # All output for NYT goes in here:
                        mainPanel(
                            plotOutput("nyt_plot", height = "650px")
                        ) # closes NYT mainPanel. Note: we DO NOT use a comma here, since the next line closes a previous function  
               ), # closes tabPanel for NYT data
               
               
               ## All UI for JHU goes in here:
               tabPanel("JHU data visualization", ## do not change this name
                        
                        # All user-provided input for JHU goes in here:
                        sidebarPanel(
                            
                            colourpicker::colourInput("jhu_color_cases",
                                            "Color for plotting COVID cases:",
                                                      value = "#E09F12"),
                            
                            colourpicker::colourInput("jhu_color_deaths",
                                            "Color for plotting COVID deaths:",
                                                      value = "#7D35BD"),
                            
                            pickerInput(inputId = "which_country",
                            "Which country or region would you like to plot?", 
                                        choices = world_countries_regions,
                                        options = list(style = "btn-primary")),
                            
                            radioGroupButtons(
                                inputId = "jhu_y_scale",
                                label = "Scale for Y-Axis?",
                                choices = c("Linear", "Log"),
                                individual = TRUE,
                                checkIcon = list(
                                    yes = tags$i(class = "fa fa-circle"),
                                    no = tags$i(class = "fa fa-circle-o"))),
                            
                            selectInput("jhu_theme",
                                        "Choose a ggplot theme to use:",
                                        choices = c("Black & White",
                                                    "Classic",
                                                    "Dark",
                                                    "Minimal"),
                                        selected = "Classic")
                            
                        ), # closes JHU sidebarPanel     
                        
                        # All output for JHU goes in here:
                        mainPanel(
                            plotOutput("jhu_plot", height = "500px")
                        ) # closes JHU mainPanel     
               ) # closes tabPanel for JHU data
    ) # closes navbarPage
) # closes shinyUI

# Server --------------------------------
server <- function(input, output, session) {
    
    ## PROTIP!! Don't forget, all reactives and outputs are enclosed in ({}). Not just parantheses or curly braces, but BOTH! Parentheses on the outside.
    
    
## All server logic for NYT goes here -------------------------------------
    
    ## Define a reactive for subsetting the NYT data
    nyt_data_subset <- reactive({  
        
        nyt_data %>%
            filter(state == input$which_state) -> nyt_state
        
        if (input$facet_county == "No"){
            ## Combine county data to get single point per day for case / death
            nyt_state %>%
                group_by(date, covid_type) %>%
                summarize(y = sum(cumulative_number)) -> final_nyt_state
        }
        
        if (input$facet_county == "Yes"){
            nyt_state %>%
                rename(y = cumulative_number) -> final_nyt_state
        }
        
        final_nyt_state
        
    })
    
    ## Define your renderPlot({}) for NYT panel that plots the reactive variable. ALL PLOTTING logic goes here.
    output$nyt_plot <- renderPlot({
        nyt_data_subset() %>%
            ggplot(aes(x = date,
                       y = y,
                       color = covid_type,
                       group = covid_type)) +
            geom_point() + 
            geom_line() +
            scale_color_manual(values = c(
                input$nyt_color_cases,
                input$nyt_color_deaths)) +
            labs(title = paste(input$which_state, "Cases & Deaths"),
                 x = "Date",
                 y = "Cumulative Count",
                 color = "Type") -> myplot
        
        ## Deal with input$y_scale choice
        if (input$y_scale == "Log"){
            myplot <- myplot + scale_y_log10()
        }
        
        ## Deal with input$facet_county
        if (input$facet_county == "Yes") myplot <- myplot + facet_wrap(~county, scales = "free_y")
        
        ### Deal with input$which_theme choice
        if (input$which_theme == "Black & White") myplot <- myplot + theme_bw()
        if (input$which_theme == "Classic") myplot <- myplot + theme_classic()
        if (input$which_theme == "Dark") myplot <- myplot + theme_dark()
        if (input$which_theme == "Minimal") myplot <- myplot + theme_minimal()
        
        ## Return the plot to be plotted
        myplot + theme(axis.text = element_text(size = 12),
                       axis.title = element_text(size = 14,
                                                 face = "bold"),
                       plot.title = element_text(size = 20,
                                                 face = "bold",
                                                 hjust = 0.5))
    })
    
    
## All server logic for JHU goes here -------------------------------------
    
    ## Define a reactive for subsetting the JHU data
    jhu_data_subset <- reactive({
        
        jhu_data %>%
            filter(country_or_region == input$which_country) -> jhu_country
        
        jhu_country
    })
    
    ## Define your renderPlot({}) for JHU panel that plots the reactive variable. ALL PLOTTING logic goes here.
    output$jhu_plot <- renderPlot({
        jhu_data_subset() %>%
            ggplot(aes(x = date,
                       y = cumulative_number,
                       color = covid_type,
                       group = covid_type)) +
            geom_point() + 
            geom_line() +
            scale_color_manual(values = c(
                input$jhu_color_cases,
                input$jhu_color_deaths)) +
            labs(title = paste(input$which_country, "Cases & Deaths"),
                 x = "Date",
                 y = "Cumulative Count",
                 color = "Type") -> my_jhu_plot
        
        
        ## Deal with input$y_scale choice
        if (input$jhu_y_scale == "Log"){
            my_jhu_plot <- my_jhu_plot + scale_y_log10()
        }
        
        ## Deal with input$jhu_theme
        if (input$jhu_theme == "Black & White") my_jhu_plot <- my_jhu_plot + theme_bw()
        if (input$jhu_theme == "Classic") my_jhu_plot <- my_jhu_plot + theme_classic()
        if (input$jhu_theme == "Dark") my_jhu_plot <- my_jhu_plot + theme_dark()
        if (input$jhu_theme == "Minimal") my_jhu_plot <- my_jhu_plot + theme_minimal()
        
        ## Return the plot to be plotted
        my_jhu_plot + theme(axis.text = element_text(size = 12),
                            axis.title = element_text(size = 14,
                                                      face = "bold"),
                            plot.title = element_text(size = 20,
                                                      face = "bold",
                                                      hjust = 0.5))
    })
    
}

# Do not touch below this line! ----------------------------------
shinyApp(ui = ui, server = server)