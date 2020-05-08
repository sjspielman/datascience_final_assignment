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
library(ggthemes)
library(shinyWidgets)
library(usmap)



source("covid_data_load.R") ## This line runs the Rscript "covid_data_load.R", which is expected to be in the same directory as this shiny app file!
# The variables defined in `covid_data_load.R` are how fully accessible in this shiny app script!!

# UI --------------------------------
ui <- shinyUI(
        navbarPage(theme = shinytheme("simplex"), #specific theme
                   title = "Covid-19 Visualizations", #unique title
            
            ## All UI for NYT goes in here:
            tabPanel("NYT data visualization", ## do not change this name
            
                    # All user-provided input for NYT goes in here:
                    sidebarPanel(
                        
                        colourpicker::colourInput("nyt_color_cases", "Color for plotting COVID cases:", value = "darkseagreen4"),
                        colourpicker::colourInput("nyt_color_deaths", "Color for plotting COVID deaths:", value = "coral2"),
                        selectInput("which_state", ##input$which_state
                                    "Which state would you like to plot?",
                                     choices = usa_states, 
                                     selected = "New Jersey"),
                        radioButtons("begin_100_nyt",
                                     "Start on the 100th case?",
                                     choices = c("Yes", "No"),
                                     selected = "No"),
                        radioButtons("facet_county", 
                                     "Show counties across panels, or pool all counties?",
                                     choices = c("Yes", "No"),
                                     selected = "No"), 
                        radioButtons("y_scale_nyt", 
                                     "Scale for Y-axis",
                                     choices = c("Linear", "Log"),
                                     selected = "Linear"),
                        selectInput("which_theme_nyt", 
                                    "Which ggplot theme to use?",
                                    choices = c("Classic", "Minimal", "Solarized","Economist", "Tufte"), 
                                    selected = "Classic")
                        
                    ), # closes NYT sidebarPanel. Note: we DO need a comma here, since the next line opens a new function     
                    
                    # All output for NYT goes in here:
                    mainPanel(
                        plotOutput("nyt_plot", height = "600px")
                    ) # closes NYT mainPanel. Note: we DO NOT use a comma here, since the next line closes a previous function  
            ), # closes tabPanel for NYT data
            
            
            ## All UI for JHU goes in here:
            tabPanel("JHU data visualization", ## do not change this name
                     
                     # All user-provided input for JHU goes in here:
                     sidebarPanel(

                         colourpicker::colourInput("jhu_color_cases", "Color for plotting COVID cases:", value = "darkslategray4"),
                         colourpicker::colourInput("jhu_color_deaths", "Color for plotting COVID deaths:", value = "salmon3"),
                         selectInput("which_country", ##input$which_country
                                     "Which Country or Region would you like to plot?",
                                     choices = world_countries_regions, 
                                     selected = "Italy"),
                         radioButtons("begin_100_jhu",
                                      "Start on the 100th case?",
                                      choices = c("Yes", "No"),
                                      selected = "No"),
                         radioButtons("y_scale_jhu", 
                                      "Scale for Y-axis",
                                      choices = c("Linear", "Log"),
                                      selected = "Linear"),
                         selectInput("which_theme_jhu", 
                                     "Which ggplot theme to use?",
                                     choices = c("Classic", "Minimal", "Solarized", "Economist", "Tufte"), 
                                     selected = "Classic")

                    
                         
                     ), # closes JHU sidebarPanel     
                     
                     # All output for JHU goes in here:
                     mainPanel(
                        plotOutput("jhu_plot", height = "600px")
                     ) # closes JHU mainPanel     
            ),# closes tabPanel for JHU data
            
            ## All UI for Map of USA goes in here:
            tabPanel("Map of USA",
                     
                     # All user-provided input for MAP goes in here:
                     sidebarPanel(
                         radioButtons("case_death",
                                      "Do you want to display cases or deaths?",
                                      choices = c("Cases", "Deaths"),
                                      selected = "Cases")
                         
                     ),
            
                     # All output for MAP goes in here:
                     mainPanel(
                         plotOutput("map_plot")
                     ) # closes MAP mainPanel     
            )# closes tabPanel for MAP data
            
        
            
    ) # closes navbarPage
) # closes shinyUI

# Server --------------------------------
server <- function(input, output, session) {

    ## PROTIP!! Don't forget, all reactives and outputs are enclosed in ({}). Not just parantheses or curly braces, but BOTH! Parentheses on the outside.
    
    

    
    ## All server logic for NYT goes here ------------------------------------------
    
    ## Define a reactive for subsetting the NYT data
    nyt_data_subset <- reactive({
        
        #choice of state
         nyt_data %>%
            filter(state == input$which_state) -> nyt_state
        
        #option for starting x on 100th day
        if (input$begin_100_nyt == "No"){
            nyt_state %>%
                rename(x = date) -> final_nyt
        }
        
        if (input$begin_100_nyt == "Yes"){
            nyt_state %>%
                pivot_wider(names_from = covid_type, values_from = cumulative_number) %>% 
                filter(cases >= 100) %>%
                pivot_longer(c(cases, deaths), names_to = "covid_type", values_to = "cumulative_number") %>%
                rename(x = date) -> final_nyt  
        }
        
        
          if(input$facet_county == "No"){
            #combine county data to get single point per day for case/death
            final_nyt %>%
                group_by(x, covid_type) %>%
                summarize(y = sum(cumulative_number)) -> final_nyt_state
          }
          if (input$facet_county == "Yes"){
              final_nyt %>%
                  rename(y= cumulative_number) -> final_nyt_state
          }
        
        final_nyt_state
        
        
        
        
    })
    
    ## Define your renderPlot({}) for NYT panel that plots the reactive variable. ALL PLOTTING logic goes here.
    output$nyt_plot <- renderPlot({
        nyt_data_subset() %>%
            ggplot(aes(x = x, 
                       y = y, 
                       color = covid_type, 
                       group = covid_type)) +
                geom_point() +
                geom_line() +
                scale_color_manual(values = c(
                    input$nyt_color_cases, 
                    input$nyt_color_deaths)) +
                labs(title = paste(input$which_state, "Cases and Deaths"),
                     x = "Date",
                     y= "Cumulative Count",
                     color = "Type") -> myplot_nyt
        
    ## Deal with input$y_scale choice       
        if (input$y_scale_nyt == "Log"){
            myplot_nyt <- myplot_nyt + scale_y_log10()
        }
        
    ## deal with input$facet_wrap
     if (input$facet_county == "Yes") myplot_nyt <- myplot_nyt + facet_wrap(~county)
                
     ## Deal with input$which_theme choice
     if (input$which_theme_nyt == "Classic") myplot_nyt <- myplot_nyt + theme_classic()
     if (input$which_theme_nyt == "Minimal") myplot_nyt <- myplot_nyt + theme_minimal()
     if (input$which_theme_nyt == "Solarized") myplot_nyt <- myplot_nyt + theme_solarized()
     if (input$which_theme_nyt == "Economist") myplot_nyt <- myplot_nyt + theme_economist()
     if (input$which_theme_nyt == "Tufte") myplot_nyt <- myplot_nyt + theme_tufte()
     
     ## return the plot to be plotted           
     myplot_nyt + theme(legend.position = "bottom")      
                
    })
    
    
    
 
    
    
    
    ## All server logic for JHU goes here ------------------------------------------

    
    ## Define a reactive for subsetting the JHU data
    jhu_data_subset <- reactive({
        jhu_data %>%
            filter(country_or_region == input$which_country) -> jhu_country
        
        
        #option for starting x on 100th day
        if (input$begin_100_jhu == "No"){
            jhu_country %>%
                rename(x = date) -> final_jhu
        }
        
        if (input$begin_100_jhu == "Yes"){
            jhu_country %>%
                pivot_wider(names_from = covid_type, values_from = cumulative_number) %>% 
                filter(cases >= 100) %>%
                pivot_longer(c(cases, deaths), names_to = "covid_type", values_to = "cumulative_number") %>%
                rename(x = date) -> final_jhu  
        }
        
        
    final_jhu

    })
    
    ## Define your renderPlot({}) for JHU panel that plots the reactive variable. ALL PLOTTING logic goes here.
    output$jhu_plot <- renderPlot({
        jhu_data_subset() %>%
            ggplot(aes(x = x,
                       y = cumulative_number,
                       color = covid_type,
                       group = covid_type)) +
            geom_point() +
            geom_line() +
            scale_color_manual(values = c(
                input$jhu_color_cases,
                input$jhu_color_deaths))+
            labs(title = paste(input$which_country, "Cases and Deaths"),
                 x = "Date",
                 y = "Cumulative Count",
                 color = "Type")  -> myplot_jhu

        
        
        ## Deal with input$y_scale choice       
        if (input$y_scale_jhu == "Log"){
            myplot_jhu <- myplot_jhu + scale_y_log10()
        }
        
        
        ## Deal with input$which_theme choice
        if (input$which_theme_jhu == "Classic") myplot_jhu <- myplot_jhu + theme_classic()
        if (input$which_theme_jhu == "Minimal") myplot_jhu <- myplot_jhu + theme_minimal()
        if (input$which_theme_jhu == "Solarized") myplot_jhu <- myplot_jhu + theme_solarized()
        if (input$which_theme_jhu == "Economist") myplot_jhu <- myplot_jhu + theme_economist()
        if (input$which_theme_jhu == "Tufte") myplot_jhu <- myplot_jhu + theme_tufte()
        
        
        ## return the plot to be plotted           
        myplot_jhu + theme(legend.position = "bottom")  

        
        
    })
    
    
    
    ## All server logic for MAP goes here ------------------------------------------
    
    
    ## Define a reactive for subsetting the MAP 
    nyt_map_subset <- reactive({
        nyt_data %>%
            select(-county) %>%
            group_by(state) %>%
            mutate(sum_data = sum(cumulative_number)) -> nyt_map
        
        
        if (input$case_death == "Cases"){
            nyt_map %>%
                filter(covid_type == "cases") -> final_map
        }
        
        if (input$case_death == "Deaths"){
            nyt_map %>%
                filter(covid_type == "deaths") -> final_map
        
        }
        
    })
    
    
    
    ## Define your renderPlot({}) for MAP panel that plots the reactive variable. ALL PLOTTING logic goes here.
    output$map_plot <- renderPlot({
       nyt_map_subset() %>%
        plot_usmap(values = sum_data, color = "black") +
            scale_fill_continuous(low = "white", high = "red", name = "Cumulative Counts", label = scales::comma)
        
    })
            
    
}
    


# Do not touch below this line! ----------------------------------
shinyApp(ui = ui, server = server)
