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
        navbarPage( theme = shinytheme("simplex"), ### Uncomment the theme and choose your own favorite theme from these options: https://rstudio.github.io/shinythemes/
                   title = "Covid data graph", ### Replace title with something reasonable
            
            ## All UI for NYT goes in here:
            tabPanel("NYT data visualization", ## do not change this name
            
                    # All user-provided input for NYT goes in here:
                    sidebarPanel(
                        
                        colourpicker::colourInput("nyt_color_cases", "Color for plotting COVID cases:", value = "green"),
                        colourpicker::colourInput("nyt_color_deaths", "Color for plotting COVID deaths:", value = "black"), 
                        
                        #Selecting up the select box for diferent states
                        selectInput("which_state", # input$which_state
                                   "Which state would you like to plot?",
                                   choices = usa_states,
                                   selected = "Maryland"),
                        
                        #setting up the radio buttons for facet_county
                        radioButtons("facet_county", # input$facet_county
                                     "Show counties across panels, or pool all counties",
                                     choices = c('No', "Yes"),
                                     selected = "No"),
                        
                       #setting up the radio buttons for y scale
                        radioButtons("y_scale", #input$y_scale
                                     "Scale for Y-axis?",
                                     choices = c('Linear', "Log"),
                                     selected = "Linear"),
                       
                        #Select box for chanigng the themes
                        selectInput("which_theme", # input$which_theme
                                    "Which ggplot theme to use?",
                                    choices = c("Classic", "Minimal", "Dark", "Light"),
                                    selected = "Classic")
 
                        
                    ), # closes NYT sidebarPanel. Note: we DO need a comma here, since the next line opens a new function     
                    
                    # All output for NYT goes in here:
                    mainPanel(
                        plotOutput("nyt_plot", height = "400px", width = "800px")
                    ) # closes NYT mainPanel. Note: we DO NOT use a comma here, since the next line closes a previous function  
            ), # closes tabPanel for NYT data
            
            
            ## All UI for JHU goes in here:
            tabPanel("JHU data visualization", ## do not change this name
                     
                     # All user-provided input for JHU goes in here:
                     sidebarPanel(

                         colourpicker::colourInput("jhu_color_cases", "Color for plotting COVID cases:", value = "lightblue4"),
                         colourpicker::colourInput("jhu_color_deaths", "Color for plotting COVID deaths:", value = "chocolate4"),
                         
                         #Selecting up the select box for diferent country
                         
                         #Selecting up the select box for diferent country/region
                         
                         selectInput("which_country", # input$which_country
                                     "Which country/region would you like to plot?",
                                     choices = world_countries_regions,
                                     selected = "Sweden"),
                         
                         #setting up the radio buttons for county
                         radioButtons("facet_region",  #input$facet_region
                                      "Show counties across panels, or pool all counties",
                                      choices = c('No', "Yes"),
                                      selected = "No"),
                         
                         #setting up the radio buttons for y scale
                         radioButtons("y_scale_jhu",   #input$y_scale_jhu
                                      "Scale for Y-axis?",
                                      choices = c('Linear', "Log"),
                                      selected = "Linear"),
                         
                         #Select box for changing the themes
                         selectInput("choose_theme", # input$choose_theme
                                     "Choose a ggplot theme to use?",
                                     choices = c("Classic", "Minimal", "Dark", "Light"),
                                     selected = "Classic")
                         
                     ), # closes JHU sidebarPanel     
                     
                     # All output for JHU goes in here:
                     mainPanel(
                        plotOutput("jhu_plot", height = "400px", width = "800px")
                     ) # closes JHU mainPanel     
            ) # closes tabPanel for JHU data
    ) # closes navbarPage
) # closes shinyUI

# Server --------------------------------
server <- function(input, output, session) {



    
    ## All server logic for NYT goes here ------------------------------------------
    
    ## Define a reactive for subsetting the NYT data
    nyt_data_subset <- reactive({
      
          nyt_data %>%
            filter(state == input$which_state) -> nyt_state
        
        if(input$facet_county == "No"){
            
            #combine state data to get single point per for cases/deaths
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
            ggplot(aes(x = date, y = y, color=covid_type, group=covid_type)) +
                geom_point() +
                geom_line() +
                scale_color_manual(values = c(input$nyt_color_cases, input$nyt_color_deaths)) +
                labs(x = "Date", y = "Cumulative number of cases", color = "Covid Type" ,
                     title= paste(input$which_state, "cases and deaths")) -> myplot
        
        #Dealing with user input$y_scale
        if(input$y_scale == "Log") {
            myplot <- myplot + scale_y_log10()
        }
        #Dealing with input$facet_wrap
        if(input$facet_county == "Yes") myplot <- myplot + facet_wrap(~county, scales = "free_y")
        
        #dealing with the input$which_theme choice
        
        if(input$which_theme == "Classic") myplot <- myplot + theme_classic()
        
        if(input$which_theme == "Minimal") myplot <- myplot + theme_minimal()
        
        if(input$which_theme == "Dark") myplot <- myplot + theme_dark()  
        
        if(input$which_theme == "Light") myplot <- myplot + theme_light()
            
        #return the plot
        myplot + theme(legend.position = "bottom")
    })
    

    ## All server logic for JHU goes here ------------------------------------------

    
    ## Define a reactive for subsetting the JHU data
    jhu_data_subset <- reactive({
      
      jhu_data %>%
        group_by(date, covid_type) %>%
       filter(Country_or_Region == input$which_country) -> jhu_country
      
      if(input$facet_region == "No"){
        
        #combine state data to get single point per for cases/deaths
        jhu_country %>%
          group_by(date, covid_type) %>%
          summarize(y = sum(cumulative_number)) -> final_jhu_country
        
      }
      
      if(input$facet_region == "Yes") {
        jhu_country %>%
          rename(y=cumulative_number) -> final_jhu_country
      }
      
      final_jhu_country
      
    })
    
    ## Define your renderPlot({}) for JHU panel that plots the reactive variable. ALL PLOTTING logic goes here.
    output$jhu_plot <- renderPlot({
      
      jhu_data_subset() %>%
        ggplot(aes(x = date, y = y, color=covid_type, group=covid_type)) +
        geom_point() +
        geom_line() +
        scale_color_manual(values = c(input$jhu_color_cases, input$jhu_color_deaths)) +
        labs(x = "Date", y = "Cumulative number of cases", color = "Covid Type" ,
             title= paste(input$which_country, "cases and deaths")) -> myjhu_plot
      
  #Dealing with input$y_scale_jhu    
      if(input$y_scale_jhu == "Log") {
        myjhu_plot <- myjhu_plot + scale_y_log10()
      }
      #Dealing with input$facet_wrap
      
      if(input$facet_region == "Yes") myjhu_plot <- myjhu_plot + facet_wrap(~Province_or_State, scales = "free")
      
      #dealing with the input$choose_theme choice
      
      if(input$choose_theme == "Classic") myjhu_plot <- myjhu_plot + theme_classic()
      
      if(input$choose_theme == "Minimal") myjhu_plot <- myjhu_plot + theme_minimal()
      
      if(input$choose_theme == "Dark") myjhu_plot <- myjhu_plot + theme_dark()  
      
      if(input$choose_theme == "Light") myjhu_plot <- myjhu_plot + theme_light()
      
     # return the plot
      myjhu_plot + theme(legend.position = "bottom")
      
    })
    
}



# Do not touch below this line! ----------------------------------
shinyApp(ui = ui, server = server)
