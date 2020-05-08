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
                   title = "Covid-19 tests displayed as Shiny!", ### Replace title with something reasonable
            
            ## All UI for NYT goes in here:
            tabPanel("NYT data visualization", ## do not change this name
            
                    # All user-provided input for NYT goes in here:
                    sidebarPanel(
                        
                        colourpicker::colourInput("nyt_color_cases", "Color for plotting COVID cases:", value = "green"),
                        colourpicker::colourInput("nyt_color_deaths", "Color for plotting COVID deaths:", value = "brown"),
                        selectInput("which_state",  #input$which_state
                                    "which state would you plot?",
                                    choices = usa_states,
                                    selected = "New Jersey"),
                        radioButtons("y_scale_ny",
                                     "scale for y axis",
                                     choices = c("Linear","Log"),
                                     selected = "Linear"),
                        radioButtons("facet_county",
                                     "should we show all counties",
                                     choices = c("No","Yes"),
                                     selected = "Yes"),
                        selectInput("which_theme", 
                                    "which ggplot theme?",
                                    ##need 4 choices
                                    choices = c("Classic", "BW","Light","Dark"),
                                    selected = ("Classic"))
                        
                        
                    ), # closes NYT sidebarPanel. Note: we DO need a comma here, since the next line opens a new function     
                    
                    # All output for NYT goes in here:
                    mainPanel(
                        plotOutput("nyt_plot",width = "700px")
                    ) # closes NYT mainPanel. Note: we DO NOT use a comma here, since the next line closes a previous function  
            ), # closes tabPanel for NYT data
            
            
            ## All UI for JHU goes in here:
            tabPanel("JHU data visualization", ## do not change this name
                     
                     # All user-provided input for JHU goes in here:
                     sidebarPanel(

                         colourpicker::colourInput("jhu_color_cases", "Color for plotting COVID cases:", value = "purple"),
                         colourpicker::colourInput("jhu_color_deaths", "Color for plotting COVID deaths:", value = "orange"),
                          selectInput("which_country_or_region",  
                                      "which country or state should we plot?",
                                      choices = world_countries_regions,
                                      selected = "Japan"),
                          radioButtons("y_scale_jhu",
                                       "scale for y axis",
                                       choices = c("Linear","Log"),
                                       selected = "Linear"),
                          selectInput("which_theme_jhu", ## need a diff name for the widget
                                      "which ggplot theme?",
                                      ##need 4 choices
                                      choices = c("Grey", "LineDraw","Minimal","Test"),
                                      selected = ("Grey"))
                         # 
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



    
    ## All server logic for NYT goes here ------------------------------------------
    
    ## Define a reactive for subsetting the NYT data
    nyt_data_subset <- reactive({
        
        nyt_data%>%
            filter(state == input$which_state) -> nyt_state
        
        if(input$facet_county == "No"){
            ## merge county to ger single point / day for case/death
            nyt_state %>%
                group_by(date, covid_type)%>%
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
       ggplot(aes(x = date, y= y, color = covid_type,group = covid_type))+
       geom_point()+
       geom_line()+
       scale_color_manual(values =c(input$nyt_color_cases,input$nyt_color_deaths))+
       labs(title = paste(input$which_state,"cases and deaths"))->newplot 

       ## for scale choices
         if(input$y_scale_ny == "Log"){
            newplot <- newplot + scale_y_log10()
        }
        
        ## for theme choices
        if(input$which_theme == "Classic") newplot <- newplot +theme_classic()
        if(input$which_theme == "BW") newplot <- newplot +theme_bw()
        if(input$which_theme == "Light") newplot <- newplot +theme_light()
        if(input$which_theme == "Dark") newplot <- newplot +theme_dark()
        
        ## make the choice for facet
        if(input$facet_county == "Yes") newplot <- newplot + facet_wrap(~county,scales = "free_y")
        
        ## adjust the date labels with axis.text
       newplot + theme(legend.position = "bottom",axis.text.x = element_text(angle = 45,hjust = 1))
    })
    
   
   
  
   

    
    
    ## All server logic for JHU goes here ------------------------------------------

    
    ## Define a reactive for subsetting the JHU data
    jhu_data_subset <- reactive({
      jhu_data%>%
        filter(country_or_region == input$which_country_or_region) -> jhu_country
      
     jhu_country
  
      
    })
    
    ## Define your renderPlot({}) for JHU panel that plots the reactive variable. ALL PLOTTING logic goes here.
    output$jhu_plot <- renderPlot({
      jhu_data_subset() %>%
        ggplot(aes(x = date, y= cumulative_number, color = covid_type,group = covid_type))+
        ## make a bar plot
        geom_bar(position = "stack", stat = "identity")+
        scale_color_manual(values =c(input$jhu_color_cases,input$jhu_color_deaths))+
        labs(title = paste(input$which_country_or_region,"cases and deaths")) ->jhuplot
      
      ## for scale choices
      if(input$y_scale_jhu == "Log"){
        jhuplot <- jhuplot + scale_y_log10()
      }
      
      ## for theme choices
      if(input$which_theme_jhu == "Grey") jhuplot <- jhuplot +theme_grey()
      if(input$which_theme_jhu == "LineDraw") jhuplot <- jhuplot +theme_linedraw()
      if(input$which_theme_jhu == "Minimal") jhuplot <- jhuplot +theme_minimal()
      if(input$which_theme_jhu == "Test") jhuplot <- jhuplot +theme_test()
      
    
                                                  ## use axis.text to adjust the labels on the x axis
      jhuplot + theme(legend.position = "bottom",axis.text.x = element_text(angle = 45,hjust = 1))
      
    })
    
}





# Do not touch below this line! ----------------------------------
shinyApp(ui = ui, server = server)
