#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above. maria was here
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
library(rsconnect)


source("covid_data_load.R") ## This line runs the Rscript "covid_data_load.R", which is expected to be in the same directory as this shiny app file!
# The variables defined in `covid_data_load.R` are how fully accessible in this shiny app script!!

# UI --------------------------------
ui <- shinyUI(
        navbarPage( theme = shinytheme("cyborg"), ### Uncomment the theme and choose your own favorite theme from these options: https://rstudio.github.io/shinythemes/
                   title = "Covid-19 Cases and Deaths", ### Replace title with something reasonable
            
            ## All UI for NYT goes in here:
            tabPanel("NYT data visualization", ## do not change this name
            
                    # All user-provided input for NYT goes in here:
                    sidebarPanel(
                        
                        colourpicker::colourInput("nyt_color_cases", "Color for plotting COVID cases:", value = "green"),
                        colourpicker::colourInput("nyt_color_deaths", "Color for plotting COVID deaths:", value = "black"),
                        selectInput("which_state", ##input$which_state,
                                    "Which state would like to plot?",
                                    choices = usa_states,
                                    selected = "New Jersey"),
                        radioButtons("facet_county",
                                     "Would you like to facet by county?",
                                     choices = c("No", "Yes"),
                                     selected ="Yes"),
                        radioButtons("y_scale",
                                     "Which scale would you like for the y-axis?",
                                     choices =c ("linear", "log"),
                                     selected ="linear"),
                        selectInput("which_theme",
                                    "Which theme would you like to use?",
                                    choices = c("classic", "linedraw","void","dark"),
                                    selected = "linedraw")
                        
                    ), # closes NYT sidebarPanel. Note: we DO need a comma here, since the next line opens a new function     
                    
                    # All output for NYT goes in here:
                    mainPanel(
                        plotOutput("nyt_plot", height = "700px")
                    ) # closes NYT mainPanel. Note: we DO NOT use a comma here, since the next line closes a previous function  
            ), # closes tabPanel for NYT data
            
            
            ## All UI for JHU goes in here:
            tabPanel("JHU data visualization", ## do not change this name
                     
                     # All user-provided input for JHU goes in here:
                     sidebarPanel(

                         colourpicker::colourInput("jhu_color_cases", "Color for plotting COVID cases:", value = "pink"),
                         colourpicker::colourInput("jhu_color_deaths", "Color for plotting COVID deaths:", value = "blue"),
                         selectInput("which_country", ##input$which_state,
                                     "Which country would like to plot?",
                                     choices = world_countries_regions,
                                     selected = "Canada"),
                         radioButtons("facet_state",
                                      "Would you like to facet by province or state? (if available)",
                                      choices = c("No", "Yes"),
                                      selected ="Yes"),
                         radioButtons("jhu_y_scale",
                                      "Which scale would you like for the y-axis?",
                                      choices =c ("linear", "log"),
                                      selected ="linear"),
                         selectInput("which_theme",
                                     "Which theme would you like to use?",
                                     choices = c("classic", "linedraw","void","dark"),
                                     selected = "linedraw")
                         
                     ), # closes JHU sidebarPanel     
                     
                     # All output for JHU goes in here:
                     mainPanel(
                        plotOutput("jhu_plot", height = "700px")
                     ) # closes JHU mainPanel     
            ) # closes tabPanel for JHU data
    ) # closes navbarPage
) # closes shinyUI

# Server --------------------------------
server<- function(input, output, session) {

    ## PROTIP!! Don't forget, all reactives and outputs are enclosed in ({}). Not just parantheses or curly braces, but BOTH! Parentheses on the outside.
    
    

    ## All server logic for NYT goes here ------------------------------------------
    
    ## Define a reactive for subsetting the NYT data
    nyt_data_subset <- reactive({ 
        
        nyt_data %>%
            filter(state == input$which_state)-> nyt_state
        
        if(input$facet_county =="No"){
            
            nyt_state%>%
            #combine county data to get single point per day for case/death
            group_by(date, covid_type)%>%
            summarize(y = sum(cumulative_number)) -> final_nyt_state 
        }
    
        if (input$facet_county == "Yes"){
        nyt_state%>%
            rename(y = cumulative_number) -> final_nyt_state
        }
    final_nyt_state
    
    })
    
    ## Define your renderPlot({}) for NYT panel that plots the reactive variable. ALL PLOTTING logic goes here.
  
      output$nyt_plot <- renderPlot({ 
       nyt_data_subset()%>%
        ggplot(aes(x=date, y= y, color= covid_type, group= covid_type))+
                   geom_point() +
                   geom_line() +
                   scale_color_manual(values= c(input$nyt_color_cases, input$nyt_color_deaths))+
                labs(title= input$which_state, "Cases and Deaths",
                     x= "Date",
                     y= "Number of Individuals",
                     color= "Covid Type")-> myplot
     #deal with user log with input log scale
      if(input$y_scale == "log"){
         myplot <- myplot + scale_y_log10()
      }
        #facet by county
        if(input$facet_county == "Yes") myplot <- myplot + facet_wrap(~county)
      
      ### deal with input$which_theme choice
          
      if (input$which_theme == "classic"){ myplot<- myplot + theme_classic()}
      if (input$which_theme == "linedraw"){ myplot <- myplot + theme_linedraw()}
      if (input$which_theme == "void"){ myplot<- myplot + theme_void()}
      if (input$which_theme == "dark"){ myplot <- myplot + theme_dark()}
      #return my plot
      
           myplot #+ theme(legend.postion = "bottom")
    
       
        })

    
#######################################    
    ## All server logic for JHU goes here ------------------------------------------
    
    ## Define a reactive for subsetting the JHU data
    jhu_data_subset <- reactive({
      
     jhu_data %>%
        filter(country_or_region == input$which_country)-> jhu_country
      
     if(input$facet_state =="No"){
        
        jhu_country%>%
          #combine county data to get single point per day for case/death
          group_by(date, covid_type)%>%
          summarize(y = sum(cumulative_number)) -> final_jhu_country
      }
      
      if (input$facet_state == "Yes"){
        jhu_country%>%
          rename(y = cumulative_number) -> final_jhu_country
      }
      final_jhu_country
      
      
    })
    
    ## Define your renderPlot({}) for JHU panel that plots the reactive variable. ALL PLOTTING logic goes here.
    output$jhu_plot <- renderPlot({  jhu_data_subset()%>%
        ggplot(aes(x= date, y= y, color= covid_type, group= covid_type))+
        geom_point() +
        geom_line() +
        scale_color_manual(values= c(input$jhu_color_cases, input$jhu_color_deaths))+
        labs(title= input$which_country, "Cases and Deaths",
             x= "Date",
             y= "Number of Individuals",
             color= "Covid Type")-> myplotj
      #deal with user log with input log scale
      
      if(input$jhu_y_scale == "log"){
        myplotj<- myplotj + scale_y_log10()
        
        }
      #facet by state/province
      if(input$facet_state == "Yes") myplotj <- myplotj + facet_wrap(~province_or_state)
      
      ### deal with input$which_theme choice
      
      if (input$which_theme == "classic"){ myplotj<- myplotj + theme_classic()}
      if (input$which_theme == "linedraw"){ myplotj <- myplotj + theme_linedraw()}
      if (input$which_theme == "void"){ myplotj<- myplotj + theme_void()}
      if (input$which_theme == "dark"){ myplotj <- myplotj + theme_dark()}
      #return my plot
      
      myplotj
      
      })
    
}


    
    


# Do not touch below this line! ----------------------------------
shinyApp(ui = ui, server = server)
