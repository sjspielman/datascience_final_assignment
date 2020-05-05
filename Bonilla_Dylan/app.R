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
        navbarPage( theme = shinytheme("lumen"), ### Uncomment the theme and choose your own favorite theme from these options: https://rstudio.github.io/shinythemes/
                   title = "COVID-19 and its Affects Across the World", ### Replace title with something reasonable
            
            ## All UI for NYT goes in here:
            tabPanel("NYT data visualization", ## do not change this name
            
                    # All user-provided input for NYT goes in here:
                    sidebarPanel(
                        
                        colourpicker::colourInput("nyt_color_cases",
                                                  "Color for plotting COVID cases:",
                                                  value = "Gold"),
                        colourpicker::colourInput("nyt_color_deaths", 
                                                  "Color for plotting COVID deaths:",
                                                  value = "Navy"),
                        
                        ##The above is the widget for selecting color
                        selectizeInput("state_nyt",
                                       "Which State?",
                                       choices=usa_states,
                                       selected="Florida"),
                         ##The above is the widget selecting for state input$state_nyt
                        
                        radioButtons("faceting_nyt",
                                     "Show Counties in the State as Seperate Graphs?",
                                     choices=c("Yes","No"),
                                     selected = "No"),
                        ##The above is for the widget selecting for facet by county #input$faceting_nyt
                        
                        radioButtons("y_scale_nyt",
                                     "Change Y-Scale to?",
                                     choices=c("Linear","Log"),
                                     selected = "Linear"),
                        #The above is the widget for selecting for Y-scale #input$y_scale_nyt
                        
                        selectInput("theme_nyt",
                                    "Select Theme",
                                    choices=c("Gray","Light","Black and White","Minimal"),
                                    selected = "Gray")
                        #the above is the widget for selecting theme for graphs #input$theme_nyt
                        
                        
                        
                    ), # closes NYT sidebarPanel. Note: we DO need a comma here, since the next line opens a new function     
                    
                    # All output for NYT goes in here:
                    mainPanel(
                        plotOutput("nyt_plot")
                    ) # closes NYT mainPanel. Note: we DO NOT use a comma here, since the next line closes a previous function  
            ), # closes tabPanel for NYT data
            
            
            ## All UI for JHU goes in here:
            tabPanel("JHU data visualization", ## do not change this name
                     
                     # All user-provided input for JHU goes in here:
                     sidebarPanel(

                         colourpicker::colourInput("jhu_color_cases", 
                                                   "Color for plotting COVID cases:", 
                                                   value = "Red"),
                         colourpicker::colourInput("jhu_color_deaths", 
                                                   "Color for plotting COVID deaths:", 
                                                   value = "Black"),
                         ##The above is the widget for selecting color
                         
                         selectizeInput("country",
                                        "Which Country?",
                                        choices=world_countries_regions,
                                        selected="US"),
                         ##The above is the widget selecting for state input$country
                         
                         radioButtons("y_scale_jhu",
                                      "Change Y-Scale to?",
                                      choices=c("Linear","Log"),
                                      selected = "Linear"),
                         #The above is the widget for selecting for Y-scale #input$y_scale_jhu
                         
                         selectInput("theme_jhu",
                                     "Select Theme",
                                     choices=c("Light","Minimal","Black and White","Gray"),
                                     selected = "Light")
                         #the above is the widget for selecting theme for graphs #input$theme_jhu
                         
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
            filter(state==input$state_nyt)->state
        #the above uses the user input for their desire state, saves as varible
        
        if(input$faceting_nyt=="No"){
            state %>%
                group_by(date,covid_type) %>%
                summarize(y=sum(cumulative_number)) ->final_state
        #the above uses the user input for county, in the case for no it adds the sum of all the counties to give a state value    
        }
        
        if(input$faceting_nyt=="Yes"){
            state %>%
                rename(y=cumulative_number) -> final_state
        #the avoce uses the user input for county, in this case for yes it does not add up the county toals in a state keeping them seperate
        }
        
        final_state
        #of which ever input for county is selected the end result will always be final_state so that its is used in the output plot
    })
    
    ## Define your renderPlot({}) for NYT panel that plots the reactive variable. ALL PLOTTING logic goes here.
    output$nyt_plot <- renderPlot({
        nyt_data_subset() %>%
            ggplot(aes(x=date,
                       y=y,
                       color=covid_type,
                       group=covid_type))+
            geom_point()+
            geom_line()+
            scale_color_manual(values=c(input$nyt_color_cases,
                                        input$nyt_color_deaths))+
            labs(title="COVID-19 Data in the United States",
                 x="Date",
                 y="Count",
                 color="Type")->plot_nyt
            #the above gives the user the plot that will be used for the output
        
        if(input$y_scale_nyt=="Log"){
            plot_nyt + scale_y_log10() -> plot_nyt
        }
             #the above adds the user input of choice of y-scale to the graph
        
        if(input$theme_nyt == "Gray") plot_nyt+theme_gray()-> plot_nyt
        if(input$theme_nyt == "Light") plot_nyt+theme_light()-> plot_nyt
        if(input$theme_nyt == "Minimal") plot_nyt+theme_minimal()-> plot_nyt
        if(input$theme_nyt == "Black and White") plot_nyt+theme_bw()-> plot_nyt
            #the above adds the user input of choice of theme to the plot varible for output
        
        if(input$faceting_nyt=="Yes") plot_nyt+facet_wrap(~county, scales="free_y")-> plot_nyt
        #the above uses the user input if they want to see indiviudal counties of state by added facet_wrap(~county)
        
        plot_nyt
    })
    
    
    
    
    ## All server logic for JHU goes here ------------------------------------------

    
    ## Define a reactive for subsetting the JHU data
    jhu_data_subset <- reactive({
        jhu_data %>%
            group_by(`Country/Region`,
                     covid_type,
                     date) %>%
            filter(`Country/Region`==input$country) %>%
            summarise(y=sum(cumulative_number))->country
        country
    })
    
    ## Define your renderPlot({}) for JHU panel that plots the reactive variable. ALL PLOTTING logic goes here.
    output$jhu_plot <- renderPlot({
        jhu_data_subset() %>%
            ggplot(aes(x=date,
                       y=y,
                       color=covid_type,
                       group=covid_type))+
            geom_point()+
            geom_line()+
            scale_color_manual(values=c(input$jhu_color_cases,
                                        input$jhu_color_deaths))+
            labs(title="COVID-19 Data Worldwide",
                 x="Date",
                 y="Count",
                 color="Type")->plot_jhu
        #the above gives the user the plot that will be used for the output JHU
        
        if(input$y_scale_jhu=="Log"){
            plot_jhu + scale_y_log10() -> plot_jhu
        }
        #the above adds the user input of choice of y-scale to the graph
        
        if(input$theme_jhu == "Gray") plot_jhu+theme_gray()-> plot_jhu
        if(input$theme_jhu == "Light") plot_jhu+theme_light()-> plot_jhu
        if(input$theme_jhu == "Minimal") plot_jhu+theme_minimal()-> plot_jhu
        if(input$theme_jhu == "Black and White") plot_jhu+theme_bw()-> plot_jhu
        #the above adds the user input of choice of theme to the plot varible for output jhu
        
        plot_jhu
        
    })
    
}





# Do not touch below this line! ----------------------------------
shinyApp(ui = ui, server = server)
