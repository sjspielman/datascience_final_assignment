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
library(tvthemes)

source("covid_data_load.R") ## This line runs the Rscript "covid_data_load.R", which is expected to be in the same directory as this shiny app file!
# The variables defined in `covid_data_load.R` are how fully accessible in this shiny app script!!

# UI --------------------------------
ui <- shinyUI(
        navbarPage( theme = shinytheme("simplex"), 
                   title = "GLOBAL COVID-19 UPDATES", 
            
            ## All UI for NYT goes in here:
            tabPanel("NYT data visualization", ## do not change this name
            
                    # All user-provided input for NYT goes in here:
                    sidebarPanel(
                        
                        colourpicker::colourInput("nyt_color_cases", "Color for plotting COVID cases:", value = "deepskyblue"), 
                        colourpicker::colourInput("nyt_color_deaths", "Color for plotting COVID deaths:", value = "mediumorchid1"),
                        selectInput("which_state", 
                                    "Select State", 
                                    choices = usa_states,
                                    selected = "New Jersey"),
                        radioButtons("facet_county",
                                     "Display all Counties",
                                     choices = c("No","Yes"),
                                     selected = "No"),
                        radioButtons("y_scale_nyt",
                                     "Select y-axis Scale",
                                     choices = c("Linear","Log"),
                                     selected = "Linear"),
                        selectInput("which_theme_nyt",
                                    "Select ggplot Theme",
                                    choices = c("Classic","RickAndMorty","Avatar", "Brooklyn99", "ParksAndRec"),
                                    selected = "Classic")
                        
                        
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

                         colourpicker::colourInput("jhu_color_cases", "Color for plotting COVID cases:", value = "olivedrab"),
                         colourpicker::colourInput("jhu_color_deaths", "Color for plotting COVID deaths:", value = "goldenrod1"),
                         selectInput("which_country",
                                     "Select Country/Region",
                                     choices = world_countries_regions, 
                                     selected = "US"),
                         radioButtons("y_scale_jhu",
                                      "Select y-axis Scale",
                                      choices = c("Linear","Log"),
                                      selected = "Linear"),
                         selectInput("which_theme_jhu",
                                    "Select ggplot Theme",
                                    choices = c("Classic","SpongeBob","Simpsons", "Dark"),
                                    selected = "Classic")
                         
                         
                         
                     ), # closes JHU sidebarPanel     
                     
                     # All output for JHU goes in here:
                     mainPanel(
                        plotOutput("jhu_plot", height= "700px")
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
            #combining data from county to unite into one point each day for case and death
            nyt_state%>%
            group_by(date, covid_type) %>%
            summarise(y = sum(cumulative_number)) -> final_nyt_state
        }
        if(input$facet_county =="Yes"){
            nyt_state %>%
                rename(y = cumulative_number) -> final_nyt_state
        }
        
        final_nyt_state ## contents of this variable aka WHAT COMES OUT OF LAST LINE OF THE REACTIVE should be assigned as nyt_data_subset()
    }) ## end of reactive definition
    
    ## Define your renderPlot({}) for NYT panel that plots the reactive variable. ALL PLOTTING logic goes here.
    output$nyt_plot <- renderPlot({
       nyt_data_subset() %>% 
            ggplot(aes(x= date, y= y, color= covid_type, group= covid_type))+
            geom_point()+
            geom_line() +
            scale_color_manual(values =c(input$nyt_color_cases, input$nyt_color_deaths)) +
                labs(title= paste(input$which_state, "Cases and Deaths"), x="Date", y= "Number of Occurences", color = "Covid Type")-> my_plot
            
            
        ##Input y scale choice    
        if(input$y_scale_nyt =="Log") {
          my_plot <- my_plot + scale_y_log10()  
        }   
        
            
        #Input which theme choice
        if(input$which_theme_nyt == "Classic") my_plot <- my_plot + theme_classic()
        if(input$which_theme_nyt == "Brooklyn99") my_plot <- my_plot + theme_brooklyn99()
        if(input$which_theme_nyt == "RickAndMorty") my_plot <- my_plot + theme_rickAndMorty()
        if(input$which_theme_nyt == "Avatar") my_plot <- my_plot + theme_avatar()
        if(input$which_theme_nyt == "ParksAndRec") my_plot <- my_plot + theme_parksAndRec_light()
        
        #Input facet_county
        if(input$facet_county == "Yes") my_plot <- my_plot+ facet_wrap(~county, scales = "free_y")
            
       
        #returns scale, add custom settings such as theme
        my_plot+
            theme(axis.text = element_text(size = 11, face= "bold.italic"), 
                  axis.title = element_text(size=14,face= "bold.italic"),
                  plot.title = element_text(size = 18,face= "bold.italic"),
                  legend.title = element_text(size = 11, face= "bold.italic"))
            
            
        })
   

    
    
    ## All server logic for JHU goes here ------------------------------------------

    
    ## Define a reactive for subsetting the JHU data
    jhu_data_subset <- reactive({
        jhu_data %>%
            filter(country_or_region == input$which_country)-> jhu_global
    
    jhu_global    
    })
    
    ## Define your renderPlot({}) for JHU panel that plots the reactive variable. ALL PLOTTING logic goes here.
    output$jhu_plot <- renderPlot({
        jhu_data_subset() %>%
            ggplot(aes(x= date, y= cumulative_number, group= covid_type, color= covid_type)) +
            geom_point()+
            geom_line() +
            scale_color_manual(values =c(input$jhu_color_cases, input$jhu_color_deaths)) +
            labs(title= paste(input$which_country, "Cases and Deaths"), x="Date", y= "Number of Occurences", color = "Covid Type")-> final_jhu_plot
        
        #Y axis input
        if(input$y_scale_jhu =="Log") {
            final_jhu_plot <- final_jhu_plot + scale_y_log10()  
        }
        
        #theme input
        if(input$which_theme_jhu == "Classic") final_jhu_plot <- final_jhu_plot + theme_classic()
        if(input$which_theme_jhu == "Dark") final_jhu_plot <- final_jhu_plot + theme_dark()
        if(input$which_theme_jhu == "SpongeBob") final_jhu_plot <- final_jhu_plot + theme_spongeBob()
        if(input$which_theme_jhu == "Simpsons") final_jhu_plot <- final_jhu_plot + theme_simpsons()
        
        
        #returning the plot
        final_jhu_plot +
            theme(axis.text = element_text(size = 11, face= "bold.italic"), 
                  axis.title = element_text(size=14,face= "bold.italic"),
                  plot.title = element_text(size = 18,face= "bold.italic"),
                  legend.title = element_text(size = 11, face= "bold.italic"))
         
         })
    
}





# Do not touch below this line! ----------------------------------
shinyApp(ui = ui, server = server)
