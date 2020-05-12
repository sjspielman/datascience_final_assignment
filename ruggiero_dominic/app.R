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

source("covid_data_load.R") ## This line runs the Rscript "covid_data_load.R", which is expected to be in the same directory as this shiny app file!
# The variables defined in `covid_data_load.R` are how fully accessible in this shiny app script!!

# UI --------------------------------
ui <- shinyUI(
        navbarPage( theme = shinytheme("united"), ### Uncomment the theme and choose your own favorite theme from these options: https://rstudio.github.io/shinythemes/
                   title = "Visualization of COVID-19 Cases and Deaths, Domestic and Worldwide", 
            
            ## All UI for NYT goes in here:
            tabPanel("NYT data visualization", ## do not change this name
            
                    # All user-provided input for NYT goes in here:
                    sidebarPanel(
              #first the color selections, with preset red and black colors as the cliche representatives for morbidity and mortality.
                        colourpicker::colourInput("nyt_color_cases", "Color for plotting COVID cases:", value ="#F04646"),
                        colourpicker::colourInput("nyt_color_deaths", "Color for plotting COVID deaths:", value = "#423A3A"),
                       
              #next to select the state, using the vector of states previously given in the load data, and selecting the first in alphabetical order.
                         selectInput("state_choice", "Select a state to plot:", choices=usa_states, selected= "Alabama"),
              #Selecting a theme with a few favorite standard ggplot themes. Keep the variables like theme_choice standard to avoid trouble later on.
                        selectInput("theme_choice", "Choose a plot theme:", choices=c("Classic","Gray", "Light", "Minimal"), selected="Classic"),
              #Now for facet choice, using buttons because there are fewer choices
                        radioButtons("facet_choice", "Display individual counties?", choices=c("Yes","No"), selected="No"),
              #Last, the choice for log scale will also use the buttons as it is a simple, two option choice.
                        radioButtons("scale_choice", "Choose a scale for the plot:", choices=c("Linear", "Logarithmic"), selected="Linear")
                        
                    ),    
                    
                    mainPanel(
                        plotOutput("nyt_plot")
                    )
            ),
            
            ## All UI for JHU goes in here:
            tabPanel("JHU data visualization", 
                     
                     
                     sidebarPanel(

                         colourpicker::colourInput("jhu_color_cases", "Color for plotting COVID cases:", value = "#F04646"),
                         colourpicker::colourInput("jhu_color_deaths", "Color for plotting COVID deaths:", value = "#423A3A"),
                         selectInput("country_choice", "Select a Country or Region to plot:", choices=world_countries_regions, selected= "Afghanistan"),
                         selectInput("jhu_theme_choice", "Select a Plot Theme:", choices=c("Classic", "Economist", "Inverse Gray", "Stata"), selected= "Classic"),
                         radioButtons("jhu_scale_choice", "Choose a scale for the plot:", choices=c("Linear", "Logarithmic"), selected="Linear")
                         
                     ),      
                     
                     
                     mainPanel(
                        plotOutput("jhu_plot")
                     )   
            ) 
    ) 
) 

# Server --------------------------------
server <- function(input, output, session) {


    

    
    ## All server logic for NYT goes here ------------------------------------------
    
    ## Define a reactive for subsetting the NYT data
    nyt_data_subset <- reactive({
    if(input$facet_choice=="No")    
    {nyt_data%>%
   #use input$ to specify the selected state within the input as the plot output.
        filter(state==input$state_choice)%>%
    #group by date and covid type so we can consolidate the data for each county
        group_by(date, covid_type)%>%
    #summarize to collapse all counties into a single state number for each day among each covid type
        summarize(total= sum(cumulative_number))->nyt_state}
    
    if(input$facet_choice=="Yes")
    {nyt_data%>%
        filter(state==input$state_choice)%>%
      #rename to keep the variable consistent.
        rename(total=cumulative_number)->nyt_state}

        nyt_state    

})
    
    output$nyt_plot <- renderPlot({
        nyt_data_subset()%>%
        ggplot(aes(x=date, y=total, color=covid_type, group=covid_type))+
                   geom_point()+
                   geom_line()+
        #scale color manual using inputs to select colors.
                   scale_color_manual(values=c(input$nyt_color_cases, input$nyt_color_deaths))+
                   labs(title=paste("Total COVID-19 Cases and Deaths in", input$state_choice))+
        xlab("Date")+ ylab("Cumulative Cases")->nyt_plot
    #if commands so it will apply these features to the plot if selected, but ignore if not.
      if(input$facet_choice=="Yes"){ 
        nyt_plot<-nyt_plot+facet_wrap(~county)}
      
    if(input$scale_choice=="Logarithmic"){
       nyt_plot<-nyt_plot+scale_y_log10()}           
        
    if(input$theme_choice=="Classic") {nyt_plot<-nyt_plot+ theme_classic()}
    
    if(input$theme_choice=="Gray") {nyt_plot<-nyt_plot+ theme_gray()}
    if(input$theme_choice=="Light") {nyt_plot<-nyt_plot+ theme_light()}
    if(input$theme_choice=="Minimal") {nyt_plot<-nyt_plot+ theme_minimal()}
    

  
    
            
    nyt_plot
  
  })
    
    
    
    
    ## All server logic for JHU goes here ------------------------------------------

    
    ## Define a reactive for subsetting the JHU data
    jhu_data_subset <- reactive({
      jhu_data%>%
        filter(country==input$country_choice)%>%
        group_by(date, covid_type)%>%
        summarize(cumulative_number= sum(cumulative_number))->jhu_country
        
        
        jhu_country
      
    })
    
    ## Define your renderPlot({}) for JHU panel that plots the reactive variable. ALL PLOTTING logic goes here.
    output$jhu_plot <- renderPlot({
      jhu_data_subset()%>%
        ggplot(aes(x=date, y=cumulative_number, color=covid_type, group=covid_type))+
        geom_line()+
        geom_point()+
        scale_color_manual(values=c(input$jhu_color_cases, input$jhu_color_deaths))+
        labs(title=paste("Total COVID-19 Cases and Deaths in", input$country_choice))+
        xlab("Date")+ ylab("Cumulative Cases")->jhu_plot
      
      
      if(input$jhu_theme_choice=="Classic") {jhu_plot<-jhu_plot+ theme_classic()}
      
      if(input$jhu_theme_choice=="Economist") {jhu_plot<-jhu_plot+ theme_economist()}
      
      if(input$jhu_theme_choice=="Inverse Gray") {jhu_plot<-jhu_plot+ theme_igray()}
      
      if(input$jhu_theme_choice=="Stata") {jhu_plot<-jhu_plot+ theme_stata()}
      
      
      if(input$jhu_scale_choice=="Logarithmic"){
        jhu_plot<-jhu_plot+scale_y_log10()}  
      
      
      
      jhu_plot
      
      
      
      
    })
    
}





# Do not touch below this line! ----------------------------------
shinyApp(ui = ui, server = server)
