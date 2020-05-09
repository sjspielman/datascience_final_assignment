library(shiny)
library(shinythemes)
library(tidyverse)
library(colourpicker)
library(plotly) #library for interactive plots
library(rsconnect) #library for shinyapps.io (the URL)

source("covid_data_load.R") ## This line runs the Rscript "covid_data_load.R", which is expected to be in the same directory as this shiny app file!
# The variables defined in `covid_data_load.R` are how fully accessible in this shiny app script!!

# UI --------------------------------
ui <- shinyUI(
        navbarPage(theme = shinytheme("spacelab"),
                   title = "Covid-19 Cases & Deaths", 
            
            ## All UI for NYT goes in here:
            tabPanel("NYT data visualization", ## do not change this name
            
                    # All user-provided input for NYT goes in here:
                    sidebarPanel(
                        
                        colourpicker::colourInput("nyt_color_cases", "Color for plotting COVID cases:", value = "B02AD1"),
                        colourpicker::colourInput("nyt_color_deaths", "Color for plotting COVID deaths:", value = "10CC9D"),
                        
                        selectInput("which_state", #input$which_state
                                    "Which state would you like to plot?",
                                    choices = usa_states,
                                    selected = "New Jersey"),
                        
                        radioButtons("y_scale_nyt",
                                     "Scale for Y-axis?",
                                     choices = c("Linear", "Log"),
                                     selected = "Linear"),
                        
                        radioButtons("facet_county",
                                     "Show counties across panels or pool all couties?",
                                     choices = c("No", "Yes"),
                                     selected = "No"),
                        
                        radioButtons("Start_100_nyt",
                                     "Start on 100th case?",
                                     choices = c("Yes", "No"),
                                     selected = "No"),
                        
                        selectInput("which_theme_nyt", #input$which_theme_nyt
                                    "Which ggplot theme would you like to use?",
                                    choices = c("Classic", "Minimal", "Linedraw", "Dark"),
                                    selected = "Linedraw")
                        
                        
                    ), # closes NYT sidebarPanel. Note: we DO need a comma here, since the next line opens a new function     
                    
                    # All output for NYT goes in here:
                    mainPanel(
                        plotlyOutput("nyt_plot", height = "700px")
                    ) # closes NYT mainPanel. Note: we DO NOT use a comma here, since the next line closes a previous function  
            ), # closes tabPanel for NYT data
            
            
            ## All UI for JHU goes in here:
            tabPanel("JHU data visualization", ## do not change this name
                     
                     # All user-provided input for JHU goes in here:
                     sidebarPanel(

                         colourpicker::colourInput("jhu_color_cases", "Color for plotting COVID cases:", value = "12D3FA"),
                         colourpicker::colourInput("jhu_color_deaths", "Color for plotting COVID deaths:", value = "6AD637"),
                         
                         selectInput("which_country_region", #input$which_country_region
                                     "Which country or region would you like to plot?",
                                     choices = world_countries_regions,
                                     selected = "US"),
                         
                         radioButtons("y_scale_jhu",
                                      "Scale for Y-axis?",
                                      choices = c("Linear", "Log"),
                                      selected = "Linear"),
                         
                         radioButtons("Start_100_jhu",
                                      "Start on 100th case?",
                                      choices = c("Yes", "No"),
                                      selected = "No"),
                         
                         selectInput("which_theme_jhu", #input$which_theme_jhu
                                     "Which ggplot theme would you like to use?",
                                     choices = c("Classic", "Minimal", "Linedraw", "Dark"),
                                     selected = "Linedraw")
                         
                     ), # closes JHU sidebarPanel     
                     
                     # All output for JHU goes in here:
                     mainPanel(
                        plotlyOutput("jhu_plot", height = "700px")
                     ) # closes JHU mainPanel     
            ) # closes tabPanel for JHU data
    ) # closes navbarPage
) # closes shinyUI

# Server --------------------------------
server <- function(input, output, session) {

    
## All server logic for NYT goes here ------------------------------------------
## Define a reactive for subsetting the NYT data
    nyt_data_subset <- reactive({
    
    #choice for which state 
    nyt_data %>% 
      filter(state == input$which_state) -> nyt_state
     
      #Option to start x on 100th case 
      if (input$Start_100_nyt == "No"){
        nyt_state %>%
          rename(x_nyt = date) -> final_nyt
      }
      if (input$Start_100_nyt == "Yes"){
        nyt_state %>%
          pivot_wider(names_from = covid_type, values_from = cumulative_number) %>% ## ONE ROW PER DATE
          filter(cases >= 100) %>%
          pivot_longer(c(cases, deaths), names_to = "covid_type", values_to = "cumulative_number") %>%
          rename(x_nyt = date) -> final_nyt  
      }
      
    #option for faceting 
        if (input$facet_county == "No"){
            final_nyt %>%
                group_by(x_nyt, covid_type) %>% 
                summarise(y_nyt = sum(cumulative_number)) -> final_nyt_state
        }
        if (input$facet_county == "Yes"){
            final_nyt %>% 
                rename(y_nyt = cumulative_number) -> final_nyt_state
        }
        #must spit it out at the end!
        final_nyt_state
    })
    
    ## Define your renderPlot({}) for NYT panel that plots the reactive variable. ALL PLOTTING logic goes here.
output$nyt_plot <- renderPlotly({
  nyt_data_subset() %>% #watch x and y and inputs!
            ggplot(aes(x = x_nyt, y= y_nyt, color= covid_type, group= covid_type)) + 
            geom_point() + 
            geom_line() +
            scale_color_manual(values = c(input$nyt_color_cases, input$nyt_color_deaths)) + 
           labs(x = "Date", y= "Total Cumlative Count", color = "Covid Type", title= paste(input$which_state, "Cases and Deaths")) -> myplot_nyt
    
#Deal with input$y_scale choice
    if (input$y_scale_nyt == "Log"){
        myplot_nyt <- myplot_nyt + scale_y_log10()   
    }
    
#Deal with input$facet_county
  if (input$facet_county == "Yes") myplot_nyt <- myplot_nyt + facet_wrap(~county)  
    
#Deal with input$which_theme choice
    if (input$which_theme_nyt == "Classic") myplot_nyt <- myplot_nyt + theme_classic()
    if (input$which_theme_nyt == "Minimal") myplot_nyt <- myplot_nyt + theme_minimal()
    if (input$which_theme_nyt == "Dark") myplot_nyt <- myplot_nyt + theme_dark()
    if (input$which_theme_nyt == "Linedraw") myplot_nyt <- myplot_nyt + theme_linedraw()
    
#Return the plot
    myplot_nyt + 
      theme(legend.position = "bottom") + #legend on bottom
      theme(plot.title = element_text(face= "bold", size = 16)) + #plot title bold and bigger
      theme(axis.text=(element_text(size= 13, color= "gray21"))) + #axis ticks bigger and grey 
      theme(axis.title = element_text(size= 15)) +  #axis titles bigger 
      theme(legend.text = element_text(size = 13)) -> myplot_nyt2
    
    #print plotly  
    print(ggplotly(myplot_nyt2))
    
    })
    
    
    
  
## All server logic for JHU goes here ------------------------------------------

    
## Define a reactive for subsetting the JHU data
  jhu_data_subset <- reactive({

  #Choice for which country or region 
  jhu_data %>% 
        filter(country_or_region == input$which_country_region) -> jhu_country
  
  #Option to start x on 100th case 
   if (input$Start_100_jhu == "No"){
     jhu_country %>%
       rename(x_jhu = date) -> final_jhu
   }
   if (input$Start_100_jhu == "Yes"){

     jhu_country %>% #seperate covid type to cases and deaths to get one row per date
       pivot_wider(names_from = covid_type, values_from = cumulative_number) %>% ## ONE ROW PER DATE
       filter(cases >= 100) %>% #then filter for ones starting after the 100th case
       pivot_longer(c(cases, deaths), names_to = "covid_type", values_to = "cumulative_number") %>%
       rename(x_jhu = date) -> final_jhu

     }
  #must spit it back out at the end!
  final_jhu
      
    })
    
    ## Define your renderPlot({}) for JHU panel that plots the reactive variable. ALL PLOTTING logic goes here.
 output$jhu_plot <- renderPlotly({

    jhu_data_subset() %>% #watch x and y along with imputs!
     ggplot(aes(x = x_jhu, y= cumulative_number, color= covid_type, group= covid_type)) +
     geom_point() +
     geom_line() +
     scale_color_manual(values = c(input$jhu_color_cases, input$jhu_color_deaths)) +
     labs(x = "Date", y= "Total Cumlative Count", color = "Covid Type", 
          title= paste(input$which_country_region, "Cases and Deaths")) -> myplot_jhu

      #Deal with input$y_scale choice
      if (input$y_scale_jhu == "Log"){
        myplot_jhu <- myplot_jhu + scale_y_log10()
      }

      #Deal with input$which_theme choice
      if (input$which_theme_jhu == "Classic") myplot_jhu <- myplot_jhu + theme_classic() 
      if (input$which_theme_jhu == "Minimal") myplot_jhu <- myplot_jhu + theme_minimal()
      if (input$which_theme_jhu == "Dark") myplot_jhu <- myplot_jhu + theme_dark()
      if (input$which_theme_jhu == "Linedraw") myplot_jhu <- myplot_jhu + theme_linedraw()

       #Return the plot
       myplot_jhu + 
         theme(legend.position = "bottom") + #legend bottom
         theme(plot.title = element_text(face= "bold", size = 16)) + #Plot title bold and bigger
         theme(axis.text=(element_text(size= 13, color= "gray21"))) + #axis ticks bigger and grey 
         theme(axis.title = element_text(size= 15)) + #axis titles bigger
         theme(legend.text = element_text(size = 13)) -> myplot_jhu2
       
       #print plotly
       print(ggplotly(myplot_jhu2))
       
    })
    
}


# Do not touch below this line! ----------------------------------
shinyApp(ui = ui, server = server)
