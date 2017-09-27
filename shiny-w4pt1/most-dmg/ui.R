#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggvis)



# Use a fluid Bootstrap layout
fluidPage(    
        
        # Give the page a title
        titlePanel("Most harmfull events - NOAA Database"),
        
        # Generate a row with a sidebar
        sidebarLayout(      
                
                # Define the sidebar with one input
                sidebarPanel(
                       # selectInput("type", "Type (human health or economic dmg)",
                       #              c("Fatalities", "Injuries", "Property", "Crops")
                       #  ),
                       selectInput("type", "Type (economic losses: Property or Crops?)",
                                   # c("Fatalities", "Injuries", "Property", "Crops")
                                   c("Property", "Crops")
                                   
                       ),
                        hr(),
                        helpText("Data from NOAA Storm Database")
                ),
                
                # Create a spot for the barplot
                mainPanel(
                        plotlyOutput("worst")  
                )
                
        )
)