#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(rmarkdown)
library(knitr)
library(scales)
library(plotly)
library(rCharts)

# library(ggvis)


# prepare data

load("data/harm.rda")



Property <- harm.df %>% filter(!is.na(prop.ev)) %>% select(1:6,9) %>% 
                filter(prop.ev > 0) %>%
                group_by(event) %>%
                summarise(total.raw = sum(prop.ev)) %>%
                arrange(desc(total.raw)) %>%
                mutate(
                        media.raw = mean(total.raw),
                        rank = seq_len(length(event))
                       )
                # filter(total.raw > mean(total.raw))

Property$event <- factor(Property$event,
                           levels = Property$event[order(Property$rank)])



Crops <- harm.df %>% filter(!is.na(crop.ev)) %>% select(1:6,10) %>%
                filter(crop.ev > 0) %>%
                group_by(event) %>%
                # summarise(total.raw = sum(crop.ev)) %>%
                summarise(total.raw = sum(crop.ev)) %>%
                arrange(desc(total.raw)) %>%
                mutate(
                        media.raw = mean(total.raw),
                        rank = seq_len(length(event))
                        )
                # filter(total.raw > mean(total.raw))

Crops$event <- factor(Crops$event,
                           levels = Crops$event[order(Crops$rank)])


shinyServer(function(input, output) {
        
        
        #         # c("Fatalities", "Injuries", "Property", "Crops")
 
        tipo <- reactive({
                x <- get(input$type)
        })

      
        
        # Fill in the spot we created for a plot
        output$worst <- renderPlotly({

                df <- tipo()

                
                df <- filter(df, rank <= input$max)
                
                
                plt <- ggplot(data=df, aes(event, total.raw, fill=event,
                                                                     text = paste(event, "<br>",
                                                                                  dollar(total.raw), " in losses")))

                        plt <- plt + geom_bar(stat="identity") +
                                geom_hline(aes(yintercept = media.raw,
                                               text=paste("mean", round(media.raw,2))), linetype=1) +
                                labs(title="All time", y="", x="") +
                                scale_y_continuous(labels = dollar)+
                                theme(legend.position="none") +
                                theme(legend.title=element_blank()) +
                                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                                theme(plot.title = element_text(hjust = 0.5))

                        ggplotly(plt + labs(title=paste("Total losses - All time"),
                                                     y="Losses", x=""), tooltip = "text")

        })
        
   
})
        
    