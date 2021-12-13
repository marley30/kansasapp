pop <- read.csv("/Users/laurenturco/Google Drive/Graduate School/STAT 824_Data Visualization/Datasets/KansasPopulation.csv")
crime <- read.csv("/Users/laurenturco/Google Drive/Graduate School/STAT 824_Data Visualization/Datasets/KansasCrimeLong.csv")
counties <- read.csv("/Users/laurenturco/Google Drive/Graduate School/STAT 824_Data Visualization/Datasets/KansasCounties.csv")

cc <- crime %>%
  left_join(counties, by = 'County')

names(crime) <- c("County","2012","2013","2014","2015","2016","2017")

crime <- crime %>%
  pivot_longer(crime, cols=2:7, names_to="Year", values_to = "Rate")

counties$County <- as.factor(counties$County)

library(shiny)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
library(ggplot2)
library(gridExtra)
library(questionr)

ui <- fluidPage(
  titlePanel('Counties in Kansas'),
  fluidRow(
    column(4,
           wellPanel(
             selectInput('county', 'Select A County', levels(as.factor(pop$County)), 
                         selected=levels(as.factor(pop$County))[106]))
           )
  ),
  fluidRow(
    column(10,
           plotOutput('population'))
  ),
  fluidRow(
    column(4,
           wellPanel(textOutput("summary")),
           wellPanel(textOutput("summary2")),
           wellPanel(textOutput("summary3"))
           ),
    column(6,
           plotOutput('area'))
  # mainPanel(
  #   plotOutput('population'),
  #   plotOutput('area'),
  #   tableOutput('table1')
  # )
)
)

server <- function(input, output) {
  # select county for population
  selectedData <- reactive({
    pop %>% filter(County == input[["county"]])
  })
  
  # select county for crime
  selectedData2 <- reactive({
    crime %>% filter(crime == input[["county"]])
  })
  
  # plotOutput('population')
  output$population <- renderPlot({
    p1 <- ggplot(selectedData(), aes(selectedData()$Year, selectedData()$Population, 
                               fill=Population)) +
      geom_col() +
      coord_cartesian(ylim=c(min(selectedData()$Population), max(selectedData()$Population))) +
      scale_fill_gradient(low="lightgoldenrod", high="deeppink4", labels=comma) +
      labs(title=paste0(input[["county"]], " ", "County Population")) +
      ylab("") + xlab("") +
      scale_y_continuous(label=comma) +
      theme(
        plot.title = element_text(hjust=0.5, size=16),
        axis.text.x = element_text(size=10),
        legend.position = "none"
      )
    
    p2 <- ggplot(selectedData2(), 
                 aes(as.numeric(selectedData2()$Year), selectedData2()$Rate,
                     size=Rate, color=Rate)) +
      geom_point() +
      coord_cartesian(ylim=c(min(selectedData2()$Rate), max(selectedData2()$Rate))) +
      scale_colour_gradient(low="lightgoldenrod", high="deeppink4", labels=comma) +
      labs(title=paste0(input[["county"]], " ", "County Crime Rate")) +
      ylab("") + xlab("") +
      scale_y_continuous(label=comma) +
      theme(
        plot.title = element_text(hjust=0.5, size=16),
        axis.text.x = element_text(size=10),
        legend.position = "none"
      ) +
      geom_line(size=.75, color="black", alpha=0.25)
    
    grid.arrange(p1,p2, ncol=2)
  })
  
  
  # select county for area
  selectedData3 <- reactive({
    counties %>% filter(counties == input[["county"]])
    x1 <- sum(counties$Area) - selectedData3()[4]
    x2 <- selectedData3()[4]
    df <- as.data.frame(c(x1,x2))
    colnames(df) <- "Area"
  })
  
  
  # plotOutput('area')
  output$area <- renderPlot({

    ggplot(df, aes(x="", Area, fill=as.factor(Area))) + 
      geom_bar(stat="identity") +
      coord_polar("y", start=0) +
      scale_fill_manual(values=c("goldenrod1", "deeppink4"), 
                        name = "Area", labels = c(input[["county"]], "All other counties")) +
      theme_void() +
     # labs(title=paste0("Relative Area ","of ", input[["county"]], " County")) +
      labs(title=bquote("Relative Area"~(mi^2)~"of"~.(input$county)~"County")) +
      theme(plot.title = element_text(hjust=0.5, size=16))
      
  })
  
  # filtering data by county for table
  popn <- function(){
    pop %>% 
      filter(County == input$county) %>%
      mutate(Population = label_comma()(Population))
  }
  # # tableOutput('table1')
  # output$table1 <- renderTable(
  #   popn()[,c(3,4)], rownames=FALSE, digits=0
  # )
  
  # SUMMARY STATISTICS
  # calculate the average rate of change
  output$summary <- renderText({
    paste0("The annual growth rate of ", input[["county"]], " County is ", 
           round((selectedData()$Population[9]-selectedData()$Population[1])/9, 1), " people per year.")
  })
  
  delta.crime <- reactive({
    crime %>% filter(County == input[["county"]]) %>%
      mutate(roc = c(NA, diff(Rate)))
  })
  output$summary2 <- renderText({
    paste0("The average annual change in violent crime offenses in ", input[["county"]], " County is ", 
           round(mean(na.rm(abs(delta.crime()$roc))), 1), " per 1,000 people.")
  })
  
  # select county for area
  selectedData3 <- reactive({
    counties %>% filter(counties == input[["county"]])
  })
    
  output$summary3 <- renderText({
    paste0(input[["county"]], " County accounts for ", round(selectedData3()[4]/sum(counties$Area)*100,2), 
            "% of the total square miles of Kansas.")
  }) 

}

shinyApp(ui = ui, server = server)



