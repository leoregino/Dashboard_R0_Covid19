#### INSTALL PACKAGES ###
#install.packages("shiny")
#install.packages("EpiEstim")
#install.packages("NobBS")}
#install.packages("jsonlite")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("rsconnect") ## Connect with ShinyApps

### LOAD LIBRARIES ###
library(shiny)
library(dplyr)
library(jsonlite)
library(tidyverse)
library(ggplot2)
library(EpiEstim)

### DIRECTORY PARAMETERS ###

# Load from local directory
### 1. Load file from local ###
file_name <- "C:/Users/Leonardo REGINO/Documents/DATAMINH/Covid19/Dashboards/Data/Data_Covid19_Colombia.csv"
DataCovid19 <-  read.csv(file = file_name, sep = ",", header = TRUE )


### 4. Aggregate data ###
DataCovid19 <- cbind(DataCovid19, data.frame(nb= rep(1,nrow(DataCovid19)) ) )

DataCovid19$fis <- as.character.Date(DataCovid19$fis)

ui <- fluidPage(
  titlePanel("Evolution of R0 - Covid19"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("Dept_Filter"),
      uiOutput("City_Filter"),
      uiOutput("Date_Range")
      ), 
      mainPanel(
        plotOutput("ts_plot"),
        #tableOutput("test_data")
      )
    )
  )



server <- function(input, output) {
  
  output$Dept_Filter <- renderUI({
    selectizeInput('Depto', 'Select a Department', choices = c("Choose a Department" = "", levels(factor(DataCovid19$departamento) ) ) )
  })
  
  output$City_Filter <- renderUI({
    selectizeInput('City', 'Select a City', choices = c("Choose a city" = "", levels(factor(DataCovid19[ DataCovid19$departamento ==  input$Depto, 'ciudad_de_ubicaci_n'] ) ) ) )    
  })
  
  output$Date_Range <- renderUI({
    dateRangeInput('NotifDate',
                  label = 'Filter by Notification Date:',
                  start =  min(as.character.Date(DataCovid19$fis)) , end = max(as.character.Date(DataCovid19$fis))
                  )
  })
  
  #Filter data
  data_filt <- reactive({
    
    if (!is.null(input$Depto) & !is.null(input$City) ) {
      
      d = DataCovid19 %>%
        filter(departamento == input$Depto) %>% 
        filter(ciudad_de_ubicaci_n == input$City) %>% 
        filter(fis >= input$NotifDate[1]) %>% 
        filter(fis <= input$NotifDate[2] )
    }
    
    else{
      d = DataCovid19
    }
    
    d
  })
  
  ## Aggregate data
  data_aggr <- reactive({
    data <- data_filt()
    data <- data %>% rename(dates = fis, I = nb)
    data <- data %>% group_by(dates) %>% summarise(I = sum(I))
    data <- data[seq(5, nrow(data)),] # The first dates contain holes, which generates errors while computeing Rt
    data <- as.data.frame(data)
    data$dates <- as.Date.character(data$dates)
    data
  })
  
  
  
  # plot time series
  output$ts_plot <- renderPlot({
    
    data <- data_aggr() 
    
    rest <- estimate_R(
      data,
      method = "uncertain_si",
      config = make_config(
        list(
          mean_si = 2.6, 
          std_mean_si = 1,
          min_mean_si = 1, 
          max_mean_si = 4.2,
          std_si = 1.5, 
          std_std_si = 0.5,
          min_std_si = 0.5, 
          max_std_si = 2.5,
          n1 = 100, n2 = 100
        )
      )
    )
    
    plot(rest)
    
  })

  # Render Table to test data
   output$test_data <- renderTable({
     #data_filt()
     data_aggr()
   })

}


shinyApp(ui = ui, server = server )

