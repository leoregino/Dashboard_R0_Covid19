#### INSTALL PACKAGES ###
#install.packages("shiny")
#install.packages("EpiEstim")
#install.packages("NobBS")}
#install.packages("jsonlite")
#install.packages("tidyverse")
#install.packages("ggplot2")

### LOAD LIBRARIES ###
library(shiny)
library(dplyr)
library(jsonlite)
library(tidyverse)
library(ggplot2)


### DIRECTORY PARAMETERS ###
datos.path <- "C:/Users/Leonardo REGINO/Documents/DATAMINH/Covid19/Dashboards/Data"
datosImport <- fromJSON(txt = paste(datos.path, "Covid19_Colombia.json", sep = "/") )
datosImport$nb <- 1

datosImport$fecha_notif <-  as.character(as.Date(datosImport$fecha_de_notificaci_n))


ui <- fluidPage(
  
  titlePanel("Evolution of R0 - Covid19"),
  
  sidebarLayout(
    
    sidebarPanel(
  
      uiOutput("Dept_Filter"),
      uiOutput("City_Filter"),
      uiOutput("Date_Range")
        
      ), 
      mainPanel(
        #tableOutput("table")
        tableOutput("Data_Covid19"),
        plotOutput("ts_plot")
      )
    )
  )



server <- function(input, output) {
  data_agg <- reactive({ 
    
    datosImport %>% 
      filter(departamento == input$Depto) %>% 
      filter(ciudad_de_ubicaci_n == input$City) %>% 
      filter(fecha_de_notificaci_n > input$NotifDate[1] & fecha_de_notificaci_n < input$NotifDate[2] )
    
  })
  
  
  output$Dept_Filter <- renderUI({
    
    selectizeInput('Depto', 'Select a Department', choices = c("Choose a Department" = "", levels(factor(datosImport$departamento) ) ) )
    
  })
  
  output$City_Filter <- renderUI({
  
    selectizeInput('City', 'Select a City', choices = c("Choose a city" = "", levels(factor(datosImport[ datosImport$departamento ==  input$Depto, 'ciudad_de_ubicaci_n'] ) ) ) )    
    
  })
  
  output$Date_Range <- renderUI({
    
    dateRangeInput('NotifDate',
                  label = 'Filter by Notification Date:',
                  start =  min(as.Date(datosImport$fecha_de_notificaci_n)) , end = max(as.Date(datosImport$fecha_de_notificaci_n))
    )
    
  })
  
  output$Data_Covid19 <- renderTable({
    #data_agg() %>% group_by(fecha_notif) %>% summarize(SUM = sum(nb))
    dataset <- data_agg() %>% group_by(fecha_notif) %>% summarize(SUM = sum(nb))
    dataset
  })

  
  # plot time series
  output$ts_plot <- renderPlot({
    
    mis_datos <- datosImport %>% 
                  filter(departamento == input$Depto) %>% 
                  filter(ciudad_de_ubicaci_n == input$City) %>% 
                  filter(fecha_de_notificaci_n > input$NotifDate[1] & fecha_de_notificaci_n < input$NotifDate[2] )
    
    mis_datos_agg <- mis_datos %>% group_by(fecha_notif) %>% summarize(SUM = sum(nb))
    plot(x = mis_datos_agg$fecha_notif, y = mis_datos_agg$SUM)
    
  })
  
  
}


shinyApp(ui = ui, server = server )

