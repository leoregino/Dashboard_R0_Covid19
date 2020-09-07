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
datosImport$nb <-1 

datosImport$fecha_de_notificaci_n <-  as.character(as.Date(datosImport$fecha_de_notificaci_n))


ui <- fluidPage(
  
  titlePanel("Evolution of R0 - Covid19"),
  
  sidebarLayout(
    
    sidebarPanel(
      uiOutput("Dept_Filter"),
      uiOutput("City_Filter"),
      uiOutput("Date_Range")
      ), 
      mainPanel(
        plotOutput("ts_plot")
      )
    )
  )



server <- function(input, output) {
  
  # data_agg <- reactive({ 
  #   if (!is.null(input$Depto) & !is.null(input$City) ) {
  #     datosImport %>% 
  #       filter(departamento == input$Depto) %>% 
  #       filter(ciudad_de_ubicaci_n == input$City) %>% 
  #       filter(fecha_de_notificaci_n >= input$NotifDate[1] & fecha_de_notificaci_n <= input$NotifDate[2] )
  #   }
  #   else {
  #     datosImport
  #   }
  #   
  # })
  
  
  data <- reactive({
  
    if (!is.null(input$Depto) & !is.null(input$City) ) {
      d = datosImport %>%
        filter(departamento == input$Depto) %>% 
        filter(ciudad_de_ubicaci_n == input$City) %>% 
        filter(fecha_de_notificaci_n > input$NotifDate[1] & fecha_de_notificaci_n < input$NotifDate[2] )
    }
    
    else{
      d = datosImport
    }
    d = d %>% 
      group_by(fecha_de_notificaci_n) %>% 
      summarise_if(is.numeric, sum, na.rm=TRUE)
    
    d
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
  
  # plot time series
   output$ts_plot <- renderPlot({
     data <- data()
     data$fecha_de_notificaci_n <- as.Date(data$fecha_de_notificaci_n)

     ggplot(data, aes(x = fecha_de_notificaci_n, y = nb))+
       geom_line() +
       geom_point() +
       labs(x = "Fecha notificacion", title = "Numero de contaminados") +
       theme(plot.title = element_text(hjust = 0.5))
   })
  
}


shinyApp(ui = ui, server = server )

