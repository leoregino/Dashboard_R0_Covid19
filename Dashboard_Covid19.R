#### INSTALL PACKAGES ###
#install.packages("shiny")
#install.packages("EpiEstim")
#install.packages("NobBS")}
#install.packages("jsonlite")
#install.packages("tidyverse")

### LOAD LIBRARIES ###
library(shiny)
library(jsonlite)
library(tidyverse)

### DIRECTORY PARAMETERS ###
datos.path <- "C:/Users/Leonardo REGINO/Documents/DATAMINH/Covid19/Dashboards/Data"
datosImport <- fromJSON(txt = paste(datos.path, "Covid19_Colombia.json", sep = "/") )


ui <- fluidPage(
  
  titlePanel("Evolution of R0 - Covid19"),
  
  sidebarLayout(
    
    sidebarPanel(
  
      uiOutput("City_Filter"),
      uiOutput("Date_Range")
        
      ), 
      mainPanel(
        tableOutput("table")
      )
    )
  )



server <- function(input, output) {
  
  tab <- reactive({ 
    
    datosImport %>% 
      filter(ciudad_de_ubicaci_n == input$City) %>% 
      filter(fecha_de_notificaci_n > input$NotifDate[1] & fecha_de_notificaci_n < input$NotifDate[2] )
    
  })
  
  output$City_Filter <- renderUI({
  
    selectizeInput('City', 'Select a City', choices = c("Choose a city" = "", levels(factor(datosImport$ciudad_de_ubicaci_n) ) ) )
    
  })
  
  output$Date_Range <- renderUI({
    
    dateRangeInput('NotifDate',
                  label = 'Filter by Notification Date:',
                  start =  min(as.Date(datosImport$fecha_de_notificaci_n)) , end = max(as.Date(datosImport$fecha_de_notificaci_n))
    )
    
  })
  
  
  output$table <- renderTable({ 
    
    tab()
    
  })
  
}


shinyApp(ui = ui, server = server )

