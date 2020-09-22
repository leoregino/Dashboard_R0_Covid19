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
#file_path <- "C:/Users/Leonardo REGINO/Documents/DATAMINH/Covid19/Dashboards/Data"
#file_name <- "Covid19_Colombia.json"
#datosImport <- fromJSON(txt = paste(file_path, file_name, sep = "/") )

url <- "https://www.datos.gov.co/resource/gt2j-8ykr.json?$limit="
nb_limit <- "10000"
datosImport <- fromJSON(paste0(url,nb_limit))


############ TREAT THE MISSING VALUES ############

# 1. [pa_s_de_procedencia]: If NA --> Assign = COLOMBIA
datosImport[is.na.data.frame(datosImport$pa_s_de_procedencia), ]$pa_s_de_procedencia <- "COLOMBIA"

# 2. [fis]: If NA --> Assign = fecha_diagnostico - 10
datosImport[is.na.data.frame(datosImport$fis), ]$fis <- datosImport[is.na.data.frame(datosImport$fis), ]$fecha_diagnostico

# 3. [fecha_diagnostico]: If NA --> Assign = 0001-01-01T00:00:00.000
datosImport[is.na.data.frame(datosImport$fecha_diagnostico), ]$fecha_diagnostico <-  "0001-01-01T00:00:00.000"

# 4. [fecha_recuperado]: IF NA --> 0001-01-01T00:00:00.000
datosImport[is.na.data.frame(datosImport$fecha_recuperado), ]$fecha_recuperado <-  "0001-01-01T00:00:00.000"

# 5. [tipo_recuperaci_n]: If NA (Vivo o Fallecido) --> Assign = "NA" 
datosImport[is.na.data.frame(datosImport$tipo_recuperaci_n), ]$tipo_recuperaci_n <-  "NA" 

# 6. [codigo_pais]: If NA (COLOMBIA) --> Assign = 170 (code ISO 3166-1)
datosImport[is.na.data.frame(datosImport$codigo_pais), ]$codigo_pais <-  170 

# 7. [nombre_grupo_etnico]: IF NA <-- Assign = "Otro"
datosImport[is.na.data.frame(datosImport$nombre_grupo_etnico), ]$nombre_grupo_etnico <-  "Otro" 

# 8. [fecha_de_muerte]: IF NA (vivo o Recuperado ) <-- Assign = 0001-01-01T00:00:00.000 
datosImport[is.na.data.frame(datosImport$fecha_de_muerte), ]$fecha_de_muerte <-  "0001-01-01T00:00:00.000" 

# datosImport[is.na.data.frame(datosImport$fis), ]$fis <- datosImport[is.na.data.frame(datosImport$fis), ]$fecha_diagnostico
# datosImport[is.na.data.frame(datosImport$pa_s_de_procedencia), ]$pa_s_de_procedencia <- "COLOMBIA"

#datosImport$nb <- 1 # To be able to count/aggregate

#datosImport$fis <- as.Date(datosImport$fis)

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
  
  data <- reactive({

    
    if (!is.null(input$Depto) & !is.null(input$City) ) {
      
      d = datosImport %>%
        filter(departamento == input$Depto) %>% 
        filter(ciudad_de_ubicaci_n == input$City) %>% 
        filter(fis > input$NotifDate[1] & fis < input$NotifDate[2] )
    }
    
    else{
      d = datosImport
    }
    # d = d %>% 
    #   group_by(fis) %>% 
    #   summarise_if(is.numeric, sum, na.rm=TRUE)
    
    d[,"nb"] <- NA
    d[,"nb"] <- 1
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
                  start =  min(as.Date(datosImport$fis)) , end = max(as.Date(datosImport$fis))
    )
    
  })
  
  # plot time series
  output$ts_plot <- renderPlot({
    
    data <- data() 
    data$fis <- as.Date(data$fis)
    
    
    data <- data %>% 
      group_by(fis) %>% 
      summarise(nb = sum(nb))
    
    
    data <- data[seq(4, nrow(data)),]
    data <- as.data.frame(data)
    
    
    data <- data %>%
      rename(
        dates = fis,
        I = nb
      )
    
    
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
  
  # output$test_data <- renderTable({
  #   data <- data()
  #   
  #   data
  # })

}


shinyApp(ui = ui, server = server )

