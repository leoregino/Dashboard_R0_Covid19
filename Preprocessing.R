
library(shiny)
library(dplyr)
library(jsonlite)
library(tidyverse)
library(ggplot2)
library(EpiEstim)


url <- "https://www.datos.gov.co/resource/gt2j-8ykr.json?$limit="
nb_limit <- "10000"
datosImport <- fromJSON(paste0(url,nb_limit))

total_row <- nrow(datosImport)

col_NA <- data.frame( matrix(ncol = 2, nrow = 0) )
colnames(col_NA) <- c("Variable", "Nb_NA") 


#col_to_treat <- c() # Columns having NA, to be processed
for (nom_col in colnames(datosImport)){
    nro_NA <- nrow(datosImport[is.na.data.frame(datosImport[nom_col]), ]) 
    if (nro_NA>0){
      #col_to_treat <- c(col_to_treat,nom_col) # Append column's name to process 
      col_NA[nrow(col_NA) + 1,] = c(nom_col, nro_NA/total_row*100 )
    }
    #print(paste0(nom_col, str(nro_NA)))
}
col_NA$Nb_NA <- as.numeric(col_NA$Nb_NA) 

# Plot % of missing values in columns
p <- ggplot(data = col_NA, aes(x = Variable, y = Nb_NA) ) +
  geom_bar(stat="identity", fill = "blue") +
  geom_text(aes(label=Nb_NA), vjust = 1.6, color = "black", size = 3) +
  labs(title = "Number of NA per variable")

p

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






a <- data[26:50,] 

for (i in 1:nrow(a) ){
  print( is.na.data.frame(a[i,"fis"]) )
}


#############3 
library(EpiEstim)

data(Flu2009)
datosImport$nb <-1 

mis_datos <- datosImport %>% 
              filter(departamento == "Bogotá D.C." ) %>%
              filter(ciudad_de_ubicaci_n == "Bogotá D.C." ) %>%
              group_by(fecha_de_notificaci_n) %>% 
              summarise_if(is.numeric, sum, na.rm=TRUE)

mis_datos <- as.data.frame(mis_datos) 
mis_datos$fecha_de_notificaci_n <- as.Date(mis_datos$fecha_de_notificaci_n)

mis_datos <- mis_datos[seq(4, nrow(mis_datos)) , ]


mis_datos <- mis_datos %>% 
  rename(
    dates = fecha_de_notificaci_n,
    I = nb
  )


res_parametric_si <- estimate_R(mis_datos, 
                               method="parametric_si",
                               config = make_config(list(
                                 mean_si = 2.6, 
                                 std_si = 1.5))
)


  
rest <- estimate_R(
  mis_datos,
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




################################

rest <- estimate_R(
  d$local,
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

death_df <- datosImport %>%
  filter(atenci_n == "Fallecido") %>%
  group_by(fecha_de_notificaci_n) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)
d$type <- "Contaminado"
death_df$type <- "Fallecido"
################################



## Deploy 
library(rsconnect)
rsconnect::deployApp('C:/Users/Leonardo REGINO/Documents/DATAMINH/Covid19/Dashboards')
