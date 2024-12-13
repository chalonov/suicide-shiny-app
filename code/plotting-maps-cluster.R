library(tidyverse)
library(readxl)
library(sf)
library(tmap)
library(tmaptools)

map_function_cluster <- function(my_event) {
  # cargar bases de datos
  db_cluster <- "./data/db-app-cluster.xlsx"
  shape_cluster <- "./data/shape/municipios.shp"
  
  # filtrar el evento
  data_cluster <- read_excel(db_cluster)
  column_names <- c("code_dep", "departamento", "code_mun", "municipio", "suicidio", "intento", "intox", "depresion")
  colnames(data_cluster)<- column_names
  
  if (my_event == "suicidio") {
    data_cluster = data.frame(data_cluster[1:4], data_cluster[5])
  }else if (my_event == "intento") {
    data_cluster = data.frame(data_cluster[1:4], data_cluster[6])
  }else if (my_event == "intox") {
    data_cluster = data.frame(data_cluster[1:4], data_cluster[7])
  }else if (my_event == "depresion") {
    data_cluster = data.frame(data_cluster[1:4], data_cluster[8])
  }
  colnames(data_cluster)<- c("code_dep", "departamento", "code_mun", "municipio", "my_cluster")
  
  
  # unir los datos y el .shp
  shape_file <- st_read(shape_cluster)
  shape_file <- shape_file |> 
    mutate(mpios = as.numeric(as.character(mpios))) |> 
    rename(COD_MUN = `mpios`)
  data_map_year = inner_join(x = shape_file, y = data_cluster, by = c('COD_MUN' = 'code_mun'))
  
  # generar el mapa
  
  if (my_event == "suicidio") {
    my_name <- "Suicidio"
    my_breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    my_labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9")
  }else if (my_event == "intento") {
    my_name <- "Intento suicida"
    my_breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    my_labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9")
  }else if (my_event == "depresion") {
    my_name <- "Depresión"
    my_breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
    my_labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19")
  }else if (my_event == "intox") {
    my_name <- "Intoxicación aguda"
    my_breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)
    my_labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23")
  }
  
  map_plot <- tm_shape(data_map_year) + 
    tm_polygons("my_cluster",
                id = "municipio", 
                title = my_name,
                popup.vars=c("Departamento: " = "departamento",
                             "Cluster: " = "my_cluster"),
                popup.format=list(departamento=list(digits=1),
                                  my_cluster=list(digits=0)),
                breaks = my_breaks,
                labels = my_labels,
                #alpha = 0.5,
                #n = 16,
                #border.col = "#990099",
                #border.alpha = 0.8,
                textNA = "NA",
                colorNA = "white",
                border.alpha = 0.1,
                showNA = FALSE,
                palette = "Paired")
  
  return(map_plot)
}

