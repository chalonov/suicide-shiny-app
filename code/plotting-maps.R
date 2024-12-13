library(tidyverse)
library(readxl)
library(sf)
library(tmap)
library(tmaptools)

db_departamental <- "./data/db-app-dep.xlsx"
db_municipal <- "./data/db-app-mun.xlsx"

shape_departamental <- './data/shape/departamentos.shp'
shape_municipal <- './data/shape/municipios.shp'

data_by_event <<- function(my_scope, my_filter, my_year) {
  
  if (my_scope == "Departamental") {
    data_raw <- read_excel(db_departamental)
    index_pos <- c(1:3)
    event_pos <- c(4, 7, 10, 13)
    column_names <- c("year","code_dep", "departamento", "suicidio", "intento", "intox", "depresion")
    table_column_names <- c("Año","Cod-Dep", "Departamento", "Mujeres", "Hombres", "Total")
    data_index <- data.frame(data_raw[index_pos])
    data_format <- data.frame(data_index, round(data.frame(data_raw[4:15]), 2)) 
  } else if (my_scope == "Municipal") {
    data_raw <- read_excel(db_municipal)
    index_pos <- c(1:5)
    event_pos <- c(6, 9, 12, 15)
    column_names <- c("year", "code_dep","departamento", "code_mun", "municipio", "suicidio", "intento", "intox", "depresion")
    table_column_names <- c("Año","Cod-Dep", "Departamento","Cod-Mun", "Municipio", "Mujeres", "Hombres", "Total")
    data_index <- data.frame(data_raw[1:5])
    data_format <- data.frame(data_index, round(data.frame(data_raw[6:17]), 2))
  }
  # posiciones en columna del evento para cada grupo
  women_pos <- event_pos 
  men_pos <- event_pos + 1
  total_pos <- event_pos + 2
  
  # datos mujeres
  women <- data.frame(data_index, data_format[women_pos]) 
  colnames(women)<- column_names
  women <- data.frame(women[index_pos], women[my_filter]) |> 
    subset(year == my_year)
  
  # datos hombres
  men <- data.frame(data_index, data_format[men_pos]) 
  colnames(men)<- column_names
  men <- data.frame(men[index_pos], men[my_filter]) |> 
    subset(year == my_year)
  
  # datos total
  total <- data.frame(data_index, data_format[total_pos]) 
  colnames(total)<- column_names
  total <- data.frame(total[index_pos], total[my_filter]) |> 
    subset(year == my_year)
  
  # Diccionario de eventos por grupo
  
  data_event <- list("index" = total[index_pos], "women" = women, "men" = men, "population" = total)
  
  table_data_event <- data.frame(total[index_pos], women[my_filter], men[my_filter], total[my_filter])
  colnames(table_data_event)<- table_column_names
  
  #return(data_event)
  return(table_data_event)
  
}

# Funcion: Formatear y renombrar el shapefile; unirlo con los datos de observaciones
data_to_map <- function(my_data_by_event, my_scope_area) {
  
  if (my_scope_area == "Departamental") {
    shape_file <- st_read(shape_departamental)
    shape_file <- shape_file |> 
      mutate(ID_ESPACIA = as.numeric(as.character(ID_ESPACIA))) |> 
      rename(COD_DEP = `ID_ESPACIA`)
    data_map_year = inner_join(x = shape_file, y = my_data_by_event, by = c('COD_DEP' = 'Cod-Dep'))
  } else if (my_scope_area == "Municipal") {
    shape_file <- st_read(shape_municipal)
    shape_file <- shape_file |> 
      mutate(mpios = as.numeric(as.character(mpios))) |> 
      rename(COD_MUN = `mpios`)
    data_map_year = inner_join(x = shape_file, y = my_data_by_event, by = c('COD_MUN' = 'Cod-Mun'))
  }
  
  return(data_map_year)
}

# Funcion: Encontrar el vector de los breaks para cada evento
data_breaks <- function(data_event, my_group) {
  if (my_group == "Mujeres") {
    group <- data_event$Mujeres
  }else if (my_group == "Hombres") {
    group <- data_event$Hombres
  } else if (my_group == "Total") {
    group <- data_event$Total
  }
  
  my_group_max <- max(group)
  
  rmax <- round(my_group_max, -1)
  if (rmax < 25) {
    seq_array <- seq(0, rmax, by = 2)
    seq_array[1] <- 1
    my_breaks <- c(-Inf, seq_array, Inf)
  }else if (rmax >= 25 & rmax < 80) {
    seq_array <- seq(0, rmax, by = 5)
    seq_array[1] <- 1
    my_breaks <- c(-Inf, seq_array, Inf)
  }else if (rmax >= 80 & rmax < 150) {
    seq_array <- seq(0, rmax, by = 10)
    seq_array[1] <- 1
    my_breaks <- c(-Inf, seq_array, Inf)
  }else if (rmax >= 150 & rmax < 250) {
    seq_array <- seq(0, rmax, by = 20)
    seq_array[1] <- 1
    my_breaks <- c(-Inf, seq_array, Inf)
  }else if (rmax >= 250 & rmax < 500) {
    seq_array <- seq(0, rmax, by = 50)
    seq_array[1] <- 1
    my_breaks <- c(-Inf, seq_array, Inf)
  }else if (rmax >= 500 & rmax < 1000) {
    seq_array <- seq(0, rmax, by = 100)
    seq_array[1] <- 1
    my_breaks <- c(-Inf, seq_array, Inf)
  }else if (rmax >= 1000 & rmax < 1500) {
    seq_array <- seq(0, rmax, by = 150)
    seq_array[1] <- 1
    my_breaks <- c(-Inf, seq_array, Inf)
  }else if (rmax >= 1500) {
    seq_array <- seq(0, rmax, by = 200)
    seq_array[1] <- 1
    my_breaks <- c(-Inf, seq_array, Inf)
  }
  
  return(my_breaks)
}

# Funcion: crea el vector de labels para la leyenda de los mapas
labels_function <- function(my_breaks) {
  
  len_my_breaks <- length(my_breaks)
  
  my_labels <- c()
  count <- 2
  
  for (item in my_breaks[2:(len_my_breaks - 2)]) {
    my_labels[count] <- paste0(my_breaks[count], "-", my_breaks[count + 1] )
    count <- count + 1
  }
  my_labels[1] <- "< 1"
  my_labels[len_my_breaks - 1] <- paste0("> ", my_breaks[len_my_breaks - 1] )
  
  return(my_labels)
  
}


# Funcion: Plotear el mapa segun el archivo unido
plotting_map_by_year <- function(my_map_data, my_group, my_name, my_scope, my_breaks, my_year, my_labels) {
  
  if (my_scope == "Departamental") {
    my_area <- "Departamento"
  }else if (my_scope == "Municipal") {
    my_area <- "Municipio"
  }
  
  if (my_name == "suicidio") {
    my_name <- "Suicidio"
  }else if (my_name == "intento") {
    my_name <- "Intento suicida"
  }else if (my_name == "depresion") {
    my_name <- "Depresión"
  }else if (my_name == "intox") {
    my_name <- "Intoxicación aguda"
  }
  
  if (my_year == 9999) {
    my_year <- "2009-2018"
  }else 
    my_year <- my_year
  
  map_plot <- tm_shape(my_map_data) + 
    tm_polygons(my_group,
                id = my_area, 
                title = my_name,
                popup.vars=c("Tasa total población: "="Total",
                             "Tasa hombres: "="Hombres",
                             "Tasa mujeres: "="Mujeres"),
                popup.format=list(Total=list(digits=1),
                                  Hombres=list(digits=1),
                                  Mujeres=list(digits=1)),
                breaks = my_breaks,
                labels = my_labels,
                #alpha = 0.5,
                #n = 16,
                #border.col = "#990099",
                #border.alpha = 0.8,
                palette = "viridis") +
    tm_layout(title = paste("Nivel:", my_scope, "<br>Población:", my_group, "<br>Periodo:", my_year))
  
  return(map_plot)
}

# Funcion: Función principal -> ejecutar todas las funciones
map_function <- function(my_scope, my_event, my_group, my_year, my_name) {
  
  data_to_plot <- data_to_map(data_by_event(my_scope, my_event, my_year), my_scope)
  
  my_breaks <- data_breaks(data_by_event(my_scope, my_event, my_year), my_group)
  
  map_to_show <- plotting_map_by_year(data_to_plot, my_group, my_name, my_scope, my_breaks, my_year, labels_function(my_breaks))
  
  return(map_to_show)
}
