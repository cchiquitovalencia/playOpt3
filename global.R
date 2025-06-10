# global.R
# Este archivo se ejecuta al iniciar la app. Carga librerías, datos globales y funciones utilitarias.

# ---- Carga de librerías ----
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(bslib)
library(dplyr)
library(rvest)
library(ggplot2)
library(tictoc)
library(stringr)
library(purrr)
library(tibble)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
#library(aws.s3)
#library(DBI)
#library(RSQLite)

# ---- Funciones utilitarias generales ----
# (Las funciones detalladas estarán en modules/mod_utils.R para modularidad)

# ---- Procesamiento de celdas y restricciones ----
source("modules/mod_utils.R") # Carga funciones auxiliares

# Función para cargar un escenario
cargar_escenario <- function(archivo_html) {
  game_html <- read_html(archivo_html)
  div_lotka_cells <- game_html %>% html_nodes("div.lotka-cell")
  
  # Procesa el escenario y obtiene los dataframes y restricciones
  escenario <- procesar_escenario(div_lotka_cells)
  final <- escenario$final
  elementos <- escenario$elementos
  simbolos <- escenario$simbolos
  restricciones <- escenario$restricciones
  not_equal_pairs <- escenario$not_equal_pairs
  equal_pairs <- escenario$equal_pairs
  valores_fijos <- escenario$valores_fijos
  
  # Modelo de optimización (solución óptima)
  n <- 6
  modelo <- construir_modelo(n, valores_fijos, not_equal_pairs, equal_pairs)
  resultado <- solve_model(modelo, with_ROI(solver = "glpk", verbose = TRUE))
  optimo <- obtener_solucion(resultado, n)
  optimo_1 <- optimo$optimo_1
  optimo_2 <- optimo$optimo_2
  
  # Conectores y utilidades de tablero
  conectores <- generar_conectores(restricciones)
  horizontal_connectors <- conectores$horizontal_connectors
  vertical_connectors <- conectores$vertical_connectors
  
  # Variables globales para el tablero
  n_rows <- 6
  n_cols <- 6
  special_cells <- obtener_special_cells(final)
  
  list(
    final = final,
    elementos = elementos,
    optimo_1 = optimo_1,
    optimo_2 = optimo_2,
    n_rows = n_rows,
    n_cols = n_cols,
    special_cells = special_cells,
    horizontal_connectors = horizontal_connectors,
    vertical_connectors = vertical_connectors
  )
}

# Función de color para celdas
cell_color <- function(i, j, value, special_cells) {
  key <- paste0(i, "_", j)
  if (key %in% names(special_cells)) {
    return(special_cells[[key]])
  }
  "#eff6fe"
}