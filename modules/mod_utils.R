# modules/mod_utils.R
# Funciones auxiliares para procesamiento de escenario, restricciones y modelo

# ---- Extrae información de cada celda del HTML ----
define_cell <- function(nodo) {
  evaluacion <- nodo |> html_nodes("div") |> html_attrs()
  es_fijo <- str_detect(evaluacion[[1]], "locked")
  
  if(length(evaluacion) == 1){
    sentido_1 <- NA; signo_1 <- NA
    sentido_2 <- NA; signo_2 <- NA
  } else if(length(evaluacion) == 2){
    sentido_1 <- sub(".*--", "", evaluacion[[2]])
    signo_1 <- nodo |> html_nodes("svg") |> html_attrs()
    signo_1 <- signo_1[[2]] |> as.matrix() |> as.data.frame() |> rownames_to_column() |> filter(rowname == "aria-label")
    signo_1 <- signo_1$V1[1]
    sentido_2 <- NA; signo_2 <- NA
  } else if(length(evaluacion) == 3){
    sentido_1 <- sub(".*--", "", evaluacion[[2]])
    signo_1 <- nodo |> html_nodes("svg") |> html_attrs()
    signo_1 <- signo_1[[2]] |> as.matrix() |> as.data.frame() |> rownames_to_column() |> filter(rowname == "aria-label")
    signo_1 <- signo_1$V1[1]
    sentido_2 <- sub(".*--", "", evaluacion[[3]])
    signo_2 <- nodo |> html_nodes("svg") |> html_attrs()
    signo_2 <- signo_2[[3]] |> as.matrix() |> as.data.frame() |> rownames_to_column() |> filter(rowname == "aria-label")
    signo_2 <- signo_2$V1[1]
  }
  
  # Imagen principal de la celda
  if(length(nodo |> html_elements("svg") |> html_attrs()) > 1){
    img <- nodo |> html_elements("svg") |> html_attrs()
    img <- img[[1]] |> unlist() |> as.matrix() |> as.data.frame() |> rownames_to_column() |> filter(rowname == "aria-label")
    img <- img$V1[1]
  } else {
    img <- nodo |> html_elements("svg") |> html_attrs() |> unlist() |> as.matrix() |> as.data.frame() |> rownames_to_column() |> filter(rowname == "aria-label")
    img <- img$V1[1]
  }
  
  return(list(img = img,
              fijo = es_fijo,
              sentido_1 = sentido_1,
              signo_1 = signo_1,
              sentido_2 = sentido_2,
              signo_2 = signo_2))
}

# ---- Procesa el escenario y obtiene dataframes y restricciones ----
procesar_escenario <- function(lotka_cells) {
  final <- lapply(lotka_cells, define_cell) |> bind_rows()
  final <- data.frame(final)
  final <- final |> rowid_to_column() |> mutate(eje_x = rep(1:6, each = 6), eje_y = rep(1:6, times = 6))
  
  elementos <- final |> filter(fijo == TRUE) |> group_split(img)
  simbolos <- final  |> filter(!is.na(signo_1)) |> group_split(signo_1)
  simbolos <- rbind(bind_rows(simbolos),
                    bind_rows(simbolos) |> filter(!is.na(sentido_2)) |> mutate(sentido_1 = sentido_2, signo_1 = signo_2)
  )
  simbolos <- simbolos  |> filter(!is.na(signo_1)) |> group_split(signo_1)
  
  # Función para añadir columnas calculadas
  convertir_simbolos <- function(simbolo) {
    simbolo |> mutate(
      nx = eje_x + as.integer(sentido_1 == "down"),
      ny = eje_y + as.integer(sentido_1 == "right")
    )
  }
  simbolos_convertidos <- simbolos |> map(convertir_simbolos)
  todos <- bind_rows(simbolos_convertidos)
  cruces <- todos |> filter(signo_1 == "Cross")
  iguales <- todos |> filter(signo_1 %in% c("Equal", "Igual"))
  restricciones <- bind_rows(cruces, iguales)
  
  # Función para generar listas de pares desde filas
  generate_pairs <- function(df, tipo) {
    df %>%
      filter(str_detect(signo_1, tipo)) %>%
      rowwise() %>%
      mutate(pair = list(list(c(eje_x, eje_y), c(nx, ny)))) %>%
      ungroup() %>%
      pull(pair)
  }
  not_equal_pairs <- generate_pairs(restricciones, "Cross")
  equal_pairs     <- generate_pairs(restricciones, "Equal|Igual")
  
  # Valores fijos: "Moon" = 1, "Sun" = 2
  valores_fijos <- final %>%
    filter(fijo == TRUE) %>%
    mutate(valor = ifelse(img == "Moon" | img == "Luna", 1L, 2L))
  
  list(
    final = final,
    elementos = elementos,
    simbolos = simbolos,
    restricciones = restricciones,
    not_equal_pairs = not_equal_pairs,
    equal_pairs = equal_pairs,
    valores_fijos = valores_fijos
  )
}

# ---- Construcción del modelo de optimización ----
construir_modelo <- function(n, valores_fijos, not_equal_pairs, equal_pairs) {
  model <- MIPModel() %>%
    add_variable(x[i, j, k], i = 1:n, j = 1:n, k = 1:2, type = "binary") %>%
    set_objective(0) %>%
    add_constraint(sum_over(x[i, j, k], k = 1:2) == 1, i = 1:n, j = 1:n) %>%
    add_constraint(sum_over(x[i, j, k], j = 1:n) == 3, i = 1:n, k = 1:2) %>%
    add_constraint(sum_over(x[i, j, k], i = 1:n) == 3, j = 1:n, k = 1:2) %>%
    add_constraint(x[i, j, k] + x[i, j + 1, k] + x[i, j + 2, k] <= 2, i = 1:n, j = 1:(n - 2), k = 1:2) %>%
    add_constraint(x[i, j, k] + x[i + 1, j, k] + x[i + 2, j, k] <= 2, i = 1:(n - 2), j = 1:n, k = 1:2)
  
  # Restricciones de valores fijos
  for (row in seq_len(nrow(valores_fijos))) {
    i <- valores_fijos$eje_x[row]
    j <- valores_fijos$eje_y[row]
    k_fijo <- valores_fijos$valor[row]
    model <- model %>%
      add_constraint(x[i, j, k_fijo] == 1)
  }
  
  # Función general para aplicar restricciones entre pares
  add_pair_constraints <- function(model, pairs, operator, n) {
    walk(pairs, function(pair) {
      coords <- unlist(pair)
      if (any(coords < 1 | coords > n)) stop("Coordenadas fuera del rango 1..n")
      i1 <- pair[[1]][1]; j1 <- pair[[1]][2]
      i2 <- pair[[2]][1]; j2 <- pair[[2]][2]
      for (k in 1:2) {
        if (operator == "!=") {
          model <<- model %>%
            add_constraint(x[i1, j1, k] + x[i2, j2, k] <= 1)
        } else if (operator == "==") {
          model <<- model %>%
            add_constraint(x[i1, j1, k] == x[i2, j2, k])
        } else {
          stop("Operador no soportado: usa '==' o '!='.")
        }
      }
    })
    return(model)
  }
  
  model <- model %>%
    add_pair_constraints(not_equal_pairs, "!=", n) %>%
    add_pair_constraints(equal_pairs, "==", n)
  
  model
}

# ---- Obtención de la solución óptima ----
obtener_solucion <- function(result, n) {
  optimo <- result %>%
    get_solution(x[i,j,k]) %>%
    filter(value > 0) %>%
    select(i, j, k) %>%
    tidyr::spread(j, k) %>%
    select(-i) |> as.data.frame() |> as.matrix()
  
  optimo_1 <- optimo
  optimo_2 <- optimo
  optimo_1[optimo_1 == 1] <- "Moon"
  optimo_1[optimo_1 == 2] <- "Sun"
  optimo_1 <- matrix(optimo_1, nrow = n, byrow = FALSE)
  optimo_2[optimo_2 == 2] <- "Moon"
  optimo_2[optimo_2 == 1] <- "Sun"
  optimo_2 <- matrix(optimo_2, nrow = n, byrow = FALSE)
  
  list(optimo_1 = optimo_1, optimo_2 = optimo_2)
}

# ---- Generación de conectores ----
generar_conectores <- function(df) {
  horizontal_connectors <- df %>%
    filter(sentido_1 == "right", !is.na(signo_1)) %>%
    transmute(
      row = eje_x,
      col1 = eje_y,
      col2 = ny,
      symbol = ifelse(signo_1 %in% c("Equal","Igual"), "=", "X")
    ) %>%
    purrr::transpose()
  
  vertical_connectors <- df %>%
    filter(sentido_1 == "down", !is.na(signo_1)) %>%
    transmute(
      col = eje_y,
      row1 = eje_x,
      row2 = nx,
      symbol = ifelse(signo_1 %in% c("Equal","Igual"), "=", "X")
    ) %>%
    purrr::transpose()
  
  list(
    horizontal_connectors = horizontal_connectors,
    vertical_connectors = vertical_connectors
  )
}

# ---- Obtención de celdas especiales (colores fijos) ----
obtener_special_cells <- function(final) {
  final %>%
    filter(fijo == TRUE) %>%
    mutate(valor = ifelse(img %in% c("Moon", "Luna", "Sun", "Sol"), "#889fbf", NA_character_)) %>%
    filter(!is.na(valor)) %>%
    transmute(clave = paste0(eje_x, "_", eje_y), valor) %>%
    tibble::deframe()
}