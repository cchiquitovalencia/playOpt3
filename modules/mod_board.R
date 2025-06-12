# modules/mod_board.R
# Módulo Shiny para el tablero de juego

# ---- UI del módulo ----
mod_board_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "display: flex; justify-content: center;",
        uiOutput(ns("board_ui"))
    ),
    br(),
    div(style = "text-align: center; font-size: 18px; font-weight: bold; color: #1e2c46;",
        "⏱ Tiempo: ", span(id = ns("timer"), "00:00")
    ),
    br(),
    div(style = "text-align: center;",
        uiOutput(ns("reset_button"))
    ),
    br(),
    uiOutput(ns("stats")),
    textOutput(ns("test"))
  )
}

# ---- Server del módulo ----
mod_board_server <- function(id, optimo_1, optimo_2, n_rows, n_cols, elementos, special_cells,
                             horizontal_connectors, vertical_connectors, cell_color, tiempo_inicial, elegido) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cell_states <- c("", "Moon", "Sun")
    initial_matrix <- matrix("", nrow = n_rows, ncol = n_cols)
    
    # Pre-calcular valores fijos
    initial_fixed_cells <- 0
    for (tbl in elementos) {
      for (i in seq_len(nrow(tbl))) {
        fila <- tbl$eje_x[i]
        columna <- tbl$eje_y[i]
        icono <- if (tbl$img[i] %in% c("Luna", "Moon")) "Moon" else if (tbl$img[i] %in% c("Sol", "Sun")) "Sun" else ""
        initial_matrix[fila, columna] <- icono
        if (icono != "") initial_fixed_cells <- initial_fixed_cells + 1
      }
    }
    
    # Pre-calcular conectores para acceso rápido
    horizontal_connectors_map <- setNames(
      lapply(1:n_rows, function(i) {
        Filter(function(x) x$row == i, horizontal_connectors)
      }),
      as.character(1:n_rows)
    )
    
    vertical_connectors_map <- setNames(
      lapply(1:n_cols, function(j) {
        Filter(function(x) x$col == j, vertical_connectors)
      }),
      as.character(1:n_cols)
    )
    
    # Variables reactivas
    board <- reactiveVal(initial_matrix)
    previous_board <- reactiveVal(initial_matrix)
    click_count <- reactiveVal(0)
    reset_count <- reactiveVal(0)
    click_timestamps <- reactiveVal(data.frame(timestamp = numeric(0), diff = numeric(0)))
    initial_fixed_cells <- sum(initial_matrix != "")
    user_filled_cells <- reactiveVal(0)
    last_changed_cell <- reactiveVal(NULL)
    game_won <- reactiveVal(FALSE)
    
    # Registrar inicio de sesión
    session_id <- session$token
    session_start_time <- as.POSIXct(Sys.time(), tz = "UTC")
    last_save_time <- reactiveVal(as.numeric(Sys.time()))
    
    # Cache para resultados de validación
    validation_cache <- reactiveValues(
      highlights = matrix(FALSE, nrow = n_rows, ncol = n_cols),
      full_highlights = matrix(FALSE, nrow = n_rows, ncol = n_cols),
      connector_violations = matrix(FALSE, nrow = n_rows, ncol = n_cols)
    )
    
    # Función optimizada para encontrar matches
    find_matches <- function(mat) {
      to_highlight <- matrix(FALSE, nrow = n_rows, ncol = n_cols)
      
      # Optimización: usar vectorización en lugar de bucles anidados
      for (i in 1:n_rows) {
        row_vals <- mat[i,]
        for (j in 1:(n_cols-2)) {
          if (row_vals[j] != "" && row_vals[j] == row_vals[j+1] && row_vals[j] == row_vals[j+2]) {
            to_highlight[i, j:(j+2)] <- TRUE
          }
        }
      }
      
      for (j in 1:n_cols) {
        col_vals <- mat[,j]
        for (i in 1:(n_rows-2)) {
          if (col_vals[i] != "" && col_vals[i] == col_vals[i+1] && col_vals[i] == col_vals[i+2]) {
            to_highlight[i:(i+2), j] <- TRUE
          }
        }
      }
      
      to_highlight
    }
    
    # Función optimizada para encontrar matches completos
    find_full_matches <- function(mat) {
      full_highlight <- matrix(FALSE, nrow = n_rows, ncol = n_cols)
      
      # Optimización: usar table() una sola vez por fila/columna
      for (i in 1:n_rows) {
        counts <- table(mat[i, mat[i,] != ""])
        if (any(counts >= 4)) {
          full_highlight[i,] <- TRUE
        }
      }
      
      for (j in 1:n_cols) {
        counts <- table(mat[mat[,j] != "", j])
        if (any(counts >= 4)) {
          full_highlight[,j] <- TRUE
        }
      }
      
      full_highlight
    }
    
    # Función optimizada para encontrar violaciones de conectores
    find_connector_violations <- function(mat) {
      invalid <- matrix(FALSE, nrow = n_rows, ncol = n_cols)
      
      # Usar los mapas pre-calculados
      for (i in 1:n_rows) {
        conns <- horizontal_connectors_map[[as.character(i)]]
        for (conn in conns) {
          val1 <- mat[i, conn$col1]
          val2 <- mat[i, conn$col2]
          if (val1 != "" && val2 != "") {
            if ((conn$symbol == "=" && val1 != val2) || 
                ((conn$symbol == "X" || conn$symbol == "x") && val1 == val2)) {
              invalid[i, conn$col1] <- TRUE
              invalid[i, conn$col2] <- TRUE
            }
          }
        }
      }
      
      for (j in 1:n_cols) {
        conns <- vertical_connectors_map[[as.character(j)]]
        for (conn in conns) {
          val1 <- mat[conn$row1, j]
          val2 <- mat[conn$row2, j]
          if (val1 != "" && val2 != "") {
            if ((conn$symbol == "=" && val1 != val2) || 
                ((conn$symbol == "X" || conn$symbol == "x") && val1 == val2)) {
              invalid[conn$row1, j] <- TRUE
              invalid[conn$row2, j] <- TRUE
            }
          }
        }
      }
      
      invalid
    }
    
    # Observador para actualizar el cache de validación
    observe({
      mat <- board()
      validation_cache$highlights <- find_matches(mat)
      validation_cache$full_highlights <- find_full_matches(mat)
      validation_cache$connector_violations <- find_connector_violations(mat)
    })
    
    # Función optimizada para guardar estadísticas
    save_game_stats <- function(is_session_end = FALSE, is_periodic = FALSE) {
      if (is_periodic && click_count() == 0) return()  # No guardar si no hay actividad
      
      # Usar ruta absoluta para backup local
      data_dir <- "/srv/shiny-server/playOpt3/data"
      
      # Imprimir información de diagnóstico
      print(paste("Directorio actual:", getwd()))
      print(paste("Intentando crear/guardar en:", data_dir))
      print(paste("Zona horaria del sistema:", Sys.timezone()))
      print(paste("Timestamp actual (UTC):", format(Sys.time(), tz = "UTC")))
      print(paste("Tipo de guardado:", 
                 if(is_session_end) "Fin de sesión" 
                 else if(is_periodic) "Periódico" 
                 else "Juego completado"))
      
      # Crear directorio para datos si no existe
      if (!dir.exists(data_dir)) {
        print("Creando directorio data...")
        dir.create(data_dir, recursive = TRUE, mode = "0777")
        print(paste("Directorio creado:", dir.exists(data_dir)))
      }
      
      # Crear timestamp UTC explícito
      current_time_utc <- as.POSIXct(Sys.time(), tz = "UTC")
      
      # Leer estadísticas existentes o crear nuevas
      stats_file <- file.path(data_dir, "game_stats.rds")
      if (file.exists(stats_file)) {
        all_stats <- readRDS(stats_file)
      } else {
        all_stats <- list()
      }
      
      # Crear estadísticas de esta sesión
      stats_data <- list(
        session_id = session_id,
        session_start = session_start_time,
        session_end = current_time_utc,
        game_won = game_won(),
        click_count = click_count(),
        reset_count = reset_count(),
        click_timestamps = click_timestamps(),
        initial_matrix = initial_matrix,
        timestamp = current_time_utc,
        timezone = "UTC",
        server_timezone = Sys.timezone(),
        game = elegido,
        save_type = if(is_session_end) "session_end" 
                   else if(is_periodic) "periodic" 
                   else "game_completed"
      )
      
      # Agregar nuevas estadísticas
      all_stats <- c(all_stats, list(stats_data))
      
      # Guardar estadísticas actualizadas
      tryCatch({
        saveRDS(all_stats, stats_file)
        Sys.chmod(stats_file, mode = "0666")
        print("Estadísticas guardadas exitosamente")
        
        # Calcular métricas
        total_sessions <- length(all_stats)
        sessions_with_clicks <- sum(sapply(all_stats, function(x) x$click_count > 0))
        completed_games <- sum(sapply(all_stats, function(x) x$game_won))
        abandoned_games <- sum(sapply(all_stats, function(x) !x$game_won && x$click_count > 0))
        
        print(paste("Total de sesiones:", total_sessions))
        print(paste("Sesiones con clicks:", sessions_with_clicks))
        print(paste("Juegos completados:", completed_games))
        print(paste("Juegos abandonados:", abandoned_games))
        
        # Actualizar tiempo del último guardado
        last_save_time(as.numeric(Sys.time()))
        
      }, error = function(e) {
        print(paste("Error al guardar en data_dir:", e$message))
        # Si hay error, intentar guardar en /tmp
        tmp_file <- file.path("/tmp", "game_stats.rds")
        print(paste("Intentando guardar en /tmp:", tmp_file))
        
        if (file.exists(tmp_file)) {
          print("Archivo existe en /tmp, actualizando...")
          existing_stats <- readRDS(tmp_file)
          existing_stats <- c(existing_stats, list(stats_data))
          saveRDS(existing_stats, tmp_file)
          print("Archivo en /tmp actualizado exitosamente")
        } else {
          print("Creando nuevo archivo en /tmp...")
          saveRDS(list(stats_data), tmp_file)
          print("Nuevo archivo en /tmp creado exitosamente")
        }
        Sys.chmod(tmp_file, mode = "0666")
        print("Permisos de /tmp actualizados")
        
        # Actualizar tiempo del último guardado incluso si hubo error
        last_save_time(as.numeric(Sys.time()))
      })
    }
    
    # Observador para guardado periódico
    observe({
      invalidateLater(1000, session)
      current_time <- as.numeric(Sys.time())
      if (current_time - last_save_time() >= 10) {
        if (click_count() > 0 && !game_won()) {
          save_game_stats(is_periodic = TRUE)
        }
      }
    })
    
    # Observador para cuando el juego termina
    observeEvent(game_won(), {
      if (game_won()) {
        save_game_stats(is_session_end = FALSE)
        session$sendCustomMessage("stopTimer", list())
        showModal(modalDialog(
          title = HTML("<h2 style='text-align: center; background-color: #1e2c46; color: #de6f41;'>¡Lo lograste! 🎯</h2>"),
          fluidPage(
            fluidRow(
              column(12,
                     div(style = "padding: 20px; background-color: #f8f9fa; border-radius: 15px;",
                         p(style = "color: #1e2c46; font-size: 20px; font-weight: bold; text-align: center; margin-bottom: 20px;",
                           "📣 Esto no es un juego. Es el camino más corto hacia tu mejor versión."),
                         
                         p(style = "color: #1e2c46; font-size: 16px; line-height: 1.6;",
                           "Si estás aquí, es porque ya sabes que puedes mejorar.
                           No necesitas que te lo digan. Lo sientes. Todos los días.
                           Que podrías decidir mejor.
                           Resolver más rápido.
                           Dominar lo complejo sin agotarte."),
                         
                         p(style = "color: #1e2c46; font-size: 16px; font-style: italic; margin: 20px 0;",
                           "Pero nadie te enseñó cómo."),
                         
                         p(style = "color: #de6f41; font-size: 18px; font-weight: bold; margin: 20px 0;",
                           "Hasta ahora."),
                         
                        div(
                           style = "text-align: center; margin: 30px 0;",
                           tags$a(
                             href = "https://www.instagram.com/cchiquitovalencia", 
                             target = "_blank",
                             style = "background-color: #de6f41; color: #1e2c46; padding: 15px 30px; border-radius: 25px; text-decoration: none; font-weight: bold; font-size: 18px; display: inline-flex; align-items: center; gap: 10px;",
                             icon("instagram"),
                             "👉👉👉 @cchiquitovalencia"
                           )
                         ),
                         
                         
                         p(style = "color: #1e2c46; font-size: 16px; line-height: 1.6;",
                           "Esto es un simulador mental.
                           Un entrenamiento silencioso para desarrollar lo que más escasea hoy:"),
                         
                         div(style = "margin: 20px 0; padding: 15px; background-color: #1e2c46; border-radius: 10px;",
                             p(style = "color: #de6f41; font-size: 16px; margin: 5px 0;", "📌 Claridad en la ambigüedad"),
                             p(style = "color: #de6f41; font-size: 16px; margin: 5px 0;", "📌 Agilidad sin ansiedad"),
                             p(style = "color: #de6f41; font-size: 16px; margin: 5px 0;", "📌 Estructura con flexibilidad"),
                             p(style = "color: #de6f41; font-size: 16px; margin: 5px 0;", "📌 Colaboración sin control"),
                             p(style = "color: #de6f41; font-size: 16px; margin: 5px 0;", "📌 Y decisiones, con impacto real")
                         ),
                         
                         p(style = "color: #1e2c46; font-size: 16px; font-weight: bold; margin: 20px 0;",
                           "Esto no es para los que buscan excusas.
                           Es para los que buscan atajos reales:"),
                         
                         div(style = "margin: 20px 0;",
                             p(style = "color: #1e2c46; font-size: 16px; margin: 5px 0;", "→ Los que saben que no hay tiempo que perder"),
                             p(style = "color: #1e2c46; font-size: 16px; margin: 5px 0;", "→ Que usar una herramienta inteligente no es hacer trampa, es optimizar."),
                             p(style = "color: #1e2c46; font-size: 16px; margin: 5px 0;", "→ Que mejorar no es opcional, es inevitable.")
                         ),
                         
                         p(style = "color: #1e2c46; font-size: 16px; line-height: 1.6;",
                           "Aquí vas a encontrar una comunidad que piensa distinto, que mejora y que usa lo que tiene para llegar más lejos."),
                         
                         p(style = "color: #de6f41; font-size: 18px; font-weight: bold; margin: 20px 0;",
                           "¿Quieres entrenar tu mente para rendir mejor en lo que importa?"),
                         
                         p(style = "color: #1e2c46; font-size: 16px; font-weight: bold; margin: 20px 0;",
                           "Sígueme."),
                         
                         p(style = "color: #1e2c46; font-size: 16px; line-height: 1.6;",
                           "Y empieza a tomar decisiones como quien ya está en otro nivel."),
                         
                         div(style = "margin: 20px 0; padding: 15px; background-color: #de6f41; border-radius: 10px;",
                             p(style = "color: #1e2c46; font-size: 16px; font-weight: bold; margin: 5px 0;", "👣 El próximo paso no es difícil."),
                             p(style = "color: #1e2c46; font-size: 16px; font-weight: bold; margin: 5px 0;", "Es distinto."),
                             p(style = "color: #1e2c46; font-size: 16px; font-weight: bold; margin: 5px 0;", "Y empieza ahora.")
                         ),
                         
                         p(style = "color: #1e2c46; font-size: 16px; font-style: italic; text-align: center; margin-top: 20px;",
                           "Te espero adentro.")
                     )
              )
            )
          ),
          easyClose = TRUE,
          footer = NULL
        ))
      }
    })
    
    # Observador para cuando la sesión termina
    session$onSessionEnded(function() {
      if (!game_won()) {
        save_game_stats(is_session_end = TRUE)
      }
    })
    
    output$stats <- renderUI({
      # Ya no guardamos aquí, solo mostramos NULL
      NULL
    })
    
    # Observador para el botón de reinicio
    observeEvent(input$reset_board, {
      if (!game_won()) {
        board(initial_matrix)
        previous_board(initial_matrix)
        reset_count(reset_count() + 1)
      }
    })
    
    progress_percent <- reactive({
      total_fillable <- n_rows * n_cols - initial_fixed_cells
      filled <- user_filled_cells()
      if (total_fillable == 0) return(0)
      round((filled / total_fillable) * 100, 1)
    })
    
    output$reset_button <- renderUI({
      if (game_won()) {
        actionButton(ns("reset_board"), "🔄 Reiniciar Tablero", 
                    class = "btn btn-primary", 
                    style = "opacity: 0.5; cursor: not-allowed;",
                    disabled = TRUE)
      } else {
        actionButton(ns("reset_board"), "🔄 Reiniciar Tablero", 
                    class = "btn btn-primary")
      }
    })
    
    # UI reactiva optimizada
    output$board_ui <- renderUI({
      mat <- board()
      highlights <- validation_cache$highlights
      full_highlights <- validation_cache$full_highlights
      connector_violations <- validation_cache$connector_violations
      
      table_rows <- list()
      for (i in 1:n_rows) {
        cell_row <- list()
        for (j in 1:n_cols) {
          btn_id <- paste0("cell_", i, "_", j)
          value <- mat[i, j]
          value_icon <- switch(
            value,
            "Moon" = HTML('<svg width="32" height="32" viewBox="0 0 100 100"><polygon points="50,10 90,90 10,90" style="fill:#de6f41;" /></svg>'),
            "Sun"  = HTML('<svg width="32" height="32" viewBox="0 0 100 100"><circle cx="50" cy="50" r="42" style="fill:#1e2c46;" /></svg>'),
            value
          )
          bg_color <- if (game_won()) {
            "#C8E6C9"
          } else if (connector_violations[i, j]) {
            "#FFCDD2"
          } else if (full_highlights[i, j]) {
            "#FFE0B2"
          } else {
            cell_color(i, j, value)
          }
          border_color <- if (highlights[i, j]) "red" else "#889fbf"
          style <- paste0(
            "width: 40px; height: 40px; padding: 0; text-align: center;",
            "border: 3px solid ", border_color, "; background-color: ", bg_color, ";"
          )
          cell <- tags$td(actionButton(ns(btn_id), label = value_icon, style = style), style = "padding:2px;")
          cell_row <- append(cell_row, list(cell))
          if (j < n_cols) {
            conn <- Filter(function(x) x$row == i &&
                             ((x$col1 == j && x$col2 == j + 1) ||
                                (x$col2 == j && x$col1 == j + 1)),
                           horizontal_connectors)
            if (length(conn) > 0) {
              symb <- conn[[1]]$symbol
              connector_cell <- tags$td(symb, style = "width: 15px; text-align: center; font-weight: bold; font-size: 15px; color: #de6f41;")
            } else {
              connector_cell <- tags$td(" ", style = "width: 15px;")
            }
            cell_row <- append(cell_row, list(connector_cell))
          }
        }
        table_rows <- append(table_rows, list(tags$tr(cell_row)))
        if (i < n_rows) {
          conn_row <- list()
          for (j in 1:n_cols) {
            conn <- Filter(function(x) x$col == j &&
                             ((x$row1 == i && x$row2 == i + 1) ||
                                (x$row2 == i && x$row1 == i + 1)),
                           vertical_connectors)
            if (length(conn) > 0) {
              symb <- conn[[1]]$symbol
              conn_cell <- tags$td(symb, style = "height: 15px; text-align: center; font-weight: bold; font-size: 15px; color: #de6f41;")
            } else {
              conn_cell <- tags$td(" ", style = "height: 15px;")
            }
            conn_row <- append(conn_row, list(conn_cell))
            if (j < n_cols) {
              conn_row <- append(conn_row, list(tags$td(" ")))
            }
          }
          table_rows <- append(table_rows, list(tags$tr(conn_row)))
        }
      }
      
      tags$table(
        style = "border-collapse: collapse;",
        table_rows
      )
    })
    
    # Observadores para clicks en celdas
    observe({
      lapply(1:n_rows, function(i) {
        lapply(1:n_cols, function(j) {
          btn_id <- paste0("cell_", i, "_", j)
          observeEvent(input[[btn_id]], {
            if (game_won()) return()
            key <- paste0(i, "_", j)
            if (key %in% names(special_cells)) return()
            
            mat <- board()
            current_val <- mat[i, j]
            next_val <- cell_states[(match(current_val, cell_states, nomatch = 1) %% length(cell_states)) + 1]
            mat[i, j] <- next_val
            board(mat)
            
            if (identical(mat, optimo_1) || identical(mat, optimo_2)) {
              game_won(TRUE)
            }
          })
        })
      })
    })
  })
}