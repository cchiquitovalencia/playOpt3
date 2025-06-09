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
    downloadButton(ns("download_times"), "Descargar tiempos entre clics"),
    textOutput(ns("test"))
  )
}

# ---- Server del módulo ----
mod_board_server <- function(id, optimo_1, optimo_2, n_rows, n_cols, elementos, special_cells,
                             horizontal_connectors, vertical_connectors, cell_color, tiempo_inicial) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cell_states <- c("", "Moon", "Sun")
    initial_matrix <- matrix("", nrow = n_rows, ncol = n_cols)
    
    # Rellenar la matriz con los valores fijos
    traducir_icono <- function(icono) {
      if (icono %in% c("Luna", "Moon")) return("Moon")
      if (icono %in% c("Sol", "Sun")) return("Sun")
      return("")
    }
    for (tbl in elementos) {
      for (i in seq_len(nrow(tbl))) {
        fila <- tbl$eje_x[i]
        columna <- tbl$eje_y[i]
        icono <- traducir_icono(tbl$img[i])
        initial_matrix[fila, columna] <- icono
      }
    }
    
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
    
    # ---- Funciones de validación y resaltado ----
    find_matches <- function(mat) {
      to_highlight <- matrix(FALSE, nrow = n_rows, ncol = n_cols)
      for (i in 1:n_rows) {
        for (j in 1:(n_cols - 2)) {
          if (mat[i, j] != "" && mat[i, j] == mat[i, j+1] && mat[i, j] == mat[i, j+2]) {
            to_highlight[i, j:(j+2)] <- TRUE
          }
        }
      }
      for (j in 1:n_cols) {
        for (i in 1:(n_rows - 2)) {
          if (mat[i, j] != "" && mat[i, j] == mat[i+1, j] && mat[i, j] == mat[i+2, j]) {
            to_highlight[i:(i+2), j] <- TRUE
          }
        }
      }
      to_highlight
    }
    find_full_matches <- function(mat) {
      full_highlight <- matrix(FALSE, nrow = n_rows, ncol = n_cols)
      for (i in 1:n_rows) {
        counts <- table(mat[i, mat[i, ] != ""])
        if (any(counts >= 4)) {
          full_highlight[i, ] <- TRUE
        }
      }
      for (j in 1:n_cols) {
        counts <- table(mat[mat[, j] != "", j])
        if (any(counts >= 4)) {
          full_highlight[, j] <- TRUE
        }
      }
      return(full_highlight)
    }
    find_connector_violations <- function(mat) {
      invalid <- matrix(FALSE, nrow = n_rows, ncol = n_cols)
      check_pair <- function(val1, val2, symbol) {
        if (val1 == "" || val2 == "") return(FALSE)
        if (symbol == "=") return(val1 != val2)
        if (symbol == "X" || symbol == "x") return(val1 == val2)
        return(FALSE)
      }
      for (conn in horizontal_connectors) {
        i <- conn$row
        j1 <- conn$col1
        j2 <- conn$col2
        if (check_pair(mat[i, j1], mat[i, j2], conn$symbol)) {
          invalid[i, j1] <- TRUE
          invalid[i, j2] <- TRUE
        }
      }
      for (conn in vertical_connectors) {
        j <- conn$col
        i1 <- conn$row1
        i2 <- conn$row2
        if (check_pair(mat[i1, j], mat[i2, j], conn$symbol)) {
          invalid[i1, j] <- TRUE
          invalid[i2, j] <- TRUE
        }
      }
      return(invalid)
    }
    
    # ---- Observers y lógica de juego ----
    observeEvent(board(), {
      req(previous_board())
      curr_board <- board()
      prev_board <- isolate(previous_board())
      if (identical(curr_board, prev_board)) return()
      change_idx <- which(curr_board != prev_board, arr.ind = TRUE)
      if (nrow(change_idx) == 1) {
        changed_cell <- paste0("(", change_idx[1,1], ",", change_idx[1,2], ")")
      } else {
        changed_cell <- NA
      }
      now <- as.numeric(Sys.time())
      df <- click_timestamps()
      time_diff <- if (nrow(df) == 0) NA else now - tail(df$timestamp, 1)
      last_cell <- last_changed_cell()
      cell_status <- if (!is.null(last_cell) && !is.na(changed_cell)) {
        if (last_cell == changed_cell) "misma" else "diferente"
      } else {
        NA
      }
      last_changed_cell(changed_cell)
      df <- rbind(df, data.frame(
        timestamp = now,
        diff = time_diff,
        celda = changed_cell,
        cambio = cell_status,
        game = elegido,
        inicio = tiempo_inicial
      ))
      click_timestamps(df)
      click_count(click_count() + 1)
      previous_board(curr_board)
      filled <- sum(curr_board != "" & initial_matrix == "")
      user_filled_cells(filled)
    })
    
    output$download_times <- downloadHandler(
      filename = function() { paste0("click_times_", Sys.time(), ".csv") },
      content = function(file) {
        write.csv(click_timestamps(), file, row.names = FALSE)
      }
    )
    
    progress_percent <- reactive({
      total_fillable <- n_rows * n_cols - initial_fixed_cells
      filled <- user_filled_cells()
      if (total_fillable == 0) return(0)
      round((filled / total_fillable) * 100, 1)
    })
    
    output$stats <- renderUI({
      tagList(
        tags$p(paste("Juego terminado:", if (game_won()) "Sí" else "No")),
        tags$p(paste("Clicks totales:", click_count())),
        tags$p(paste("Reinicios:", reset_count())),
        tags$p(paste("Progreso:", progress_percent(), "%"))
      )
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
    
    observeEvent(input$reset_board, {
      if (!game_won()) {
        board(initial_matrix)
        previous_board(initial_matrix)
        reset_count(reset_count() + 1)
      }
    })
    
    output$board_ui <- renderUI({
      mat <- board()
      highlights <- find_matches(mat)
      full_highlights <- find_full_matches(mat)
      connector_violations <- find_connector_violations(mat)
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
    
    # ---- Observers para clicks en celdas ----
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
              session$sendCustomMessage("stopTimer", list())
              showModal(modalDialog(
                title = HTML("<h2 style='text-align: center; background-color: #1e2c46; color: #de6f41;'>¡Te felicito! Lo lograste 🎉</h2>"),
                fluidPage(
                  fluidRow(
                    column(12,
                           p(style = "color: #1e2c46;", "¿Te atreves a mostrar tu resultado y descubrir dónde realmente estás en esta partida?"),
                           p(style = "color: #1e2c46;", "Ingresa tus datos y compite con los demás. Solo los mejores recibirán un regalo sorpresa…"),
                           p(style = "color: #de6f41;", em("algo que nadie espera, pero que todos querrán.")),
                           p(style = "color: #1e2c46;", "¿Prefieres mantener el bajo perfil? Sin problema, pero recuerda: lo misterioso siempre tiene su recompensa.")
                    )
                  ),
                  fluidRow(
                    column(12,
                           HTML("<input id='winner_email' type='email' placeholder='ejemplo@correo.com' style='width:100%; padding:10px; font-size:20px;'><br><span style='color: #1e2c46;'>Tu correo electrónico</span>")
                    )
                  ),br(),
                ),
                fluidRow(
                  column(12,
                         div(style='text-align: center; margin-top: 20px;',
                             actionButton(ns("submit_email"), "Enviar", style = "background-color: #de6f41; color: #1e2c46; padding: 10px 20px; border: none; border-radius:15px; cursor: pointer;"),
                             actionButton(ns("cancel"), "Cancelar", style = "background-color: #889fbf; color: #1e2c46; padding: 10px 20px; border: none; border-radius:15px; cursor: pointer;")
                         )
                  )
                ),
                easyClose = FALSE,
                footer = HTML("Sígueme en <a href='https://www.instagram.com/cchiquitovalencia' target='_blank' style='color: #de6f41; text-decoration: none;'>@cchiquitovalencia</a>")
              ))
            }
          }, ignoreInit = TRUE)
        })
      })
    })
    
    observeEvent(input$submit_email, {
      email <- input$winner_email
      if (!is.null(email) && grepl(".+@.+\\..+", email)) {
        cat("Correo ingresado:", email, "\n")
        removeModal()
        showModal(modalDialog(
          title = "¡Gracias!",
          p("Tu correo fue recibido con decisión. 🎯"),
          easyClose = TRUE,
          footer = NULL
        ))
      } else {
        showModal(modalDialog(
          title = "Correo no válido",
          p("Por favor ingresa un correo válido."),
          textInput(ns("winner_email"), "Tu correo electrónico:", value = email),
          actionButton(ns("submit_email"), "Enviar"),
          easyClose = FALSE,
          footer = NULL
        ))
      }
    })
    observeEvent(input$cancel, {
      removeModal()
    })
  })
}