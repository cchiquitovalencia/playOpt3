# server.R
# Lógica principal del servidor de la app Shiny

source("modules/mod_board.R") # Carga el módulo del tablero

server <- function(input, output, session) {
  # Selecciona aleatoriamente un escenario HTML para esta sesión
  elegido <- sample(list.files("./escenarios", pattern = "\\.html$", full.names = TRUE), 1)
  
  # Carga el escenario
  escenario_data <- cargar_escenario(elegido)
  
  # Llama al módulo del tablero, pasando los datos y funciones globales
  mod_board_server(
    id = "board1",
    optimo_1 = escenario_data$optimo_1,
    optimo_2 = escenario_data$optimo_2,
    n_rows = escenario_data$n_rows,
    n_cols = escenario_data$n_cols,
    elementos = escenario_data$elementos,
    special_cells = escenario_data$special_cells,
    horizontal_connectors = escenario_data$horizontal_connectors,
    vertical_connectors = escenario_data$vertical_connectors,
    cell_color = function(i, j, value) cell_color(i, j, value, escenario_data$special_cells),
    tiempo_inicial = Sys.time()
  )
  
  # Mostrar el modal de instrucciones al inicio
  showModal(modalDialog(
    title = HTML("<h2 style='text-align: center; background-color: #1e2c46; color: #de6f41;'>¿Cómo se juega?</h2>"),
    HTML("
      <p>Tu misión es simple... o eso parece:</p>
      <ul>
        <li>Llena la cuadrícula usando solo <span style='font-size:20px;'><svg width='24' height='24'><polygon points='12,2 22,22 2,22' style='fill:#de6f41;'/></svg>️</span> y <span style='font-size:20px;'><svg width='24' height='24'><circle cx='12' cy='12' r='10' style='fill:#1e2c46;'/></svg>️</span>.</li>
        <li><b>¡Pero ojo!</b> No puede haber más de <b>2 triángulos o 2 circulos</b> consecutivos, ni en fila ni en columna. Nada de tríos.  <br><svg width='24' height='24'><polygon points='12,2 22,22 2,22' style='fill:#de6f41;'/></svg><svg width='24' height='24'><polygon points='12,2 22,22 2,22' style='fill:#de6f41;'/></svg><svg width='24' height='24'><polygon points='12,2 22,22 2,22' style='fill:#de6f41;'/></svg> ❌     <br> <svg width='24' height='24'><circle cx='12' cy='12' r='10' style='fill:#1e2c46;'/></svg><svg width='24' height='24'><circle cx='12' cy='12' r='10' style='fill:#1e2c46;'/></svg> ✅ </li>
        <li>Cada fila y cada columna debe tener <b>la misma cantidad</b> de <span style='font-size:20px;'><svg width='24' height='24'><polygon points='12,2 22,22 2,22' style='fill:#de6f41;'/></svg>️</span> y <span style='font-size:20px;'><svg width='24' height='24'><circle cx='12' cy='12' r='10' style='fill:#1e2c46;'/></svg>️</span>. Equilibrio, ¿sabes?</li>
      </ul>
      <hr>
      <p>¿Ves símbolos como <b>=</b> o <b>x</b> entre algunas celdas?</p>
      <ul>
        <li><b>=</b> significa que esas dos celdas deben ser <b>iguales</b>.  <br>(<svg width='24' height='24'><polygon points='12,2 22,22 2,22' style='fill:#de6f41;'/></svg>️ = <svg width='24' height='24'><polygon points='12,2 22,22 2,22' style='fill:#de6f41;'/></svg>️ o <svg width='24' height='24'><circle cx='12' cy='12' r='10' style='fill:#1e2c46;'/></svg>=<svg width='24' height='24'><circle cx='12' cy='12' r='10' style='fill:#1e2c46;'/></svg>)</li>
        <li><b>x</b> significa que deben ser <b>diferentes</b>. (<svg width='24' height='24'><polygon points='12,2 22,22 2,22' style='fill:#de6f41;'/></svg>️ ≠ <svg width='24' height='24'><circle cx='12' cy='12' r='10' style='fill:#1e2c46;'/></svg>)</li>
      </ul>
      <hr>
      <p><b>Haz clic</b> en una celda para alternar entre <svg width='24' height='24'><polygon points='12,2 22,22 2,22' style='fill:#de6f41;'/></svg>️,  <svg width='24' height='24'><circle cx='12' cy='12' r='10' style='fill:#1e2c46;'/></svg> y vacía. ¡Experimenta!</p>
      <p>Cuando todo esté en su lugar... lo sabrás 😉</p>
      <p><i>¿Listo para descifrar el equilibrio del universo?</i></p>
    "),
    footer = actionButton("start_game", "Jugar", class = "play-button"),
    easyClose = FALSE,
    size = "l"
  ))
  
  observeEvent(input$start_game, {
    removeModal()
    session$sendCustomMessage("startTimer", list())
  })
}

