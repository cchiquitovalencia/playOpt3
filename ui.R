# ui.R
# Interfaz de usuario principal de la app Shiny

source("modules/mod_board.R") # Carga el módulo del tablero

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css"),
    # Script para el temporizador
    tags$script(HTML("
      var startTime = null;
      var timer = null;
      
      function startTimer() {
        startTime = Date.now();
        if (timer) clearInterval(timer);
        timer = setInterval(function(){
          var now = Date.now();
          var elapsed = Math.floor((now - startTime)/1000);
          var minutes = Math.floor(elapsed / 60);
          var seconds = elapsed % 60;
          document.getElementById('board1-timer').innerText =
            (minutes < 10 ? '0' : '') + minutes + ':' +
            (seconds < 10 ? '0' : '') + seconds;
        }, 1000);
      }
      
      Shiny.addCustomMessageHandler('stopTimer', function(message) {
        if (timer) clearInterval(timer);
      });
      
      Shiny.addCustomMessageHandler('startTimer', function(message) {
        startTimer();
      });
    "))
  ),
  
  tags$head(
    # Estilos personalizados
    tags$style(HTML("
      .game-title {
        font-family: 'Press Start 2P', monospace;
        font-size: 28px;
        text-align: center;
        padding: 20px 10px;
        color: #de6f41;
        background-color: #1e2c46;
        border-radius: 12px;
        margin-bottom: 20px;
        box-shadow: 0 4px 10px rgba(0,0,0,0.4);
        letter-spacing: 1px;
      }
      @import url('https://fonts.googleapis.com/css2?family=Press+Start+2P&display=swap');
      
      .modal-content {
        background-color: #f8f9fa;
        border-radius: 15px;
        border: 3px solid #1e2c46;
      }
      
      .modal-header {
        background-color: #1e2c46;
        color: #de6f41;
        border-radius: 12px 12px 0 0;
        padding: 15px;
      }
      
      .modal-body {
        padding: 20px;
        color: #1e2c46;
      }
      
      .play-button {
        background-color: #de6f41;
        color: #1e2c46;
        border: none;
        padding: 10px 30px;
        font-size: 18px;
        font-weight: bold;
        border-radius: 25px;
        margin: 20px auto;
        display: block;
        transition: all 0.3s ease;
      }
      
      .play-button:hover {
        background-color: #1e2c46;
        color: #de6f41;
        transform: scale(1.05);
      }
    "))
  ),
  
  div(class = "game-title", "🕹️   Opt3"),
  setBackgroundColor(
    color = "ghostwhite",
    gradient = c("linear", "radial"),
    direction = c("bottom", "top", "right", "left"),
    shinydashboard = FALSE
  ),
  tags$head(tags$style(
    HTML('
      #sidebar {
        background-color: #9db4d3;
        color: #1e2c46;
      }
      body, label, input, button, select {
        font-family: "Arial";
      }')
  )),
  
  mainPanel(
    width = 6,
    # Llama al módulo del tablero
    mod_board_ui("board1")
  ),
  sidebarPanel(
    width = 4,
    id = "sidebar",
    h3("¿Cómo se juega?"),
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
    br(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css"),
      tags$style(HTML("
        #board1-reset_board {
          color: #1e2c46;
          background-color: #de6f41;
          border: 2px solid #1e2c46;
          font-size: 16px;
          font-weight: bold;
          padding: 10px 20px;
          border-radius: 20px;
          transition: all 0.3s ease;
          display: block;
          margin: 20px auto 0 auto;
        }
        #board1-reset_board:hover {
          color: #de6f41;
          background-color: #1e2c46;
          border-color: #de6f41;
          cursor: pointer;
        }
      "))
    )
  )
)