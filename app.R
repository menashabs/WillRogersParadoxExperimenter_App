# ----------------------------------------------------------

# ---------- The Will Rogers Paradox Experimenter ----------

# ----------------------------------------------------------

# --- LIBRARIES ---
library(shiny)
library(ggplot2)
library(dplyr)

# --- UI ---
ui <- fluidPage(
  # --- Custom Color Palette (Sepia + Vintage) ---
  tags$style(HTML("
  body {
    background-color: #f5f5f5;
    color: #1f1f1f;
    font-family: 'Georgia', serif;
  }

  h1, h2, h3, h4 {
    color: #111111;
    font-weight: bold;
  }

  .title-box {
    background-color: #1f1f1f;
    padding: 20px;
    border-radius: 10px;
    text-align: center;
    margin-bottom: 25px;
  }

  .title-box h1 {
    color: #f5f5f5;
  }

  .well {
    background-color: #e0e0e0;
    border: 1px solid #bdbdbd;
    border-radius: 10px;
  }

  .btn {
    background-color: #2b2b2b !important;
    color: #ffffff !important;
    border: none;
    font-weight: bold;
  }

  .btn:hover {
    background-color: #4a4a4a !important;
  }

  label {
    color: #1f1f1f;
    font-weight: bold;
  }

  .selectize-input {
    background-color: #e6e6e6;
    color: #1f1f1f;
    border: 1px solid #bdbdbd;
  }

  .message-box {
    background-color: #e0e0e0;
    border: 2px solid #2b2b2b;
    border-radius: 10px;
    padding: 15px;
    margin-top: 15px;
    text-align: center;
    font-size: 18px;
    font-weight: bold;
    color: #1f1f1f;
  }

  a {
    color: #1f1f1f;
    text-decoration: underline;
  }
")),
  
  # --- Title Box ---
  tags$div(class = "title-box",
           h1("WillRogersParadoxExperimenter: Will Rogers Paradox Experimenting App")),
  
  # --- Image + Text Side by Side ---
  fluidRow(
    column(
      6,
      tags$img(
        src = "Will_Rogers_Paradox_Image.jpg",
        width = "100%",
        style = "max-height:350px; border-radius:10px; margin-bottom:15px;"
      )
    ),
    column(
      6,
      tags$div(
        style = "font-size:17px; text-align:justify; margin-bottom:35px; margin-top:20px;",
        "The Will Rogers Paradox shows how moving an element from one group to another can raise the average of both groups.",
        tags$br(), tags$br(),
        "The idea comes from a joke by humorist Will Rogers, who said during the Great Depression:",
        tags$br(),
        tags$i('“When the Okies left Oklahoma and moved to California, they raised the average intelligence in both states.”'),
        tags$br(), tags$br(),
        "This joke pointed out that, mathematically, both averages could rise under certain conditions. It’s a witty reminder that averages can be deceptive. Statistics sometimes tell a perfectly silly truth when categories shift.",
        tags$br(), tags$br(),
        tags$a(href= "https://medium.com/@menashabrayani/the-will-rogers-paradox-when-statistics-tell-a-perfectly-silly-truth-89cc8c2a2d9a", "Read more about Will Rogers Paradox here.", target="_blank", style="colour:#2e1c14; text-decoration:underline;")
      )
    )
  ),
  
  # --- Layout: Wide Sidebar and Main Panel ---
  fluidRow(
    column(
      width = 7,
      wellPanel(
        h3("Groups Setup", style = "color:#2e1c14;"),
        fluidRow(
          column(
            6,
            h4("Group A", style = "color:#2e1c14;"),
            selectInput("addA", "Add value", choices = 0:1001, selected = 50),
            actionButton("addA_btn", "Add to Group A"),
            verbatimTextOutput("A_values")
          ),
          column(
            6,
            h4("Group B", style = "color:#2e1c14;"),
            selectInput("addB", "Add value", choices = 0:1001, selected = 80),
            actionButton("addB_btn", "Add to Group B"),
            verbatimTextOutput("B_values")
          )
        ),
        hr(),
        h3("Move a Value", style = "color:#2e1c14;"),
        fluidRow(
          column(
            6,
            selectInput("move_from", "Move value from:", choices = c("A → B", "B → A"))
          ),
          column(
            6,
            numericInput("move_val", "Value to move", value = 50, min = 0, max = 1000)
          )
        ),
        actionButton("move_btn", "Move Value")
      )
    ),
    
    column(
      width = 5,
      fluidRow(
        column(
          6,
          wellPanel(
            h3("Mean of Group A", style = "color:#2e1c14;"),
            h2(textOutput("meanA"), style = "color:#3b2f2f; font-weight:bold;")
          )
        ),
        column(
          6,
          wellPanel(
            h3("Mean of Group B", style = "color:#2e1c14;"),
            h2(textOutput("meanB"), style = "color:#3b2f2f; font-weight:bold;")
          )
        )
      ),
      uiOutput("messageBox"),
      br(),
      tags$div(
        style = "font-size:16px; text-align:center; margin-top:30px;",
        "💡 Tip: Try values like A = 25, 35, 45 and B = 10, 20, 30.",
        tags$br(),
        "Then move 25 from A to B to see the paradox."
      )
    )
  )
)

# --- SERVER ---

server <- function(input, output, session) {
  values <- reactiveValues(
    A = numeric(),
    B = numeric(),
    prev_meanA = NA,
    prev_meanB = NA,
    message = "Move a value between groups to explore the paradox."
  )
  
  # Add value to Group A
  observeEvent(input$addA_btn, {
    values$A <- unique(c(values$A, as.numeric(input$addA)))
  })
  
  # Add value to Group B
  observeEvent(input$addB_btn, {
    values$B <- unique(c(values$B, as.numeric(input$addB)))
  })
  
  output$A_values <- renderText({
    paste("Group A:", ifelse(length(values$A) == 0, "No values yet", paste(values$A, collapse = ", ")))
  })
  
  output$B_values <- renderText({
    paste("Group B:", ifelse(length(values$B) == 0, "No values yet", paste(values$B, collapse = ", ")))
  })
  
  # Move value between groups
  observeEvent(input$move_btn, {
    values$prev_meanA <- mean(values$A, na.rm = TRUE)
    values$prev_meanB <- mean(values$B, na.rm = TRUE)
    
    if (input$move_from == "A → B" && input$move_val %in% values$A) {
      values$A <- values$A[values$A != input$move_val]
      values$B <- c(values$B, input$move_val)
    } else if (input$move_from == "B → A" && input$move_val %in% values$B) {
      values$B <- values$B[values$B != input$move_val]
      values$A <- c(values$A, input$move_val)
    }
    
    # Update paradox message
    if (!is.na(values$prev_meanA) && !is.na(values$prev_meanB)) {
      if (mean(values$A) > values$prev_meanA && mean(values$B) > values$prev_meanB) {
        values$message <- "Both averages increased — Will Rogers paradox in action!"
      } else {
        values$message <- "Not satisfying the conditions of paradox — keep experimenting!"
      }
    } else {
      values$message <- "Move a value between groups to explore the paradox."
    }
  })
  
  # Mean Outputs
  output$meanA <- renderText({
    if (length(values$A) > 0) round(mean(values$A), 2) else "-"
  })
  
  output$meanB <- renderText({
    if (length(values$B) > 0) round(mean(values$B), 2) else "-"
  })
  
  # Styled Message Box under means
  output$messageBox <- renderUI({
    tags$div(class = "message-box", values$message)
  })
}

shinyApp(ui, server)

