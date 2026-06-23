library(shiny)

# -----------------------------
# FUNCTION: CUMULATIVE TRIANGLE
# -----------------------------
compute_triangle = function(claims, tail_factor) {
  
  loss_years = sort(unique(claims$loss_year))
  n = length(loss_years)
  
  tri = matrix(
    NA,
    nrow = n,
    ncol = 3,
    dimnames = list(loss_years, c("1", "2", "3"))
  )
  
  # -----------------------------
  # BUILD CUMULATIVE TRIANGLE
  # -----------------------------
  for (i in seq_along(loss_years)) {
    
    ly = loss_years[i]
    
    for (d in 1:3) {
      
      if (any(claims$loss_year == ly &
              claims$dev_year == d)) {
        
        tri[i, d] = sum(
          claims$amount[
            claims$loss_year == ly &
              claims$dev_year <= d
          ]
        )
      }
    }
  }
  
  # -----------------------------
  # DEVELOPMENT FACTORS (UNCHANGED LOGIC)
  # -----------------------------
  f12 = sum(tri[, 2], na.rm = TRUE) /
    sum(tri[, 1], na.rm = TRUE)
  
  f23 = tri[1, 3] / tri[1, 2]
  
  # -----------------------------
  # APPLY CHAIN LADDER TO ALL YEARS (INCLUDING 2020+)
  # -----------------------------
  for (i in 2:n) {
    
    # Dev 2
    if (is.na(tri[i, 2])) {
      tri[i, 2] = tri[i, 1] * f12
    }
    
    # Dev 3
    if (is.na(tri[i, 3])) {
      tri[i, 3] = tri[i, 2] * f23
    }
  }
  
  # -----------------------------
  # ULTIMATE WITH TAIL
  # -----------------------------
  ultimate = tri[, 3] * tail_factor
  
  result = cbind(tri, "Tail (Ult.)" = ultimate)
  
  list(triangle = result)
}

# -----------------------------
# UI
# -----------------------------
ui = fluidPage(
  
  h2("Chain-Ladder Loss Development"),
  
  h3("Upload Claims Data"),
  
  fileInput(
    "claims_file",
    "Choose CSV File",
    accept = ".csv"
  ),
  
  helpText(
    "CSV must contain columns: loss_year, dev_year and amount."
  ),
  
  br(),
  
  h3("Input Parameter"),
  
  numericInput(
    "tail_factor",
    "Tail Factor",
    value = 1.10,
    min = 1,
    step = 0.01
  ),
  
  br(),
  
  h3("Cumulative Paid Claims Triangle ($)"),
  
  tableOutput("triangle_table"),
  
  br(),
  
  h3("Claims Development Pattern"),
  
  plotOutput("claims_plot", height = "500px")
)

# -----------------------------
# SERVER
# -----------------------------
server = function(input, output, session) {
  
  # Read CSV
  claims_data = reactive({
    
    req(input$claims_file)
    
    read.csv(
      input$claims_file$datapath,
      stringsAsFactors = FALSE
    )
  })
  
  # Compute triangle
  result = reactive({
    
    compute_triangle(
      claims = claims_data(),
      tail_factor = input$tail_factor
    )
  })
  
  # Table output
  output$triangle_table = renderTable({
    
    tri = result()$triangle
    
    cbind(
      "Loss Year" = rownames(tri),
      as.data.frame(round(tri, 2))
    )
  }, digits = 2)
  
  # Plot output
  output$claims_plot = renderPlot({
    
    tri = result()$triangle
    
    plot_matrix = as.matrix(tri[, 1:4])
    
    colours = rainbow(nrow(plot_matrix))
    
    matplot(
      t(plot_matrix),
      type = "b",
      pch = 19,
      lty = 1,
      lwd = 3,
      col = colours,
      xaxt = "n",
      xlab = "Development Stage",
      ylab = "Cumulative Paid Claims ($)",
      main = "Claims Development by Loss Year"
    )
    
    axis(
      side = 1,
      at = 1:4,
      labels = c("Dev 1", "Dev 2", "Dev 3", "Ultimate")
    )
    
    # Value labels
    for (i in 1:nrow(plot_matrix)) {
      
      text(
        x = 1:4,
        y = plot_matrix[i, ],
        labels = format(round(plot_matrix[i, ]), big.mark = ","),
        pos = 3,
        cex = 0.7,
        col = colours[i]
      )
    }
    
    points(
      x = rep(4, nrow(plot_matrix)),
      y = plot_matrix[, 4],
      pch = 17,
      cex = 1.5,
      col = colours
    )
    
    legend(
      "topleft",
      legend = rownames(plot_matrix),
      col = colours,
      lty = 1,
      lwd = 3,
      pch = 19,
      title = "Loss Year"
    )
  })
}

# -----------------------------
# RUN APP
# -----------------------------
shinyApp(ui, server)
