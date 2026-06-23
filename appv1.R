library(shiny)
library(rhandsontable)
# ------ version 1: edit in shiny app ---- 

# starting dataset (just some sample claims, can be changed)
default_claims = data.frame(
  loss_year = c(2017, 2017, 2017, 2018, 2018, 2019),
  dev_year  = c(1, 2, 3, 1, 2, 1),
  amount    = c(524792, 218265, 2225, 798502, 197157, 917636)
)
# this function builds the cumulative triangle and applies chain ladder projection
compute_triangle = function(claims, tail_factor)
  {
  
  loss_years = sort(unique(claims$loss_year))
  
  # empty structure 
  tri = matrix(
    NA,
    nrow = length(loss_years),
    ncol = 3,
    dimnames = list(loss_years, c("1", "2", "3"))
  )
  # go through each loss year and build cumulative values
  for (i in seq_along(loss_years)) {
    
    ly = loss_years[i]
    
    for (d in 1:3) {
      # check if we actually have that dev year in the data
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
  # factor from dev 1 to dev 2 
  f12 = sum(tri[!is.na(tri[, 2]), 2]) /
    sum(tri[!is.na(tri[, 2]), 1])
  
  # factor from dev 2 to dev 3
  f23 = tri[1, 3] / tri[1, 2]
  
  # fill missing triangle values manually (excel formulas)
  
  if (is.na(tri[2, 3])) {
    tri[2, 3] = tri[2, 2] * f23
  }
  
  if (is.na(tri[3, 2])) {
    tri[3, 2] = tri[3, 1] * f12
  }
  
  if (is.na(tri[3, 3])) {
    tri[3, 3] = tri[3, 2] * f23
  }
  
  # apply tail factor to get ultimate claims
  ultimate = tri[, 3] * tail_factor
  
  result = cbind(
    tri,
    "Tail (Ult.)" = ultimate
  )
  
  list(triangle = result)
}

#  input UI
ui = fluidPage(
  
  h2("Chain-Ladder Loss Development"),
  
  h3("Input Parameter"),
  
  numericInput(
    "tail_factor",
    "Tail Factor",
    value = 1.10,
    min = 1,
    step = 0.01
  ),
  
  h3("Claims Data"),
  
  helpText(
    "You can edit the table directly below by inserting values."
  ),
  
  rHandsontableOutput("claims_table"),
  
  br(),
  
  h3("Cumulative Paid Claims Triangle ($)"),
  
  tableOutput("triangle_table"),
  
  br(),
  
  h3("Claims Development Pattern"),
  
  plotOutput("claims_plot", height = "500px")
)

# server logic
server = function(input, output, session) {
  
  # store table so user can edit it
  rv = reactiveValues(
    claims = default_claims
  )
  
  # editable table shown to user
  output$claims_table = renderRHandsontable({
    
    rhandsontable(rv$claims, rowHeaders = NULL) |>
      hot_col("loss_year", type = "numeric") |>
      hot_col("dev_year", type = "numeric") |>
      hot_col("amount", type = "numeric")
    
  })
  
  # update data whenever user edits table
  observeEvent(input$claims_table, {
    rv$claims = hot_to_r(input$claims_table)
  })
  
  # recompute triangle whenever some data changes
  result = reactive({
    
    req(input$tail_factor)
    
    claims_clean = rv$claims[complete.cases(rv$claims), ]
    
    compute_triangle(
      claims = claims_clean,
      tail_factor = input$tail_factor
    )
  })
  
  # show triangle output
  output$triangle_table = renderTable({
    
    tri = result()$triangle
    
    cbind(
      "Loss Year" = rownames(tri),
      as.data.frame(round(tri, 2))
    )
    
  }, digits = 2)
  
  # plot 
  output$claims_plot = renderPlot({
    
    tri = result()$triangle
    
    plot_matrix = as.matrix(tri[, 1:4])
    
    colours = c("blue", "red", "darkgreen")
    
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
    
    # label each point 
    for (i in 1:nrow(plot_matrix)) {
      
      text(
        x = 1:4,
        y = plot_matrix[i, ],
        labels = format(round(plot_matrix[i, ]), big.mark = ","),
        pos = 3,
        cex = 0.8,
        col = colours[i]
      )
    }
    # highlight ultimate values
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
# run app
shinyApp(ui, server)
