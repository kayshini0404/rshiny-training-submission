library(shiny)
# ------ version 2: upload csv file---- 


#   cumulative claims triangle ,  applies tail factor at the end
compute_triangle = function(claims, tail_factor) {
  
  # get unique loss year
  loss_years = sort(unique(claims$loss_year))
  
  #empty triangle 
  tri = matrix(
    NA,
    nrow = length(loss_years),
    ncol = 3,
    dimnames = list(loss_years, c("1", "2", "3"))
  )
  
  # loop through each loss year and populate cumulative values
  for (i in seq_along(loss_years)) {
    
    ly = loss_years[i]
    
    for (d in 1:3) {
      
      # check if data exist
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
  
  # development factor from dev 1 to dev 2
  f12 = sum(tri[!is.na(tri[, 2]), 2]) /
    sum(tri[!is.na(tri[, 2]), 1])
  
  # development factor from dev 2 to dev 3
  f23 = tri[1, 3] / tri[1, 2]
  
  # fill missing values , projection
  
  if (is.na(tri[2, 3])) {
    tri[2, 3] = tri[2, 2] * f23
  }
  
  if (is.na(tri[3, 2])) {
    tri[3, 2] = tri[3, 1] * f12
  }
  
  if (is.na(tri[3, 3])) {
    tri[3, 3] = tri[3, 2] * f23
  }
  
  # ultimate claims = last dev period × tail factor
  ultimate = tri[, 3] * tail_factor
  
  result = cbind(
    tri,
    "Tail (Ult.)" = ultimate
  )
  
  list(triangle = result)
}

numericInput(
  "tail_factor",
  "Tail Factor",
  value = 1.10,
  min = 1,
  step = 0.01
)


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




server = function(input, output, session) 
  {
  
  # readfile
  claims_data = reactive({
    
    req(input$claims_file)
    
    read.csv(
      input$claims_file$datapath,
      stringsAsFactors = FALSE
    )
  })
  
  # run triangle calculation whenever data changes
  result = reactive({
    
    compute_triangle(
      claims = claims_data(),
      tail_factor = input$tail_factor
    )
  })
  
  # display triangle in table form
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
    # label each point for readability
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





# run shiny app
shinyApp(ui, server)