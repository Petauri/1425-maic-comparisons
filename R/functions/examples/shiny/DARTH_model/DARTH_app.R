#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

rm(list = ls())
library(shiny)
library(truncnorm)
library(DT)
library(matrixStats)



# source the sick sicker functions from online...
source("https://raw.githubusercontent.com/RobertASmith/paper_makeHEshiny/master/f_MM_sicksicker.R")
source("https://raw.githubusercontent.com/RobertASmith/paper_makeHEshiny/master/f_gen_psa.R")
source("https://raw.githubusercontent.com/RobertASmith/paper_makeHEshiny/master/f_wrapper.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Darth SSD model"),
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = "i_iterations",
        label = "Number of iterations",
        value = 1000,
        min = 500,
        max = 5000,
        step = 100
      ),
      numericInput(
        inputId = "i_c_Trt",
        label = "Cost of the intervention treatment",
        value = 1000,
        min = 0,
        max = 100000,
        step = 1
      ),
      numericInput(
        inputId = "i_ceacMin",
        label = "min WTP for 1 QALY in CEAC",
        value = 0,
        min = 0,
        max = 100000,
        step = 1
      ),
      numericInput(
        inputId = "i_ceacMax",
        label = "max WTP for 1 QALY in CEAC",
        value = 100000,
        min = 0,
        max = 100000,
        step = 1
      ),
      actionButton(
        inputId = "b_runModel",
        label = "Run the model"
      )
    ),
    mainPanel(
      DTOutput("ICER_table"),
      plotOutput("CE_plane_inc"),
      plotOutput("CEAC_plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  f_test_cost <- function(c_Trt, n_age_init = 50, n_sim = 5000) {
    
    matrix(
      c(c_Trt, unlist(lapply(c_Trt, function(this_c_Trt) {
        res <- f_wrapper(c_Trt = this_c_Trt, n_age_init = n_age_init, n_sim = n_sim)
        return(mean(res[,"Cost_Trt"] - res[,"Cost_NoTrt"]) / mean((res[,"QALY_Trt"] - res[,"QALY_NoTrt"])))
      }))),
      ncol = 2,
      dimnames = list(NULL,c("tx_cost", "ICER"))
    )
  }
  
  observeEvent(
    input$b_runModel, {
      
      # Get the set of inputs required to run the model:
      iterations <- input$i_iterations
      c_Trt      <- input$i_c_Trt
      ceacMin    <- input$i_ceacMin
      ceacMax    <- input$i_ceacMax
      
      # Use these to run the model:
      psa_res <- f_wrapper(c_Trt = c_Trt, n_sim = iterations)
      
      psa_res$icost <- psa_res$Cost_Trt - psa_res$Cost_NoTrt
      psa_res$iqaly <- psa_res$QALY_Trt - psa_res$QALY_NoTrt
      
      v_wtp <- seq(ceacMin,ceacMax,100)
      
      # Compute a CEAC from these results:
      
      CEAC <- vapply(X = v_wtp, FUN.VALUE = numeric(1), FUN = function(wtp) {
        length(which(psa_res[,"ICER"] <= wtp)) / nrow(psa_res) 
      })
      
      # Compile the CEAC
      CEAC <- data.frame(
        v_wtp,
        CEAC
      )
      
      mean_results <- colMeans(psa_res)
      
      ICER_table <- data.frame(
        treatment = c("No treatment", "Treatment"),
        costs = c(mean_results["Cost_NoTrt"]  , mean_results["Cost_Trt"]),
        qalys = c(mean_results["QALY_NoTrt"]  , mean_results["QALY_Trt"]),
        icost = c(NA,mean_results["Cost_Trt"] - mean_results["Cost_NoTrt"]),
        iqaly = c(NA,mean_results["QALY_Trt"] - mean_results["QALY_NoTrt"]),
        ICER = c(NA,(mean_results["Cost_Trt"] - mean_results["Cost_NoTrt"]) / (mean_results["QALY_Trt"] - mean_results["QALY_NoTrt"]))
      )
      
      CE_plane_inc <- ggplot2::ggplot(data = psa_res, ggplot2::aes(x = iqaly, y = icost)) + 
        ggplot2::geom_point() + 
        ggplot2::theme_classic() +
        ggplot2::scale_x_continuous(limits = c(-1.5,1.5)) +
        ggplot2::scale_y_continuous(limits = c(-15000,15000)) + 
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::geom_vline(xintercept = 0)
      
      CEAC_plot <- ggplot2::ggplot(data = CEAC, ggplot2::aes(x = v_wtp , y = CEAC)) + 
        ggplot2::geom_line() + 
        ggplot2::theme_classic() +
        ggplot2::scale_y_continuous(limits = c(0,1)) +
        ggplot2::scale_x_continuous(limits = c(0,max(CEAC$v_wtp)))
      
      output$ICER_table <- renderDT({
        datatable(
          data = ICER_table,
          rownames = FALSE,
          options = list(
            "searching" = FALSE,
            "paging" = FALSE
          )) %>%
          DT::formatCurrency(columns = c(2,4,6), currency = "£", digits = 0) %>%
          DT::formatRound(columns = c(3,5), digits = 3)
      })
      output$CE_plane_inc <- renderPlot({CE_plane_inc})
      output$CEAC_plot <- renderPlot({CEAC_plot})
      
    }
  )
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
