#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

dashboardPage(
    freshTheme = create_theme(
        adminlte_color(
            light_blue = "#55e7ff",
            blue       = "#2011a2",
            navy       = "#201148",
            red        = "#ff34b3"
        ),
        adminlte_sidebar(
            dark_bg       = "#D8DEE9",
            dark_hover_bg = "#81A1C1",
            dark_color    = "#2E3440"
        ),
        adminlte_global(
            content_bg  = "#FFF",
            box_bg      = "#D8DEE9", 
            info_box_bg = "#D8DEE9"
        )
    ),
    options = list(sidebarExpandOnHover = TRUE),
    header = dashboardHeader(
        title  = "Shiny CEM examples",
        leftUi = tagList(
            actionButton("confirm_i_reg_n_reg",label = "Update drug costs"),
        )
    ),
    sidebar = dashboardSidebar(
        minified  = FALSE,
        collapsed = TRUE,
        "to add tabs in here to change the page"
    ),
    body = dashboardBody(
        fluidPage(
            shiny::sidebarLayout(
                fluid = TRUE,
                sidebarPanel = sidebarPanel(
                    shiny::uiOutput("ui_i_reg_n_reg"),
                    downloadButton("save_debug_i",label = "Click to save input list"),
                    downloadButton("save_debug_L",label = "Click to save L"),
                    downloadButton("save_debug_R",label = "Click to save R"),
                    downloadButton("save_S",label = "Click to save S"),
                    hr(),
                    h3("Load in a previous snapshot file"),
                    fileInput(
                        inputId     = "load_S",
                        label       = "Upload a previous snapshot (S)",
                        multiple    = FALSE,
                        accept      = "*.rds",
                        width       = NULL,
                        buttonLabel = "Load previous state",
                        placeholder = "No file selected",
                        capture     = NULL
                    ),
                    hr(),
                    fluidRow(
                        width = 12,
                        box(
                            title = "Select regimen and combinate:",
                            status = "success",
                            solidHeader = TRUE,
                            width = 12,
                            collapsible = FALSE,
                            uiOutput("ui_drug_cost_select")
                        )
                    ),
                    hr(),
                    fluidRow(
                        width = 12,
                        box(
                            title = "view inputs",
                            status = "danger",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            width = 12,
                            h1("Debug output (inputs)"),br(),verbatimTextOutput("debugi")
                        )
                    )
                ),
                mainPanel = mainPanel(
                    shiny::uiOutput("ui_drug_cost_inputs"),
                    hr(),
                    box(
                        title = "QC",
                        status = "danger",
                        solidHeader = TRUE,
                        width = 12,
                        collapsible = TRUE,
                        collapsed = TRUE,
                        fluidRow(
                            shiny::column(6,h1("Debug output (L)"),br(),verbatimTextOutput("debugL")),
                            shiny::column(6,h1("Debug output (R)"),br(),verbatimTextOutput("debugR"))
                        )
                    )
                )
            )
            
        )
    ),
    controlbar = dashboardControlbar(),
    title      = "DashboardPage"
)

# Define UI for application that draws a histogram
# fluidPage(
# 
#     # Application title
#     titlePanel(h1("DSLR simple (as possible) application")),
# 
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             shiny::uiOutput("ui_i_reg_n_reg"),
#             actionButton("confirm_i_reg_n_reg",label = "Confirm drug cost input changes"),
#             downloadButton("save_debug_i",label = "Click to save input list"),
#             downloadButton("save_debug_L",label = "Click to save L"),
#             downloadButton("save_debug_R",label = "Click to save R"),
#             hr(),
#             fluidRow(
#                 width = 12,
#                 h1("Select regimen and combinate:"),
#                 uiOutput("ui_drug_cost_select")
#             ),
#             hr(),
#             fluidRow(
#                 width = 12,
#                 h1("Debug output (inputs)"),br(),verbatimTextOutput("debugi")
#             )
#             ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#             fluidRow(
#                 width = 12,
#                 box(
#                     title = "Drug cost input box",
#                     status = "primary",
#                     solidHeader = TRUE,
#                     width = 12,
#                     collapsible = FALSE,
#                     closable = FALSE,
#                     shiny::uiOutput("ui_drug_cost_inputs")
#                 )
#             ),
#             br(),
#             fluidRow(
#               shiny::column(6,h1("Debug output (L)"),br(),verbatimTextOutput("debugL")),
#               shiny::column(6,h1("Debug output (R)"),br(),verbatimTextOutput("debugR"))
#             )
#         )
#     )
# )


