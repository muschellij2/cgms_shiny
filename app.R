## app.R ##
library(shinydashboard)
library(gluc)
library(readxl)
library(scales)
library(matrixStats)
library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)

make_time_only = function(timevar, crap_date = "1900-01-01") {
  hour = strftime(timevar, format = "%H:%M")
  hour = ymd_hm(paste(crap_date, hour))
}

ui <- dashboardPage(
  dashboardHeader(title = "Interactive Display of CGM Data"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Single Image Plots",
      tabName = "single_plots"
    )
  )),
  dashboardBody(
    # useShinyjs(),
    tabItems(
      # First tab content
      tabItem(
        tabName = "single_plots",
        fileInput(inputId = "infile", label = "Input File"),
        selectInput("device", "Which Device:", 
                    choices = c("Abbott", "Dexcom")),
        dataTableOutput("dtab"),
        box(plotlyOutput("plot1"), width = NULL)
      )
    ))
)


server <- function(input, output) {
  
  get_data = reactive({
    fname = input$infile$datapath
    device = input$device
    validate(
      need(!is.null(fname), "Please select a data set"),
      need(!is.null(device), "Please select a device")
    )
    if (is.null(device) | is.null(fname)) {
      return(NULL)
    } 
    func = switch(device,
                  Abbott = read_abbott_export,
                  Dexcom = read_dexcom_export)
    res = func(fname, complete = FALSE)
    res$glucose
  })
  
  # output$plot1 <- renderPlot({
  output$plot1 <- renderPlotly({
    run_df = get_data()
    if (!is.null(run_df)) {
      g = ggplot(data = run_df, aes(x = time, 
                                    y = glucose)) + 
        geom_line()
      g = g + guides(colour = FALSE)
      g 
    }
  })
  
  output$dtab = renderDataTable({
    run_df = get_data()
    run_df
  }, options = list(pageLength = 5)
  )
  
}

shinyApp(ui, server)
