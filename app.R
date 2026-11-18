Mining and Markets

library(shiny)
ui <- navbarPage(
  title = "Mining and Markets",
  tabPanel("About"),
  tabPanel("Interactive Map"),
  tabPanel("Graph 1"),
  tabPanel("Graph 2"),
  tabPanel("Data")
)
server <- function(input, output, session) {
}
shinyApp(ui = ui, server = server)

