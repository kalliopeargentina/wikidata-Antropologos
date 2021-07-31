#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(visNetwork)
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  fluidRow(
    # # Display two tables: one with the nodes, one with the edges.
    # column(
    #   width = 6,
    #   tags$h1("Nodes in the graph:"),
    #   tableOutput("all_nodes"),
    #   tags$h1("Edges in the graph:"),
    #   tableOutput("all_edges")
    # ),
    # # The graph.
    column(
      width = 12,
      visNetworkOutput("editable_network", height = "800px")
    )
  )
))
