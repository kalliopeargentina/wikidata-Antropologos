#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
require(visNetwork)
library(dplyr)
library(visNetwork)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  #Load dataset
  #Nodes
  nodes <- read.csv('personas.csv',stringsAsFactors = FALSE,fileEncoding = "UTF-8")
  nodes <- select(nodes, -c(4:7))
  colnames(nodes) <- c("id", "label","title")
  nodes <- nodes %>% mutate(font.size = 20)
  #Edges
  edges <- read.csv('relaciones.csv',stringsAsFactors = FALSE,fileEncoding = "UTF-8")
  colnames(edges) <- c("from", "to", "label")
  
  #lnodes <-  data.frame(color = c("lightblue", "red"), 
  #                      label = c("reverse", "depends"), 
  #                      font.align = "top") 
  
  
  # Initialize the graph with these nodes/edges.  We have to assign edges an ID
  # in case the user edits them later.
  init.nodes.df <- nodes
  init.edges.df <- edges
  
  # `graph_data` is a list of two data frames: one of nodes, one of edges.
  graph_data = reactiveValues(
    nodes = init.nodes.df,
    edges = init.edges.df
  )
  
  # Render the graph.
  output$editable_network <- renderVisNetwork({
    visNetwork(graph_data$nodes, graph_data$edges,width = "100%",
               main = "8 Antropólogos",
               footer = "por Marcos Buccellato",
               submain = "Relaciones que se pueden encontrar en wiki pedia seleccionando a 8 antropologos:
               Leo Frobenius, Bronislaw Malinowski, Marcel Mauss, James Frazer, Franz Boas, Edward Burnett Tylor, Alfred Radcliffe-Brown, Fritz Graebner"
    ) %>%
      visNodes(
        shape = "dot",
        color = list(
          background = "#0085AF",
          border = "#013848",
          highlight = "#FF8000"
        ),
        shadow = list(enabled = TRUE, size = 15),
        scaling = list("label" = list("enabled" = TRUE))
        
      ) %>%
      visEdges(
        shadow = FALSE,
        color = list(color = "#0085AF", highlight = "#C62F4B"),
        arrows = "middle",
        smooth = TRUE
        
      ) %>%
      visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
                 selectedBy = "label",manipulation = FALSE) %>% 
      #  visLayout(randomSeed = 11, improvedLayout = TRUE ) %>%
      #  visLegend(addNodes = lnodes, useGroups = FALSE) 
      visInteraction(navigationButtons = TRUE) %>%
      visPhysics( enabled = TRUE, solver = "forceAtlas2Based",forceAtlas2Based = list(gravitationalConstant = -50))
    
  })
  
  # If the user edits the graph, this shows up in
  # `input$[name_of_the_graph_output]_graphChange`.  This is a list whose
  # members depend on whether the user added a node or an edge.  The "cmd"
  # element tells us what the user did.
  observeEvent(input$editable_network_graphChange, {
    # If the user added a node, add it to the data frame of nodes.
    if(input$editable_network_graphChange$cmd == "addNode") {
      temp = bind_rows(
        graph_data$nodes,
        data.frame(id = input$editable_network_graphChange$id,
                   label = input$editable_network_graphChange$label,
                   stringsAsFactors = F)
      )
      graph_data$nodes = temp
    }
    # If the user added an edge, add it to the data frame of edges.
    else if(input$editable_network_graphChange$cmd == "addEdge") {
      temp = bind_rows(
        graph_data$edges,
        data.frame(id = input$editable_network_graphChange$id,
                   from = input$editable_network_graphChange$from,
                   to = input$editable_network_graphChange$to,
                   stringsAsFactors = F)
      )
      graph_data$edges = temp
    }
    # If the user edited a node, update that record.
    else if(input$editable_network_graphChange$cmd == "editNode") {
      temp = graph_data$nodes
      temp$label[temp$id == input$editable_network_graphChange$id] = input$editable_network_graphChange$label
      graph_data$nodes = temp
    }
    # If the user edited an edge, update that record.
    else if(input$editable_network_graphChange$cmd == "editEdge") {
      temp = graph_data$edges
      temp$from[temp$id == input$editable_network_graphChange$id] = input$editable_network_graphChange$from
      temp$to[temp$id == input$editable_network_graphChange$id] = input$editable_network_graphChange$to
      graph_data$edges = temp
    }
    # If the user deleted something, remove those records.
    else if(input$editable_network_graphChange$cmd == "deleteElements") {
      for(node.id in input$editable_network_graphChange$nodes) {
        temp = graph_data$nodes
        temp = temp[temp$id != node.id,]
        graph_data$nodes = temp
      }
      for(edge.id in input$editable_network_graphChange$edges) {
        temp = graph_data$edges
        temp = temp[temp$id != edge.id,]
        graph_data$edges = temp
      }
    }
  })
  
  # Render the table showing all the nodes in the graph.
  output$all_nodes = renderTable({
    graph_data$nodes
  })
  
  # Render the table showing all the edges in the graph.
  output$all_edges = renderTable({
    graph_data$edges
  })
  
})
