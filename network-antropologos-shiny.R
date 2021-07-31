library(shiny)
library(visNetwork)
library(dplyr)
library(DT)
library(tidyverse)
#Nodes
nodes <- read.csv('personas.csv',stringsAsFactors = FALSE,fileEncoding = "UTF-8")
nodes <- select(nodes, -c(5:6))
nodes <- select(nodes, -c(1))
colnames(nodes) <- c("id", "title","image")
nodes <- nodes %>% mutate(font.size = 20)
#Edges
edges <- read.csv('relaciones-flourish.csv',stringsAsFactors = FALSE,fileEncoding = "UTF-8") %>%
relocate(Label, .after = last_col())
colnames(edges) <- c("from", "to", "label")

#lnodes <-  data.frame(color = c("lightblue", "red"), 
#                      label = c("reverse", "depends"), 
#                      font.align = "top") 


# Initialize the graph with these nodes/edges.  We have to assign edges an ID
# in case the user edits them later.
init.nodes.df <- nodes
init.edges.df <- edges

ui <- fluidPage(
  
  # Application title
  titlePanel("8 Antropólogos"),

  sidebarLayout(
    
    sidebarPanel(
      width = 4,
      # box(id = "myBox", title = "Datos", width = '100px',
      #     selectInput(inputId = "myInput", label = "my input", choices = c(letters)),
      #       # 
            # tags$h1("Nodes in the graph:"),
            # tableOutput("all_nodes"),
            # tags$h1("Edges in the graph:"),
            # tableOutput("all_edges")
            # 
          
          #verbatimTextOutput("shiny_return")
      htmlOutput("summary"),
      uiOutput("image1"),
      DTOutput('tbl')
      
          
      ),
      # actionButton(inputId = "button", label = "show / hide")

     mainPanel(
        visNetworkOutput("editable_network", height = "600px", width="100%")
    )
    
  )
  
)

server <- function(input, output) {
  
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
                 scaling = list("id" = list("enabled" = TRUE))
                
               ) %>%
      visEdges(
        shadow = FALSE,
        color = list(color = "#0085AF", highlight = "#C62F4B"),
        arrows = "middle",
        smooth = TRUE
        
      ) %>%
      visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
                 selectedBy = "id",manipulation = list(
                   enabled = TRUE,
                   editEdgeCols = c("label"),
                   addEdgeCols = c("from", "to", "label"),
                   editNodeCols = c("id", "title", "image"),
                   addNodeCols = c("id", "title", "image")
                 )) %>% 
      visInteraction(navigationButtons = TRUE) %>%
      visPhysics( enabled = TRUE, solver = "forceAtlas2Based",
                  forceAtlas2Based = list(gravitationalConstant = -50)) %>%
      visEvents(click = "function(nodes){
                  Shiny.onInputChange('current_node_selection', nodes.nodes[0]);
                  ;}",
                doubleClick = "function(nodes){
                  Shiny.onInputChange('current_node_selection', 'NAN');
                ;}"
      ) 
          
  
    
  })
  
  
  ## observe the button being pressed
  # observeEvent(input$button, {
  #   shinyjs::toggle("myBox")
  # })
  # 
  #observavle selection
  #render data table restricted to selected nodes
 
  observe({
    input$deselect
    input$current_node_selection
      
     if (!(is.null(input$current_node_selection)) && !(input$current_node_selection == "NAN")) {
     output$tbl <- renderDT(
        graph_data$edges %>% 
          filter((to %in% input$current_node_selection)|(from %in% input$current_node_selection)),
        extensions = 'Buttons',
        options = list(lengthChange = FALSE,
                       pageLength = 5,
                       dom = 'Bfrtip',
                       buttons = list(
                         list(
                           extend = "csv",
                           text = 'CSV Completo',
                           exportOptions = list(modifier = list(selected = FALSE))
                         ), 
                         list(
                           extend = "excel",
                           text = 'Excel Completo',
                           exportOptions = list(modifier = list(selected = FALSE))
                         )
                       )
        ),
        editable = list(target = "cell", disable = list(columns = c(1,2))),colnames = c('De','A','Tipo de Relación'),
        filter = 'top'
     ) 
     output$summary <- renderText((paste("<h1>",input$current_node_selection,"</h1>")))
     output$image1<- renderUI({
       img <- graph_data$nodes %>% filter((id == input$current_node_selection))
       
       imgurl2 <- img[3]
       tags$img(src=imgurl2, style ="height:100px;max-width:100px;")
     })
     } else {
       #TODO: no parecen estar funcionando las opciones de los botones. Se tuvo que usar server=FALSE.
       output$tbl <- renderDT(server = FALSE,
         graph_data$edges,
         extensions = 'Buttons',
         options = list(lengthChange = FALSE,
                        pageLength = 5,
                        dom = 'Bfrtip',
                        buttons = list(
                          list(
                            extend = "csv",
                            text = 'CSV Completo',
                            charset= 'UTF-8',
                            exportOptions = list(modifier = list(page = "all"))
                          ), 
                          list(
                            extend = "excel",
                            text = 'Excel Completo',
                            charset= 'UTF-8',
                            exportOptions = list(modifier = list(page = "all"))
                          )
                        )
         ),
         editable =  list(target = "cell", disable = list(columns = c(1,2))),colnames = c('De','A','Tipo de Relación'),
         filter = 'top'
       ) 
       
       output$summary <- renderText((paste("<h1> Datos Completos</h1>")))
     }
    
  })
  
    # ###EDITS DATATABLE
  proxyTbl <- dataTableProxy("tbl")
  observeEvent(input$tbl_cell_edit, {
    info <- input$tbl_cell_edit
    i <- info$row
    j <- info$col  
    v <- info$value
    graph_data[["edges"]][i, j] <- coerceValue(v, graph_data[["edges"]][i, j])
    
    #replaceData(proxyTbl,graph_data[["edges"]], resetPaging = FALSE, rownames = FALSE)  # important 
    #Update the network
    visNetworkProxy("network") %>%
      visUpdateEdges(edges = graph_data[["edges"]]) 
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
  # 
  # # Render the table showing all the nodes in the graph.
  # output$all_nodes = renderTable({
  #   graph_data$nodes
  # })
  # 
  # # Render the table showing all the edges in the graph.
  # output$all_edges = renderTable({
  #   graph_data$edges
  # })
  
}

shinyApp(ui = ui, server = server)