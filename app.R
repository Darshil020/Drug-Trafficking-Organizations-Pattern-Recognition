#Author:
#Darshil GOhel : dXg163330
#Date : 31st August,2017


library(shiny)
#library(plotly)
library(shinythemes)
library(DT)
library(ggplot2)
library(igraph)
library(networkD3)
#install.packages("GGally")
#library(GGally)
#library(network)
#library(sna)
#library(ggplot2)
library(igraph)
library(magrittr)
library(visNetwork)
library(data.table)


ui = fluidPage(titlePanel("Darshil_Gohel_SNA"),
               theme = shinythemes::shinytheme("cosmo"),
                           
                          
                                    sidebarLayout(
                                      
                                      sidebarPanel(width=3,
                                                   
                                                   fluidRow(
                                                     column(12,
                                                            
                                                            fileInput('file11', 'Input the CSV file',accept=c('.csv'))
                                                     )      
                                                  )
                                        
                                      ),
                                      
                                      mainPanel(
                                        tabsetPanel(
                                          tabPanel("Actual Data",
                                                   fluidRow(
                                                     column(6,
                                                            
                                                            dataTableOutput("Values11")
                                                            
                                                     )
                                                     
                                                   )
                                          ),
                                          tabPanel("Tall Table",
                                                   fluidRow(
                                                     column(6,
                                                            
                                                            dataTableOutput("Values12")
                                                            
                                                     )
                                                     
                                                   )
                                          ),
                                          
                                          tabPanel("Answers 4-13", 
                                                   h4("4. How many calls were placed in total?"),
                                                   verbatimTextOutput("txtout11"),
                                                   
                                                   h4("5. Who received the most calls?"),
                                                   verbatimTextOutput("txtout12"),
                                                   
                                                   h4("6. Who placed the most calls?"),
                                                   verbatimTextOutput("txtout13"),
                                                   
                                                   h4("7. Assume that each call is, just a connection. Compute the degree centrality of each person. Display in a visual graph. The degree centrality of a node(person) i, can be defined as the total number of nodes connected to node ni."),
                                                   visNetworkOutput("viznet07"),
                                                   
                                                   
                                                   h4("8. Assume that each call is just a connection. Compute the betweenness centrality of each person. Display in a visual graph."),
                                                   visNetworkOutput("viznet08"),
                                                   
                                                   
                                                   h4("9. Using the calling/called information, and ignoring the number of calls placed,compute the indegree and outdegree centrality per person. Display in a visual graph. Indegree and outdegree centrality are simply the count of inwards and outwards directed connections to or from a node."),
                                                   fluidRow(
                                                     column(6,
                                                            h4("In Degree Centrality"),
                                                            visNetworkOutput("viznet09")
                                                            
                                                     ),
                                                     column(6,
                                                            h4("Out Degree Centrality"),
                                                            visNetworkOutput("viznet10")
                                                            
                                                     )
                                                     
                                                   ),

                                                   
                                                   
                                                   
                                                   
                                                   h4("10. Assume that a call is just an indication of a connection between two people and it does not matter who placed or received the call. Can you visualize this network?"),
                                                   fluidRow(
                                                     column(4
                                                            
                                                     ),
                                                     column(4,
                                                            h3("Total Network connections")
                                                            
                                                            
                                                     ),
                                                     column(4
                                                            
                                                     )
                                                     
                                                   ),
                                                   forceNetworkOutput("force10"),
                                                   
                                                   
                                                   h4("11. Assume that you are interested in only outbound calls (a directed arrow showing who called whom). Can you visualize this network?"),
                                                   fluidRow(
                                                     column(4
                                                            
                                                     ),
                                                     column(4,
                                                            h3("Outbound connections")
                                                            
                                                            
                                                     ),
                                                     column(4
                                                            
                                                     )
                                                     
                                                   ),
                                                   forceNetworkOutput("force11"),
                                                   
                                                   
                                                   h4("12. Assume that you are interested in only inbound calls (a directed arrow showing who received the call from whom). Can you visualize this network?"),
                                                   fluidRow(
                                                     column(4
                                                            
                                                     ),
                                                     column(4,
                                                            h3("Inbound connections")
                                                            
                                                            
                                                     ),
                                                     column(4
                                                            
                                                     )
                                                     
                                                   ),
                                                   forceNetworkOutput("force12"),
                                                   
                                                   h4("13. Compare the three networks you have visualized. What are your observations?"),
                                                   verbatimTextOutput("txtout19"),
                                                   verbatimTextOutput("txtout20"),
                                                   verbatimTextOutput("txtout21")
                                                   
                                                   
                                                   )
                                         
                                          
                                        )
                                        
                                      )         
                                    )
                           
                           
                
                
)


server <- function(input, output) {
  dataInput11 <- reactive({
    #df2=read.table('C:/Users/dgohel/Documents/Darshil Data/D/ABA with R/COCAINE_DEALING.csv',sep=',',header=TRUE)
    inFile <- input$file11  
    validate(need(!is.null(inFile), "Please import the input file"))
    
    df=read.table(inFile$datapath,header=TRUE,sep=',')
    head(df)
    rownames(df) <- df[,1]
    df <- df[ -c(1)];
    return(df)
    
    
  })
  
  
  
  output$Values11 <- DT::renderDataTable({
    df=dataInput11()
    df
    
  },options = list(pageLength = 50))  

  output$Values12 <- DT::renderDataTable({
    df=dataInput11()
    len=dim(df)[1]*dim(df)[2]
    
    result_df=data.frame(matrix(data=NA,nrow=dim(df)[1]*dim(df)[2],ncol=3))
    k=1
    for(i in colnames(df)){
      for( j in rownames(df)){
        result_df[k,1]=j
        result_df[k,2]=i
        result_df[k,3]=df[j,i]
        k=k+1;
      }
    }
    
    colnames(result_df)=c('calling','called','times')
    result_df
    
  },options = list(pageLength = 50))  
  
  
  output$txtout11=renderPrint({
    df=dataInput11()
    sum(result_df[,'times'])
    
  })
  
  output$txtout12=renderPrint({
    df=dataInput11()
    df_copy=df
    df_copy["Total" ,] <- colSums(df_copy)
    max_value=max(df_copy["Total" ,])
    Max_call_Reciver=rep(NA,dim(df_copy)[2])
    count1=1
    for(i in colnames(df_copy)){
      if(df_copy['Total',i]==max_value){
        Max_call_Reciver[count1]=i
        count1=count1+1
        }
      
    }
    Max_call_Reciver=Max_call_Reciver[!is.na(Max_call_Reciver)]
    Max_call_Reciver
    
  })
  
  output$txtout13=renderPrint({
    df=dataInput11()
    df_copy=data.frame(t(as.matrix(df)))
   
    df_copy["Total" ,] <- colSums(df_copy)
    Max_call_Dailer=colnames(df_copy['Total',])[apply(df_copy['Total',],1,which.max)]
    Max_call_Dailer
    
  })
  

  
  output$txtout19=renderPrint({
    print("Undirected Network : It can be easily understood from the graph that it is hierarcical from the point of seneority. The number of connection are varing person to person. E.g some persons are connected to only one person like Bill,Marky,Jenny. Hence,this people are at bottom of the network. Persons like steve,Tommy,Meena and Blacky who are actaully in contact with many people and hence it can be easily concluded that they are some what at senior level.Kay is the most senior person as he is connected to most of the people in the network.  The whole network is concentrated around Kay.")

  })
  output$txtout20=renderPrint({
    print("Out Bound Network : Highest Number of calls are done by Kay. So, it can be assumed that he can be central and important figure in the whole network and he often needs to get and give the imformation from the whole network.he has more oftenly called to some senior person like steve, Tommy, Meena etc than the any other individual. senior persons like steve, Tommy, Meena have called their junior members more often. hence, it can be assumed that senior people tends to get information from their respective junior members and pass it to Kay who is most senior member.")

  })
  output$txtout21=renderPrint({
    print("In Bound Networks: Highest Number of calls are received by Dante and Meena. So , it can be assumed that they are at very imporatant place. people often call them to either give or ask information. So, it can be very precious imformation transit via Dante and Meena.")
    
  })
  
  output$viznet07 <- renderVisNetwork({
    df=dataInput11()
    len=dim(df)[1]*dim(df)[2]
    
    result_df=data.frame(matrix(data=NA,nrow=dim(df)[1]*dim(df)[2],ncol=3))
    k=1
    for(i in colnames(df)){
      for( j in rownames(df)){
        result_df[k,1]=j
        result_df[k,2]=i
        result_df[k,3]=df[j,i]
        k=k+1;
      }
    }
    
    colnames(result_df)=c('calling','called','times')
    
    result_df_final=result_df[result_df$times!=0,]
    result_df_final_sorted=result_df_final[order(result_df_final$calling),]
    graph <- graph_from_data_frame(result_df_final_sorted, directed=T)
    graph <- simplify(graph)
    V(graph)$alldegree <- centr_degree(graph, mode = "all")$res
    nodes <- get.data.frame(graph, what="vertices")
    nodes <- data.frame(id = nodes$name, title = nodes$name, group = nodes$alldegree, alldegree = nodes$alldegree)
    nodes <- nodes[order(nodes$id, decreasing = F),]
    edges <- get.data.frame(graph, what="edges")[1:2]
    
    visNetwork(nodes, edges,main = "All Degree Centrality", height = "500px", width = "100%") %>%
      visOptions(selectedBy = "alldegree", highlightNearest = TRUE, nodesIdSelection = TRUE)%>%
      visPhysics(stabilization = FALSE)
  })  
  
  output$viznet08 <- renderVisNetwork({
    df=dataInput11()
    len=dim(df)[1]*dim(df)[2]
    
    result_df=data.frame(matrix(data=NA,nrow=dim(df)[1]*dim(df)[2],ncol=3))
    k=1
    for(i in colnames(df)){
      for( j in rownames(df)){
        result_df[k,1]=j
        result_df[k,2]=i
        result_df[k,3]=df[j,i]
        k=k+1;
      }
    }
    
    colnames(result_df)=c('calling','called','times')
    
    result_df_final=result_df[result_df$times!=0,]
    result_df_final_sorted=result_df_final[order(result_df_final$calling),]
    graph <- graph_from_data_frame(result_df_final_sorted, directed=F)
    graph <- simplify(graph)
    V(graph)$betweenness <- betweenness(graph)
    nodes <- get.data.frame(graph, what="vertices")
    nodes <- data.frame(id = nodes$name, title = nodes$name, group = nodes$betweenness, betweenness = nodes$betweenness)
    nodes <- nodes[order(nodes$id, decreasing = F),]
    edges <- get.data.frame(graph, what="edges")[1:2]
    
    visNetwork(nodes, edges,main = "Betweenness Centrality", height = "500px", width = "100%") %>%
      visOptions(selectedBy = "betweenness", highlightNearest = TRUE, nodesIdSelection = TRUE)%>%
      visPhysics(stabilization = FALSE)
    
  })  
  
  output$viznet09 <- renderVisNetwork({
    df=dataInput11()
    len=dim(df)[1]*dim(df)[2]
    
    result_df=data.frame(matrix(data=NA,nrow=dim(df)[1]*dim(df)[2],ncol=3))
    k=1
    for(i in colnames(df)){
      for( j in rownames(df)){
        result_df[k,1]=j
        result_df[k,2]=i
        result_df[k,3]=df[j,i]
        k=k+1;
      }
    }
    
    colnames(result_df)=c('calling','called','times')
    
    result_df_final=result_df[result_df$times!=0,]
    graph <- graph_from_data_frame(result_df_final_sorted, directed=T)
    graph <- simplify(graph)
    V(graph)$indegree <- centr_degree(graph, mode = "in")$res
    nodes <- get.data.frame(graph, what="vertices")
    nodes <- data.frame(id = nodes$name, title = nodes$name, group = nodes$indegree, indegree = nodes$indegree)
    nodes <- nodes[order(nodes$id, decreasing = F),]
    edges <- get.data.frame(graph, what="edges")[1:2]
    
    visNetwork(nodes, edges,main = "In Degree Centrality", height = "500px", width = "100%") %>%
      visOptions(selectedBy = "indegree", highlightNearest = TRUE, nodesIdSelection = TRUE)%>%
      visPhysics(stabilization = FALSE)%>% 
      visEdges(arrows = "to")
    
    
  })  
  output$viznet10 <- renderVisNetwork({
    df=dataInput11()
    len=dim(df)[1]*dim(df)[2]
    
    result_df=data.frame(matrix(data=NA,nrow=dim(df)[1]*dim(df)[2],ncol=3))
    k=1
    for(i in colnames(df)){
      for( j in rownames(df)){
        result_df[k,1]=j
        result_df[k,2]=i
        result_df[k,3]=df[j,i]
        k=k+1;
      }
    }
    
    colnames(result_df)=c('calling','called','times')
    
    result_df_final=result_df[result_df$times!=0,]
    graph <- graph_from_data_frame(result_df_final_sorted, directed=T)
    graph <- simplify(graph)
    V(graph)$outdegree <- centr_degree(graph, mode = "out")$res
    nodes <- get.data.frame(graph, what="vertices")
    nodes <- data.frame(id = nodes$name, title = nodes$name, group = nodes$outdegree, outdegree = nodes$outdegree)
    nodes <- nodes[order(nodes$id, decreasing = F),]
    edges <- get.data.frame(graph, what="edges")[1:2]
    
    visNetwork(nodes, edges,main = "Out Degree Centrality", height = "500px", width = "100%") %>%
      visOptions(selectedBy = "outdegree", highlightNearest = TRUE, nodesIdSelection = TRUE)%>%
      visPhysics(stabilization = FALSE)%>% 
      visEdges(arrows = "to")
    
  })  
  
  output$force10 <- renderForceNetwork({
    
    df=dataInput11()
    len=dim(df)[1]*dim(df)[2]
    
    result_df=data.frame(matrix(data=NA,nrow=dim(df)[1]*dim(df)[2],ncol=3))
    k=1
    for(i in colnames(df)){
      for( j in rownames(df)){
        result_df[k,1]=j
        result_df[k,2]=i
        result_df[k,3]=df[j,i]
        k=k+1;
      }
    }
    
    colnames(result_df)=c('calling','called','times')
    
    result_df_final=result_df[result_df$times!=0,]
    networkData=result_df_final[,c('calling','called')]
    simpleNetwork(networkData,opacity = 1)
    
  })
  
  output$force11 <- renderForceNetwork({
    
    df=dataInput11()
    len=dim(df)[1]*dim(df)[2]
    
    result_df=data.frame(matrix(data=NA,nrow=dim(df)[1]*dim(df)[2],ncol=3))
    k=1
    for(i in colnames(df)){
      for( j in rownames(df)){
        result_df[k,1]=j
        result_df[k,2]=i
        result_df[k,3]=df[j,i]
        k=k+1;
      }
    }
    
    colnames(result_df)=c('calling','called','times')
    
    result_df_final=result_df[result_df$times!=0,]
    
    Node_name=rownames(df)
    group_list=rep(0,length(Node_name))
    Node_df=data.frame(Node_name,group_list)
    Node_df$index=rownames(Node_df)
    
    result_df_final_sorted=result_df_final[order(result_df_final$calling),]
    
    colnames(result_df_final_sorted)[1]='Node_name'
    Link_df1=merge(x = result_df_final_sorted, y = Node_df, by = "Node_name", all.x = TRUE)
    colnames(Link_df1)[c(1,2,3,5)]=c('name','Node_name','value','source')
   
    
    Link_df2=merge(x = Link_df1, y = Node_df, by = "Node_name", all.x = TRUE)
    Link_df2[,'index']=as.numeric(Link_df2[,'index'])
    Link_df2[,'source']=as.numeric(Link_df2[,'source'])
    
    Link_df_final=Link_df2[order(Link_df2$source),c('source','index','value')]
    colnames(Link_df_final)[2]='target'
    
    
    Link_df_final$source=Link_df_final$source-1
    Link_df_final$target=Link_df_final$target-1
    
    forceNetwork(Links=Link_df_final,Nodes = Node_df,
                 Source = 'source', Target = 'target', 
                 NodeID = 'Node_name', Group = 'group_list',Value = "value",arrows = TRUE,zoom=TRUE,opacity = 1,opacityNoHover = 1,fontSize = 10)
  })
  
  output$force12 <- renderForceNetwork({
    
    df=dataInput11()
    len=dim(df)[1]*dim(df)[2]
    
    result_df=data.frame(matrix(data=NA,nrow=dim(df)[1]*dim(df)[2],ncol=3))
    k=1
    for(i in rownames(df)){
      for( j in colnames(df)){
        result_df[k,1]=j
        result_df[k,2]=i
        result_df[k,3]=df[i,j]
        k=k+1;
      }
    }
    
    colnames(result_df)=c('called','calling','times')
    result_df_final=result_df[result_df$times!=0,]
    result_df_final_sorted=result_df_final[order(result_df_final$called),]
    
    Node_name=rownames(df)
    group_list=rep(0,length(Node_name))
    Node_df=data.frame(Node_name,Node_name,group_list)
    Node_df$index=rownames(Node_df)
    
    colnames(Node_df)[c(1,2)]=c('called','calling')
    Link_df1=merge(x = result_df_final_sorted, y = Node_df[,c('called','index')], by = "called", all.x = TRUE)
    colnames(Link_df1)[4]='source'
    colnames(Link_df1)[3]='value'
    
    Link_df2=merge(x = Link_df1, y = Node_df[,c('calling','index')], by = "calling", all.x = TRUE)
    Link_df2[,'index']=as.numeric(Link_df2[,'index'])
    Link_df2[,'source']=as.numeric(Link_df2[,'source'])
    Link_df_final=Link_df2[order(Link_df2$source),c('source','index','value')]
    colnames(Link_df_final)[2]='target'
    
    
    Link_df_final$source=Link_df_final$source-1
    Link_df_final$target=Link_df_final$target-1
    
    forceNetwork(Links=Link_df_final,Nodes = Node_df,
                 Source = 'source', Target = 'target', 
                 NodeID = 'called', Group = 'group_list',Value = "value",arrows = TRUE,zoom=TRUE,opacity = 1,opacityNoHover = 1,fontSize = 10)
    
  })
 
  
}
shinyApp(ui, server)