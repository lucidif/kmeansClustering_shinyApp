myUI<- shinyUI(
    fluidPage(
        fluidRow(column(3,
                        wellPanel(
                            fluidRow(column(12,
                                    fileInput("inFile",
                                              label="load file"
                                              )
                                        )
                                     ),
                            
                            
                            
                            fluidRow(column(12,
                                            
                                    uiOutput("xVarSlot")

                                            )
                                     ),
                            fluidRow(column(12,
                                    uiOutput("yVarSlot")

                                            )
                            ),
                            
                            fluidRow(
                                column(12,
                                       selectInput("alg",label="methods"
                                                   , choices=c("k-means", "pam")
                                                   )
                                       )
                            ),
                            
                            fluidRow(column(12,
                                    numericInput("clCount",
                                        label="cluster count",
                                        value=1
                                    )        
                                            )
                            )
                        )
                        
                        ),
                 column(9,
                        
                        fluidRow(plotOutput("plot1")),
                        fluidRow(plotOutput("plot2"))
                        )
                 )
        
    )
)

myServer<- shinyServer(function(input,output){
   
  ##requirements "cluster"  CRAN package
   
    reac<-reactiveValues()
   
    observeEvent(input$inFile,{
        reac$myTab<-read.table(
            input$inFile$datapath,
            sep="\t", header=TRUE,
            row.names=1)
        reac$cona<-colnames(reac$myTab)
        
        output$xVarSlot<-renderUI({
            selectInput("xVar",
                        label="x variable",
                        choices=reac$cona
                        )
        })
        output$yVarSlot<-renderUI({
            selectInput("yVar",
                        label="y variable",
                        choices=reac$cona
                    )
        })
        
        
    })#end observe
    
    output$plot1<-renderPlot({
        
        reac$myTabSub<-reac$myTab[, c(input$xVar, input$yVar)]
        
        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
        
        if (input$alg=="k-means"){
            reac$km<-kmeans(reac$myTabSub, input$clCount)
            plot(reac$myTabSub, col=reac$km$cluster, pch=20, cex=3)
            
            points(reac$km$centers, pch=4, cex=4, lwd=4)
        } else {#input$alg=="pam"
            reac$pam<- cluster::pam(reac$myTabSub, input$clCount)
            plot(reac$myTabSub, col=reac$pam$clustering, pch=20, cex=3)
        }
        
        # reac$myTabSub<-reac$myTab[, c(input$xVar, input$yVar)]
        # 
        # reac$km<-kmeans(reac$myTabSub, input$clCount)
        

        

        
    })
    
    output$plot2<-renderPlot({
        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
        reac$tab<-table(reac$km$cluster)
        #tabTT<<-reac$tab
        barplot(reac$tab, col=rownames(reac$tab))
    })
    
    
})

shinyApp(server=myServer, ui=myUI)