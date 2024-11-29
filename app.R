library(shiny)

#load pre-trained model
final_model<-readRDS("markov_model.rds")

#set up UI
ui<-fluidPage(
  titlePanel("Markov Model Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("steps","Number of Steps:",value=50,min=1),
      sliderInput("pHealthyToSick","P(Healthy → Sick):",min=0,max=1,value=0.2),
      sliderInput("pSickToHealthy","P(Sick → Healthy):",min=0,max=1,value=0.3),
      actionButton("simulate","Simulate")
    ),
    mainPanel(
      plotOutput("statePlot"),
      tableOutput("stateTable")
    )
  )
)

#set up server
server<-function(input,output) {
  simulate<-eventReactive(input$simulate, {
    #transition matrix based on user input
    transition_matrix<-matrix(
      c(1-input$pHealthyToSick,input$pHealthyToSick,
        input$pSickToHealthy,1-input$pSickToHealthy),
      nrow=2,byrow=TRUE
    )
    
    #define states
    states<-c("Healthy","Sick")
    
    #simulate Markov process
    simulate_markov(transition_matrix,states,1,input$steps)
  })
  
  output$statePlot <- renderPlot({
    states<-simulate() #simulate states
    state_labels<-unique(states) #unique state names
    state_numeric<-match(states,state_labels)  #map states to numeric indices
  
    #plot state progression
    plot(
      x=1:length(states),
      y=state_numeric,
      type="b",
      col="blue",
      pch=16,
      ylab="State",
      xlab="Step",
      xaxt="n",
      yaxt="n"
    )
    axis(2,at=1:length(state_labels),labels=state_labels) #y-axis with state names
    axis(1,at=1:length(states),labels=1:length(states)) #x-axis with step numbers
  })
  
  output$stateTable<-renderTable({
    data.frame(Step=1:input$steps,State=simulate())
  })
}

#finally, run the app
shinyApp(ui=ui,server=server)
