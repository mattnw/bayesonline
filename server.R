#Code to run once when app is launched only
library(MCMCpack)

#Code to take a term in a string version of a formula and treat it as a factor

#Code to run for each new visitor
#A code snippet within a render* or reactive function is run each time 
  #that a widget that the snippet depends on is changed
shinyServer(function(input, output, session) {

  inFile <- reactive({
    if (is.null(input$file)) {
      return(NULL)
    } else {
      input$file
    }
  })
  
  myData <- reactive({
    if (is.null(inFile())) {
      return(NULL)
    } else {
      data = read.csv(inFile()$datapath)
      
    }
  })
  
  interactions <- reactive({
  if (input$interacts == ""){
    return("")
  } else paste("+", input$interacts)
  })
  
observe({
  updateSelectInput( 
    session,
    inputId = "DV",
    choices=names(myData()))
})

observe({
  updateSelectInput( 
    session,
    inputId = "IVs",
    choices=names(myData()))
})

observe({
  updateSelectInput( 
    session,
    inputId = "factors",
    choices=c(input$IVs,input$DV))
})

mod.form = reactive({
  IVsfactorised <- sapply(input$IVs, function(IV){
    if (IV %in% input$factors) {paste("as.factor(", IV, ")", sep = "")}
    else {IV}
  })
  DVfactorised <- if (input$DV %in% input$factors) {paste("as.factor(", input$DV, ")", sep = "")} else input$DV
  paste(DVfactorised, " ~ ", paste(IVsfactorised, sep = "", collapse = " + "), interactions())
  })

terms = reactive({ #Got to be a way to do this without fitting the model (but perhaps who cares given the MCMC iterations?)
  names(lm(formula(mod.form()), data = myData())$coefficients)})

df = reactive({ 
  if (is.null(input$IVs) & (input$interacts == "")){
    return(data.frame(ModelTerms = c("None"), PriorMeans = c(0), PriorPrecisions = c(0)))}
  else {data.frame(ModelTerms = terms(), PriorMeans = rep(0, times = length(terms())),
                   PriorPrecisions = rep(0, times = length(terms()))
                   )}})

output$matrix <- renderUI({
  matrixInput("Priors", "", data = df())
})

b0 = reactive({as.vector(input$Priors[,2])})
B0 = reactive({as.vector(input$Priors[,3])})

observeEvent(input$analysis, {
  if (input$family == "Normal linear regression")  
  {model = MCMCregress(formula(mod.form()), data = myData(), b0 = b0(), B0 = B0())}
  else if (input$family == "Binomial logistic regression")
    {model = MCMClogit(formula(mod.form()), data = myData(), b0 = b0(), B0 = B0())}
  else if (input$family == "Multinomial logistic regression")
    {model = MCMCmnl(formula(mod.form()), data = myData(), b0 = b0(), B0 = B0())}
  else {model = MCMCpoisson(formula(mod.form()), data = myData(), b0 = b0(), B0 = B0())}
  
  output$modelSummary <- renderTable({
    data.frame(Coefficient = rownames(summary(model)$quantiles),
                            Mean = as.numeric(summary(model)$statistics[,1]),
                            "Percentile2.5" = as.numeric(summary(model)$quantiles[,1]),
                            "Percentile95" = as.numeric(summary(model)$quantiles[,5]),
               std.coefs = as.numeric(summary(model)$statistics[,1])*(sapply(X = rownames(summary(model)$quantiles), FUN = function(X){
                 if (X %in% colnames(myData()))
                 sd(myData()[,paste(X)], na.rm = TRUE)
               else NA})/sd(myData()[,paste(input$DV)], na.rm = TRUE))
    )
               
  }, digits = 3)
  output$plots <- renderPlot({
    plot(model)
  })
  output$probs <- renderPrint({
    apply(model, 2, FUN = function(X){
      count = sum(X > 0)
      reps = nrow(model)
      count/reps
    })
  })
})
})