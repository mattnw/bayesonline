#Code to run once when app is launched only

std.fun = function(x){
  (x-mean(x, na.rm = TRUE)/sd(x, na.rm = TRUE))
}

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
    inputId = "z",
    choices=c(input$IVs,input$DV))
})

mod.form = reactive({
  paste(input$DV, " ~ ", paste(input$IVs, sep = "", collapse = " + "), interactions())
})

terms = reactive({
  names(lm(formula(mod.form()), data = head(myData()))$coefficients)})

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
    {model = MCMCmnl(formula(mod.form()), data = useData(), b0 = b0(), B0 = B0())}
  else {model = MCMCpoisson(formula(mod.form()), data = myData(), b0 = b0(), B0 = B0())}
  
  output$modelSummary <- renderTable({
    data.frame(Coefficient = rownames(summary(model)$quantiles),
                            Mean = as.numeric(summary(model)$statistics[,1]),
                            "Percentile2.5" = as.numeric(summary(model)$quantiles[,1]),
                            "Percentile95" = as.numeric(summary(model)$quantiles[,5]))
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