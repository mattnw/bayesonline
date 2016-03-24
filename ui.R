library(shinyIncubator)

shinyUI(fluidPage(
  titlePanel("Easy Bayesian data analysis online"),
  
  fluidRow(
    column(6,
           
      h2("Introduction"),
        p("This page allows you to perform Bayesian versions of several of the most commonly used
        data analyses in the social sciences. Bayesian analyses have a number of advantages over
        null hypothesis significance tests. Perhaps the most important advantage is that Bayesian
        tests", em("tell us what we want to know."), "Whereas a p value is statistic with a horrible
        interpretation (the probability of obtaining a test statistic as
        or more extreme than that observed if the null hypothesis is true), Bayesian analyses give us
        direct and intuitive results. For example, as you'll see on this page, you can very easily
        calculate the actual probability that a particular hypothesis is true!"),
      p("Historically, Bayesian analyses have been hard to do for computing reasons, but modern computer
        processors make them a lot more feasible in real research settings. That said, most true Bayesian
        analyses currently require users to be able to use computer code to run them. That's fine for some
        researchers, but for others it's a major barrier. In this app, you will be able to conduct
        Bayesian versions of several common analyses, without having to write any code or install any software. 
        I hope you find it helpful!"),
      p("-", a("Matt Williams", href = "http://www.massey.ac.nz/massey/expertise/profile.cfm?stref=235150"), " (2016)."),
      h3("Priors"),
      p("In a Bayesian analysis, we have to specify ahead of time what knowledge we have about the probable
        values of the parameters. I.e.., ", em("before"), "looking at the data, what do we know about what
        the true value of the slopes and intercept parameters (in the population)? In some cases, we might
        wish to pretend we knew nothing ahead of time and specify what are called ", em("uninformative"),
        "priors."),
      p("This is what is assumed by default in this application: The default priors for the model
        parameters (e.g., intercept and slopes) have mean zero and infinite variance. This represents a state
        of absolute ignorance before seeing the data: I.e., an assumption that absolutely any value of the
        parameters (from negative infinity to positive infinity) is equally likely. If you know a bit about
        Bayesian analysis, you may instead wish to specify ", em("informative priors"), "that clearly
        specify what prior knowledge you have (e.g., that small effects are more likely than large ones. 
        The shape of the prior distribution for the slopes and intercepts is assumed to have a normal distribution
        shape."),
      
    
    br(),
           h2("How to use this application for specific analyses"),
                     h4("Comparing two means (like in an independent samples t-test) "),
                     p("Simply specify the dichotomous grouping variable as the IV, with dummy coding such that 
        one group is coded as 0 and the other as 1. The coefficient for the group variable indicates the mean difference."),
                     h4("Comparing more than two means (like in ANOVA)"),
                     p("Create K-1 dummy variables, where K is the number of groups. Specify each of the dummy variables as an IV.
        The coefficient for each dummy variable indicates the mean difference between the group that variable refers to
        and the reference group"),
                     h4("Correlation"),
                     p("Convert the two variables to z scores, save the z scores in the .csv file, and then upload it. You can
        now  specify one variable as the IV, and the other as the DV (it doesn't matter which way round).
        The coefficient for the 'IV' is   the correlation."),
                     h4("Logistic regression"),
                     p("Ensure that your dependent variable only has values of 0 or 1. Select the binomial logistic regression option."),
    h4("Poisson regression"),
    p("Ensure that your dependent variable only has integer values of 0 o greater. Select the Poisson regression option."),                 
    h4("Support for repeated measures"),
                     p("This application does not currently provide support for repeated measures data (e.g., Bayesian equivalents
        of repeated measures ANOVA or paired t-tests. Support for Bayesian mixed models is planned for future
        development, which will allow analysis of repeated measures data."),
        br(),

        h2("Technical details"),
        p("Technically, what this page allows you to accomplish is to estimate a a Bayesian generalized linear model, 
              using the, ",
          a("MCMCpack", href = "https://cran.r-project.org/web/packages/MCMCpack/index.html"), "package in ",
          a("R.", href = "https://www.r-project.org/"),
          "Importantly, many popular statistical tests are just special cases of generalized linear models,
              so this means that you can  perform Bayesian equivalents of a number of statistical tests that we
              usually think of as being distinct from another."),
        h4("Priors: Additional information"),
        p("This application assumes a diagonal multivariate normal prior on the scale (slope and intercept) parameters. 
              Non-informative priors are assumed by default. Informative 
              priors can currently be specified for the scale parameters (i.e., slopes and intercepts),
              but not nuisance parameters such as error variances. Informative priors for nuisance parameters and 
              non-diagonal priors for the scale parameters can be 
              specified when using MCMCpack within an R environment. See the", 
          a("MCMCpack", href= "https://cran.r-project.org/web/packages/MCMCpack/MCMCpack.pdf"),
          "documentation for details."),
        h4("Distributional assumptions"),
        p("Bayesian analyses, like essentially all statistical analyses, have assumptions. In this application,
              the following assumptions are made for all models:"),
    tags$ul(
        tags$li("That, for any combination of values on the predictors, the error terms each have a mean of zero (this 
              crucial assumption would be violated in the case of unmodelled linearity or measurement error in the predictor
              variables, amongst other scenarios)"),
        tags$li("That the error terms are independent (as might be violated, for example, if the error terms were autocorrelated")
        ),
        p("Furthermore, specific analysis types have further assumptions:"),
        tags$ul(
        tags$li("For normal linear regression: That the errors are normally distributed and have the same variance for any combination
              of values on the predictors"),
        tags$li("For binomial logistic regression: That for any given combination of values of the predictors, the response variable
              takes a binomial distribution"),
        tags$li("For Poisson regression: That, for any given combination of values of the predictors, the response variable
              takes a Poisson distribution (a Poisson distribution has mean = variance)"))
    
      ),
     
       column(6,
              h2("Input your data and analysis details"),         
      fileInput("file", label = "Input datafile", accept = c(".csv")),
      helpText("Upload your data in .csv format. If you have any nominal variables, 
                          ensure they are dummy-coded appropriately."),
      selectInput("family", "Select your model type/family",
                  choices = list("Normal linear regression", "Binomial logistic regression", 
                                 "Multinomial logistic regression", "Poisson regression"),
                  selected = "Normal linear regression"),
              selectInput("DV", "Select your response/'dependent' variable", ""),
              selectInput("IVs", "Select your predictor/'independent' variables", "", multiple = TRUE),      
              textInput("interacts", "Specify any interaction terms"),
              helpText("Specify interaction terms in the format",
                 code("IV1*IV2 + IV3*IV4")),
      selectInput("factors", "Are any of your variables nominal?", "", multiple = TRUE),
      br(),
      h4("Priors"),
      p("Parameter", "Prior mean", "Prior variance"),
      uiOutput("matrix"),        
      actionButton("analysis","Analyze!"),    
    br(),
    br(),
      h2("Model output"),
              h3("Model summary"),
    p("The model summary below includes two important pieces of output:"),
    tags$ul(
      tags$li("The posterior mean for each coefficient (i.e., the best estimate of the parameter)"), 
      tags$li("The 2.5% and 97.5% percentiles of the MCMC distribution for each coefficient. This interval can be
              interpreted as a", a("credible interval", href = "https://en.wikipedia.org/wiki/Credible_interval"),
              "which ", a("(unlike a confidence interval)", href = "http://link.springer.com/article/10.3758/s13423-015-0947-8"),
              ", has a very nice
              intuitive interpretation: There is literally a 95% probability that the true parameter falls within
              the stated interval.")),
                 tableOutput("modelSummary"),
  h3("Posterior probability that each parameter is positive"),
  p("The following statistics directly indicate the probability that the true value of each parameter
        in the population is ", em("positive"), "(presuming that the model assumptions are correct):"),
  verbatimTextOutput("probs"),
                 h3("Trace and density plots"),
      p("There are two important sets of plots below. The trace plots for each parameter show how the estimates
        of this parameter varied across the different iterations of the model. This should look like a '
        hairy caterpillar', with lots of random wiggling up and down. The density plots, on the other hand,
        show a complete representation of what the most (and least) probable values of each parameter are."),
                 plotOutput("plots")
                 
     
  
    ))
  ))
