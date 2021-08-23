library(shiny)
library(bnlearn)
library(gRain) # for exact inference
library(tidyverse) # ggplot


# data 
capital <- read.csv("capital_under_combs.csv") # main data
comb <- read.csv("combinations.csv", colClasses = "factor") # combinations

n_scenario <- sum(capital$comb_number==0) # number of scenarios
comb1 <- comb[rep(1:nrow(comb), each = n_scenario), ]
names(comb1)[1] <- "scenario"
capital <- cbind(capital, comb1)

# cyclones
cycl_year_1 <- c("Carlos", "Enawo", "Harvey", "Irma", "Maria", "Ophelia")
cycl_year_2 <- c("Berguitta", "Fakir", "Isaac", "Helene", "Kirk", "Leslie", "Ava")

capital <- capital[c("capy1", "capy2", "haz_incr", "exp_incr", "cap_incr", 
                     cycl_year_1, cycl_year_2)]
capital$haz_incr <- as.factor(capital$haz_incr)
capital$exp_incr <- as.factor(capital$exp_incr)
capital$cap_incr <- as.factor(capital$cap_incr)


# create DAG
nodes_year_1 <- paste0("[", paste0(cycl_year_1, collapse="]["), "]")
nodes_year_2 <- paste0("[", paste0(cycl_year_2, collapse="]["), "]")
parents_year_1 <- paste0(cycl_year_1, collapse=":")
parents_year_2 <- paste0(cycl_year_2, collapse=":")

dag <- model2network(paste0("[haz_incr][exp_incr][cap_incr]",
                            nodes_year_1, nodes_year_2,
                            "[capy1|haz_incr:exp_incr:cap_incr:",
                            parents_year_1, "]",
                            "[capy2|haz_incr:exp_incr:cap_incr:capy1:",
                            parents_year_2, "]"))
# learn parameters
bn_model <- bn.fit(dag, data = capital)

# plot bn
plotbn <- function(dag, cyclone_y1, cyclone_y2){
  
  if (length(cyclone_y1) == length(cycl_year_1) & 
      length(cyclone_y2) == length(cycl_year_2)){ # all cyclones
    
    graphviz.plot(dag,
                  groups = list(cycl_year_1, cycl_year_2,
                                c("cap_incr", "exp_incr", "haz_incr")),
                  layout = "dot")
    
  } else {
    not_c1 <- cycl_year_1[!(cycl_year_1 %in% cyclone_y1)]
    not_c2 <- cycl_year_2[!(cycl_year_2 %in% cyclone_y2)]
    
    hlight <- list(nodes = c(not_c1, not_c2),
                   arcs = matrix(c(not_c1, not_c2,
                                   rep("capy1", length(not_c1)),
                                   rep("capy2", length(not_c2))),
                                 ncol = 2),
                   col = "grey",
                   textCol = "grey")
    
    graphviz.plot(dag,
                  highlight = hlight,
                  groups = list(cycl_year_1, cycl_year_2,
                                c("cap_incr", "exp_incr", "haz_incr")),
                  layout = "dot")
    
  }
}


# inputs to consider
low_increase <- array(c(0.4, 0.3, 0.2, 0.1, 0), dimnames = list(c(1, 3, 5, 7, 9)))
uniform_increase <- array(c(0.2, 0.2, 0.2, 0.2, 0.2), dimnames = list(c(1, 3, 5, 7, 9)))
high_increase <- array(c(0, 0.1, 0.2, 0.3, 0.4), dimnames = list(c(1, 3, 5, 7, 9)))
haz_dist <- list(low_increase, uniform_increase, high_increase)
haz_name <- c("low", "uniform", "high") # names for each pattern


cap_lab <- c("0", "30", "60", "90", "120", "150")
names(cap_lab) <- c("0% increase", "30% increase", "60% increase",
                    "90% increase", "120% increase", "150% increase")
haz_lab <- c("Low hazard increase more likely",
             "Uniform hazard increase",
             "High hazard increase more likely")
names(haz_lab) <- c("low", "uniform", "high")

# years
years <- c("capy1", "capy2")
names(years) <-  c(2017, 2018)

# ggplot
plotcap <- function(dist, n_each) {
  ggplot(data = dist, mapping = aes(x = val, y = stat(count / n_each))) +
    geom_histogram(data = subset(dist, val < 0), fill = "red", col="grey",
                   binwidth = 250, boundary = 0) +
    geom_histogram(data = subset(dist, val >= 0), fill = "blue", col="grey",
                   binwidth = 250, boundary = 0) +
    facet_grid(cols = vars(haz),
               labeller = labeller(haz = haz_lab)) +
    xlim(-1800, 1800) +
    xlab("Capital Level") + 
    ylab("Likelihood of Capital Availability") +
    ggtitle("Inference on Capital Levels") +
    theme(plot.title = element_text(hjust=0.5))
}



ui <- fluidPage(
  fluidRow(
    column(3,
           checkboxGroupInput("cyclone1", "Cyclones of first year", 
                              choices = cycl_year_1),
           checkboxGroupInput("cyclone2", "Cyclones of second year", 
                              choices = cycl_year_2)
           ),
    column(12, plotOutput("bn")),
    column(5,
           selectInput("year", "Year under consideration", years),
           radioButtons("policy", "Capital increase in fund (policy)", 
                        choices = cap_lab),
           # radioButtons("gdp", "GDP growth (estimate)", c(1,3,5,7,9)),
           # selectInput("hazard", "How does hazard increase? (climate impact)", 
           #             c("Lower increase more frequent", 
           #               "Uniform increase", 
           #               "Higher increase more frequent"))
           ),
    column(12, plotOutput("figure")),
  )
)

server <- function(input, output, session) {
  
  # bn part
  output$bn <- renderPlot({
    plotbn(dag, input$cyclone1, input$cyclone2)
  }, res = 96)
  
  # policy figures
  policy <- reactive({input$policy})
  output$figure <- renderPlot({
    
    dist_all <- data.frame(val = NULL, haz = NULL)
    
    for (haz in 1:3){
      
      # change prior
      bn_model$haz_incr <- haz_dist[[haz]]
      
      # realized capital value
      evidence <- as.list(as.character(as.numeric(c(cycl_year_1 %in% input$cyclone1, cycl_year_2 %in% input$cyclone2))))
      names(evidence) <- c(cycl_year_1, cycl_year_2)
      evidence["cap_incr"] <- input$policy
      val <- cpdist(bn_model, nodes = input$year,
                    evidence = evidence,
                    method = "lw")[[input$year]]
      # add to dataframe
      dist_all <-
        rbind(dist_all,
              cbind(val = val,
                    haz = haz_name[haz]))
    }
    # fix data type
    dist_all$val <- as.numeric(dist_all$val)
    dist_all$haz <- factor(dist_all$haz,
                           levels = c("low", "uniform", "high"))

    # total number of simulations for each case
    n_each <- nrow(dist_all[dist_all$haz == "high", ])
    
    plotcap(dist_all, n_each)
  })
  
}


shinyApp(ui, server)
