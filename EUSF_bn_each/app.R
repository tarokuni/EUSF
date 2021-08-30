library(shiny)
library(bnlearn)
library(gRain) # for exact inference
library(tidyverse) # ggplot
library(vroom) # for faster import of data
library(Rgraphviz)


# data 
capital <- vroom("capital_under_combs.csv") # main data # load a bit faster using vroom
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

# dag for display
dag_disp <- model2network(paste0("[haz_incr][exp_incr][cap_incr]",
                                 nodes_year_1, nodes_year_2,
                                 "[Year 1\nhazard\nlevel|haz_incr:", parents_year_1, "]",
                                 "[Payout\nYear 1|Year 1\nhazard\nlevel:exp_incr]",
                                 "[capy1|Payout\nYear 1:cap_incr]",
                                 "[Year 2\nhazard\nlevel|haz_incr:", parents_year_2, "]",
                                 "[Payout\nYear 2|Year 2\nhazard\nlevel:exp_incr]",
                                 "[capy2|Payout\nYear 2:cap_incr:capy1]"))





# plot bn
plotbn <- function(dag, cyclone_y1, cyclone_y2){
  
  newnames <- bnlearn::nodes(dag)
  newnames[newnames == "exp_incr"] <- "GDP\nincrease"
  newnames[newnames == "haz_incr"] <- "Hazard\nincrease"
  newnames[newnames == "cap_incr"] <- "Capital\nincrease"
  newnames[newnames == "capy1"] <- "Capital\nYear 1"
  newnames[newnames == "capy2"] <- "Capital\nYear 2"
  
  dag <- rename.nodes(dag, newnames)

  
  if (length(cyclone_y1) == length(cycl_year_1) & 
      length(cyclone_y2) == length(cycl_year_2)){ # all cyclones
    
    g <- graphviz.plot(dag,
                      groups = list(cycl_year_1, cycl_year_2,
                                    c("GDP\nincrease", "Hazard\nincrease", "Capital\nincrease"),
                                    c("Year 1\nhazard\nlevel", "Capital\nYear 1", "Payout\nYear 1"),
                                    c("Year 2\nhazard\nlevel", "Capital\nYear 2", "Payout\nYear 2")),
                      layout = "dot",
                      render = FALSE)
    
  } else {
    not_c1 <- cycl_year_1[!(cycl_year_1 %in% cyclone_y1)]
    not_c2 <- cycl_year_2[!(cycl_year_2 %in% cyclone_y2)]
    
    hlight <- list(nodes = c(not_c1, not_c2),
                   arcs = matrix(c(not_c1, not_c2,
                                   rep("Capital\nYear 1", length(not_c1)),
                                   rep("Capital\nYear 2", length(not_c2))),
                                 ncol = 2),
                   col = "grey",
                   textCol = "grey")
    
    g <- graphviz.plot(dag,
                  highlight = hlight,
                  groups = list(cycl_year_1, cycl_year_2,
                                c("GDP\nincrease", "Hazard\nincrease", "Capital\nincrease"),
                                c("Year 1\nhazard\nlevel", "Capital\nYear 1", "Payout\nYear 1"),
                                c("Year 2\nhazard\nlevel", "Capital\nYear 2", "Payout\nYear 2")),
                  layout = "dot",
                  render = FALSE)
    
  }

  nodeRenderInfo(g) <- list(shape = list("Capital\nincrease" = "rectangle",
                                         "GDP\nincrease" = "rectangle",
                                         "Hazard\nincrease" = "rectangle"))
  # edgeRenderInfo(g) <- list(lwd = 2)
  renderGraph(g)    
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
  
  if (min(dist$val) > 1800) {
    lim <- 3000
  } else {
    lim <- 1800
  }
  
  ggplot(data = dist, mapping = aes(x = val, y = stat(count / n_each))) +
    geom_histogram(data = subset(dist, val < 0), fill = "red", col="grey",
                   boundary = 0) +
    geom_histogram(data = subset(dist, val >= 0), fill = "blue", col="grey", 
                   boundary = 0) +
    facet_grid(cols = vars(haz),
               labeller = labeller(haz = haz_lab)) +
    xlim(-lim, lim) +
    xlab("Capital Level (Million EUR)") + 
    ylab("Likelihood") +
    theme(plot.title = element_text(hjust=0.5))
}



ui <- fluidPage(
  
  titlePanel("Storyline approach to the effects of tropical cyclones on the EUSF"),
  
  h4("Specify which cyclones to consider in the storyline:"),
  
  sidebarLayout(
    sidebarPanel(
           checkboxGroupInput("cyclone1", "Cyclones in first year", 
                              choices = cycl_year_1),
           checkboxGroupInput("cyclone2", "Cyclones in second year", 
                              choices = cycl_year_2)
           ),
    mainPanel( plotOutput("bn"))
  ),
  tabsetPanel(
    tabPanel("Outcomes of specific policy options",
      sidebarLayout(
        sidebarPanel(
               selectInput("year", "Year under consideration", years),
               radioButtons("policy", "Capital increase in fund (policy)", 
                            choices = cap_lab),
               ),
        mainPanel(
          h4("Will capital levels be positive under the chosen capital increase?"),
          plotOutput("figure")
          )
      ),
    ),
    tabPanel("What policy to take to achive positive capital value",
      sidebarLayout(
        sidebarPanel(
          selectInput("year_p", "Year under consideration", years),
          radioButtons("hazard_p", "Hazard increase pattern", 
                       choices = names(haz_lab)),
          h5("(Values are given in 30% margins up to 150%.)")
        ),
        mainPanel(
          h4("Policy needed to retain capital positive with a certain probability:"),
          tableOutput("table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # bn part
  output$bn <- renderPlot({
    plotbn(dag_disp, input$cyclone1, input$cyclone2)
  }, res = 96)
  
  
  # 1st tab: policy figures
  policy <- reactive({input$policy})
  output$figure <- renderPlot({
    
    dist_all <- data.frame(val = NULL, haz = NULL)
    
    for (haz in 1:3){
      
      # change prior
      bn_model$haz_incr <- haz_dist[[haz]]
      
      # realized capital value
      evidence <- as.list(as.character(as.numeric(c(cycl_year_1 %in% input$cyclone1, 
                                                    cycl_year_2 %in% input$cyclone2))))
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
  
  
  # 2nd tab: Which policy to take
  output$table <- renderTable({
    evidence <- as.list(as.character(as.numeric(c(cycl_year_1 %in% input$cyclone1, 
                                                  cycl_year_2 %in% input$cyclone2))))
    names(evidence) <- c(cycl_year_1, cycl_year_2)
    
    bn_model$haz_incr <- haz_dist[haz_name== input$hazard_p][[1]]
    
    result <- data.frame(NA, NA, NA, NA, NA)
    
    for (cap in cap_lab) {
      
      evidence["cap_incr"] <- cap
      
      val <- cpdist(bn_model, nodes = input$year_p,
                    evidence = evidence,
                    method = "lw")[[input$year_p]]
      
      prop <- sum(val >= 0) / length(val)
      
      if (prop >= 0.1 & is.na(result[1,1])) {
        result[1, 1] <- paste0(cap, "% increase needed")
      } 
      if (prop >= 0.33 & is.na(result[1,2])) {
        result[1, 2] <-  paste0(cap, "% increase needed")  
      } 
      if (prop >= 0.5 & is.na(result[1,3])) {
        result[1, 3] <-  paste0(cap, "% increase needed")
      } 
      if (prop >= 0.67 & is.na(result[1,4])) {
        result[1, 4] <-  paste0(cap, "% increase needed")
      } 
      if (prop >= 0.9 & is.na(result[1,5])) {
        result[1, 5] <-  paste0(cap, "% increase needed") 
      }
      
    }
    
    names(result) <- c("10%", "33%", "50%", "67%", "90%")
    
    result
    }, colnames = TRUE)
  
}


shinyApp(ui, server)
