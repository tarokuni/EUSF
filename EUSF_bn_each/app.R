# add repository; needed especially when deploying to shinyapps.io
# options(repos = BiocManager::repositories())

library(shiny, warn.conflicts = FALSE) # app
library(tidyverse, warn.conflicts = FALSE) # ggplot
library(bnlearn, warn.conflicts = FALSE) # bn
library(gRain, warn.conflicts = FALSE) # for exact inference
library(Rgraphviz, warn.conflicts = FALSE) # additional options for figures

# data 
capital <- readRDS("capital_under_combs.rds") # main data 
comb <- readRDS("combinations.rds")
# comb <- read.csv("combinations.csv", colClasses = "factor") # combinations

# combining the data
n_scenario <- sum(capital$comb_number==0) # number of scenarios
comb1 <- comb[rep(1:nrow(comb), each = n_scenario), ]
names(comb1)[1] <- "scenario"
capital <- cbind(capital, comb1)

# cyclones
cycl_year_1 <- c("Carlos", "Enawo", "Harvey", "Irma", "Maria", "Ophelia")
cycl_year_2 <- c("Berguitta", "Fakir", "Isaac", 
                 "Helene", "Kirk", "Leslie", "Ava")

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

rm(capital)


# dag for display -> this will be a causal network, not necessarily a bn
dag_disp <-
  model2network(paste0("[haz_incr][exp_incr][cap_incr]",
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
                                    c("GDP\nincrease", "Hazard\nincrease", 
                                      "Capital\nincrease"),
                                    c("Year 1\nhazard\nlevel", 
                                      "Capital\nYear 1", "Payout\nYear 1"),
                                    c("Year 2\nhazard\nlevel", 
                                      "Capital\nYear 2", "Payout\nYear 2")),
                      layout = "dot",
                      render = FALSE)
    
  } else {
    not_c1 <- cycl_year_1[!(cycl_year_1 %in% cyclone_y1)]
    not_c2 <- cycl_year_2[!(cycl_year_2 %in% cyclone_y2)]
    
    hlight <- list(nodes = c(not_c1, not_c2),
                   arcs = matrix(c(not_c1, not_c2,
                                   rep("Year 1\nhazard\nlevel", length(not_c1)),
                                   rep("Year 2\nhazard\nlevel", length(not_c2))),
                                 ncol = 2),
                   col = "grey",
                   textCol = "grey")
    
    g <- graphviz.plot(dag,
                  highlight = hlight,
                  groups = list(cycl_year_1, cycl_year_2,
                                c("GDP\nincrease", "Hazard\nincrease", 
                                  "Capital\nincrease"),
                                c("Year 1\nhazard\nlevel", "Capital\nYear 1", 
                                  "Payout\nYear 1"),
                                c("Year 2\nhazard\nlevel", "Capital\nYear 2", 
                                  "Payout\nYear 2")),
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
low_increase <- array(c(0.4, 0.3, 0.2, 0.1, 0), 
                      dimnames = list(c(1, 3, 5, 7, 9)))
uniform_increase <- array(c(0.2, 0.2, 0.2, 0.2, 0.2), 
                          dimnames = list(c(1, 3, 5, 7, 9)))
high_increase <- array(c(0, 0.1, 0.2, 0.3, 0.4), 
                       dimnames = list(c(1, 3, 5, 7, 9)))
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
plotcap <- function(dist, n_each, detail = FALSE) {
  
  if (min(dist$val) > 1800) {
    lim <- 3000
  } else {
    lim <- 1800
  }
  
  if (detail == TRUE) {
    ggplot(data = dist, mapping = aes(x = val, y = stat(count / n_each))) +
      geom_histogram(data = subset(dist, val < 0), fill = "red", col="grey",
                     boundary = 0) +
      geom_histogram(data = subset(dist, val >= 0), fill = "blue", col="grey", 
                     boundary = 0) +
      xlim(-lim, lim) +
      xlab("Capital Level (Million EUR)") + 
      ylab("Likelihood") +
      theme(plot.title = element_text(hjust=0.5))
  } else { 
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
}


#################
#################

ui <- fluidPage(
  
  titlePanel("Storyline approach to the effects of tropical cyclones on the EUSF"),
  
  h4(strong("Specify which cyclones to consider in the storyline:")),
  
  sidebarLayout(
    sidebarPanel(width = 4,
      fluidRow(
        column(6,
          h5(strong("Cyclones in first year - probability of each cyclone")),
          uiOutput("input_cyclones_1")
          ),
        column(6,
          h5(strong("Cyclones in first year - probability of each cyclone")),
          uiOutput("input_cyclones_2")
          )
      )
    ),
    mainPanel(width = 8,
      fluidRow(
        column(8,
          mainPanel(plotOutput("bn", width = "1000px", height = "500px"))
          )
      )
    )
  ),
  
  
  tabsetPanel(
    
    # first tab
    tabPanel("Outcomes of specific policy options",
      column(12,
        column(4,
        h5(strong("NB: "), "We use a flat prior probability distribution for GDP increase,
           and three different hazard increase probability distribution patterns.
           To specify the prior probability distributions, choose the next tab.")
        )
      ),
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
    
    # second tab
    tabPanel("Outcomes of specific policy options (specify prior distributions)",
      sidebarLayout(
        sidebarPanel(width = 6,
                     fluidRow(
                       column(12,
                              h5(strong("Set probability for each increase level in GDP")),
                       )
                     ),
                     fluidRow(column(12, uiOutput("gdp_warning"))),
                     fluidRow(
                       column(2,
                              numericInput("exp_incr_1", "1% increase", 
                                           value = 0.2, min = 0, max = 1, step = 0.01),
                       ),
                       column(2,
                              numericInput("exp_incr_6", "6% increase", 
                                           value = 0.2, min = 0, max = 1, step = 0.01),
                       ),
                       column(2,
                              numericInput("exp_incr_11", "11% increase", 
                                           value = 0.2, min = 0, max = 1, step = 0.01),
                       ),
                       column(2,
                              numericInput("exp_incr_16", "16% increase", 
                                           value = 0.2, min = 0, max = 1, step = 0.01),
                       ),
                       column(2,
                              numericInput("exp_incr_21", "21% increase", 
                                           value = 0.2, min = 0, max = 1, step = 0.01),
                       )
                     ),
                     fluidRow(
                       column(12,
                              h5(strong("Set probability for each increase level in hazard intensity")),
                       )
                     ),
                     fluidRow(column(12, uiOutput("hazard_warning"))),
                     fluidRow(
                       column(2,
                              numericInput("haz_incr_1", "1% increase", 
                                           value = 0.2, min = 0, max = 1, step = 0.01),
                       ),
                       column(2,
                              numericInput("haz_incr_3", "3% increase", 
                                           value = 0.2, min = 0, max = 1, step = 0.01),
                       ),
                       column(2,
                              numericInput("haz_incr_5", "5% increase", 
                                           value = 0.2, min = 0, max = 1, step = 0.01),
                       ),
                       column(2,
                              numericInput("haz_incr_7", "7% increase", 
                                           value = 0.2, min = 0, max = 1, step = 0.01),
                       ),
                       column(2,
                              numericInput("haz_incr_9", "9% increase", 
                                           value = 0.2, min = 0, max = 1, step = 0.01),
                       )
                     ),
                     fluidRow(
                       column(2,
                              selectInput("year_d", "Year under consideration", years)
                       ),
                       column(4, 
                              radioButtons("policy_d", "Capital increase in fund (policy)", 
                                    choices = cap_lab)
                       ),
                       column(4,
                              actionButton("click", "Show probability distribution",
                                           class="btn btn-primary")
                              )
                     )
        ),
        mainPanel(width = 6,
          h4("Will capital levels be positive under the chosen capital increase?"),
          plotOutput("figure_d")
        )
      )
    ),
    
    # 3rd tab
    tabPanel("What policy to take to achive positive capital value",
      sidebarLayout(
        sidebarPanel(width = 6,
                     fluidRow(
                       column(12,
                              h5(strong("Set probability for each increase level in GDP")),
                       )
                     ),
                     fluidRow(column(12, uiOutput("gdp_warning_p"))),
                     fluidRow(
                       column(2,
                              numericInput("exp_incr_1_p", "1% increase", 
                                           value = 0.2, min = 0, max = 1, step = 0.01),
                       ),
                       column(2,
                              numericInput("exp_incr_6_p", "6% increase", 
                                           value = 0.2, min = 0, max = 1, step = 0.01),
                       ),
                       column(2,
                              numericInput("exp_incr_11_p", "11% increase", 
                                           value = 0.2, min = 0, max = 1, step = 0.01),
                       ),
                       column(2,
                              numericInput("exp_incr_16_p", "16% increase", 
                                           value = 0.2, min = 0, max = 1, step = 0.01),
                       ),
                       column(2,
                              numericInput("exp_incr_21_p", "21% increase", 
                                           value = 0.2, min = 0, max = 1, step = 0.01),
                       )
                     ),
                     fluidRow(
                       column(12,
                              h5(strong("Set probability for each increase level in hazard intensity")),
                       )
                     ),
                     fluidRow(column(12, uiOutput("hazard_warning_p"))),
                     fluidRow(
                       column(2,
                              numericInput("haz_incr_1_p", "1% increase", 
                                           value = 0.2, min = 0, max = 1, step = 0.01),
                       ),
                       column(2,
                              numericInput("haz_incr_3_p", "3% increase", 
                                           value = 0.2, min = 0, max = 1, step = 0.01),
                       ),
                       column(2,
                              numericInput("haz_incr_5_p", "5% increase", 
                                           value = 0.2, min = 0, max = 1, step = 0.01),
                       ),
                       column(2,
                              numericInput("haz_incr_7_p", "7% increase", 
                                           value = 0.2, min = 0, max = 1, step = 0.01),
                       ),
                       column(2,
                              numericInput("haz_incr_9_p", "9% increase", 
                                           value = 0.2, min = 0, max = 1, step = 0.01),
                       )
                     ),
                     fluidRow(
                       column(3,
                              selectInput("year_p", "Year under consideration", years)
                       ),
                       column(4,
                              actionButton("click_p", "Show needed policy option",
                                           class="btn btn-primary")
                       )
                     ),
          h5("(Values are given in 30% margins up to 150%.)")
        ),
        mainPanel(width = 6,
          h4("Policy needed to retain capital positive with a certain probability:"),
          tableOutput("table")
        )
      )
    )
  )
)

checked_cyclones <- c(NULL)


#################
#################


server <- function(input, output, session) {
  
  # render input panel
  
  output$input_cyclones_1 <- 
    renderUI({
      L <- vector("list", 6)
      for (i in 1:6) {
        L[[i]] <- list(checkboxInput(cycl_year_1[i], cycl_year_1[i]),
                       conditionalPanel(condition = paste0("input.", cycl_year_1[i], " == true"),
                                        sliderInput(paste0(cycl_year_1[i], "_p"), 
                                                    paste0("Probability of ", cycl_year_1[i]), 
                                                    min = 0, max = 1, value = 1)
                                        )
                       )
      }
      return(L)
    })
  
  output$input_cyclones_2 <- 
    renderUI({
      L <- vector("list", 7)
      for (i in 1:7) {
        L[[i]] <- list(checkboxInput(cycl_year_2[i], cycl_year_2[i]),
                       conditionalPanel(condition = paste0("input.", cycl_year_2[i], " == true"),
                                        sliderInput(paste0(cycl_year_2[i], "_p"), 
                                                    paste0("Probability of ", cycl_year_2[i]), 
                                                    min = 0, max = 1, value = 1)
                                        )
                       )
      }
      return(L)
    })


  # Warning signs for probability input  
  output$gdp_warning <- 
    renderUI({
      exp_incr_all <- c(input$exp_incr_1, input$exp_incr_6, input$exp_incr_11, 
                        input$exp_incr_16, input$exp_incr_21)
      if (any(exp_incr_all < 0)){
        return(h4("Probabilities must be non-negative!", style="color:red;"))
      } else if (sum(exp_incr_all) != 1) {
        return(h4("Probabilities must add up to 1!", style="color:red;"))
      }
    })
  
  output$hazard_warning <- 
    renderUI({
      haz_incr_all <- c(input$haz_incr_1, input$haz_incr_3, input$haz_incr_5, 
                        input$haz_incr_7, input$haz_incr_9)
      if (any(haz_incr_all < 0)){
        return(h4("Probabilities must be non-negative!", style="color:red;"))
      } else if (sum(haz_incr_all) != 1) {
        return(h4("Probabilities must add up to 1!", style="color:red;"))
      }
    })
  
  output$gdp_warning_p <- 
    renderUI({
      exp_incr_all_p <- c(input$exp_incr_1_p, input$exp_incr_6_p, input$exp_incr_11_p, 
                        input$exp_incr_16_p, input$exp_incr_21_p)
      if (any(exp_incr_all_p < 0)){
        return(h4("Probabilities must be non-negative!", style="color:red;"))
      } else if (sum(exp_incr_all_p) != 1) {
        return(h4("Probabilities must add up to 1!", style="color:red;"))
      }
    })
  
  output$hazard_warning_p <- 
    renderUI({
      haz_incr_all_p <- c(input$haz_incr_1_p, input$haz_incr_3_p, input$haz_incr_5_p, 
                          input$haz_incr_7_p, input$haz_incr_9_p)
      if (any(haz_incr_all_p < 0)){
        return(h4("Probabilities must be non-negative!", style="color:red;"))
      } else if (sum(haz_incr_all_p) != 1) {
        return(h4("Probabilities must add up to 1!", style="color:red;"))
      }
    })
  
  
  # bn part
  output$bn <- renderPlot({
    cycl1 <- c(NULL)
    cycl2 <- c(NULL)
    for (i in 1:6){
      req(length(input[[cycl_year_1[i]]]) > 0)
      if (input[[cycl_year_1[i]]] == TRUE){
        cycl1 <- append(cycl1, cycl_year_1[i])
      }
    }
    for (i in 1:7){
      req(length(input[[cycl_year_2[i]]]) > 0)
      if (input[[cycl_year_2[i]]] == TRUE){
        cycl2 <- append(cycl2, cycl_year_2[i])
      }
    }
    
    plotbn(dag_disp, cycl1, cycl2)
  }, res = 196)
  
  
  # 1st tab: policy figures
  output$figure <- renderPlot({
    
    dist_all <- data.frame(val = NULL, haz = NULL)
    
    for (haz in 1:3){
      
      # change prior
      bn_model$haz_incr <- haz_dist[[haz]]
      for (cyc in c(cycl_year_1, cycl_year_2)) {
        req(length(input[[cyc]]) > 0)
        if (input[[cyc]] == TRUE) {
          bn_model[[cyc]] <- array(c(1 - input[[paste0(cyc, "_p")]],input[[paste0(cyc, "_p")]]),
                                   dimnames = list(c("0", "1")))
        }
      }
      
      # (un)realized cyclones and capital value input
      evidence <- vector(mode = "list")
      for (cyc in c(cycl_year_1, cycl_year_2)) {
        if (input[[cyc]] == FALSE) {
          evidence[[cyc]] <- "0"
        }
      }
      evidence["cap_incr"] <- input$policy
      
      # Monte Carlo
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
  
  
  
  
  # 2nd tab: policy figures with priors specified
    
  draw_figure <- eventReactive(input$click, {
      
    dist_all <- data.frame(val = NULL)
    
    # change prior
    bn_model$exp_incr <- array(c(input$exp_incr_1, input$exp_incr_6, input$exp_incr_11, 
                                 input$exp_incr_16, input$exp_incr_21),
                               dimnames = list(c("1", "6", "11", "16", "21")))
    bn_model$haz_incr <- array(c(input$haz_incr_1, input$haz_incr_3, input$haz_incr_5, 
                                 input$haz_incr_7, input$haz_incr_9),
                               dimnames = list(c("1", "3", "5", "7", "9")))
    
    for (cyc in c(cycl_year_1, cycl_year_2)) {
      if (input[[cyc]] == TRUE) {
        bn_model[[cyc]] <- array(c(1 - input[[paste0(cyc, "_p")]],input[[paste0(cyc, "_p")]]),
                                 dimnames = list(c("0", "1")))
      }
    }
    
    # (un)realized cyclones and capital value input
    evidence <- vector(mode = "list")
    for (cyc in c(cycl_year_1, cycl_year_2)) {
      if (input[[cyc]] == FALSE) {
        evidence[[cyc]] <- "0"
      }
    }
    evidence["cap_incr"] <- input$policy_d
    
    # MonteCarlo
    val <- cpdist(bn_model, nodes = input$year_d,
                  evidence = evidence,
                  method = "lw")[[input$year_d]]
    # add to dataframe
    dist_all <-
      rbind(dist_all,
            cbind(val = val))
    
    # fix data type
    dist_all$val <- as.numeric(dist_all$val)
    
    # total number of simulations for each case
    n_each <- nrow(dist_all)
    
    plotcap(dist_all, n_each, detail = TRUE)
  })
  
  output$figure_d <-  renderPlot(draw_figure())
  
  
  # 3rd tab: Which policy to take
  draw_table <- eventReactive(input$click_p, {
    
    # change prior
    bn_model$exp_incr <- array(c(input$exp_incr_1_p, input$exp_incr_6_p, input$exp_incr_11_p, 
                                 input$exp_incr_16_p, input$exp_incr_21_p),
                               dimnames = list(c("1", "6", "11", "16", "21")))
    bn_model$haz_incr <- array(c(input$haz_incr_1_p, input$haz_incr_3_p, input$haz_incr_5_p, 
                                 input$haz_incr_7_p, input$haz_incr_9_p),
                               dimnames = list(c("1", "3", "5", "7", "9")))
    
    for (cyc in c(cycl_year_1, cycl_year_2)) {
      if (input[[cyc]] == TRUE) {
        bn_model[[cyc]] <- array(c(1 - input[[paste0(cyc, "_p")]],input[[paste0(cyc, "_p")]]),
                                 dimnames = list(c("0", "1")))
      }
    }
    
    
    result <- data.frame(NA, NA, NA, NA, NA)
    evidence <- vector(mode = "list")
    
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
  })
  
  
  output$table <- renderTable(draw_table(), colnames = TRUE)
  
}


shinyApp(ui, server)
