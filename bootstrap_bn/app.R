library(shiny)
library(bnlearn)
library(gRain) # for exact inference
library(tidyverse) # ggplot

# data 
data_uni <- read.csv("sim_data.csv") # main data
data_high <- read.csv("sim_data_high.csv")[, 2:6] # main data
data_low <- read.csv("sim_data_low.csv")[, 2:6] # main data

# fixing data
data_uni$n_cyc <- as.factor(data_uni$n_cyc) 
data_uni$cap <- as.factor(data_uni$cap) 
data_uni$haz <- "uniform"

data_low$n_cyc <- as.factor(data_low$n_cyc) 
data_low$cap <- as.factor(data_low$cap) 
data_low$haz <- as.factor(data_low$haz)
data_low$exp <- as.numeric(data_low$exp) 
data_low$haz <- "low"

data_high$n_cyc <- as.factor(data_high$n_cyc) 
data_high$cap <- as.factor(data_high$cap) 
data_high$haz <- as.factor(data_high$haz)
data_high$exp <- as.numeric(data_high$exp) 
data_high$haz <- "high"

data_all <- rbind(data_uni, data_low[, 1:4], data_high[, 1:4])

# create DAG

dag <- model2network("[n_cyc][cap][payout|n_cyc:cap]")


# plot histogram
plothist <- function(n_cyc, haz) {
  if (haz == "low"){
    data = data_low
  } else if (haz == "uniform"){
    data = data_uni
  } else {
    data = data_high
  }
  
  ggplot(data = data[data$n_cyc == n_cyc, ], 
         mapping = aes(x = payout, y = stat(count / sum(count)))) +
    geom_histogram(col = "grey") +
    xlab("Payout value") + 
    ylab("Likelihood") +
    ggtitle("Distribution of Payouts per year") +
    theme(plot.title = element_text(hjust=0.5))
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
haz_lab <-  c("low", "uniform", "high")
names(haz_lab) <-c("Low hazard increase more likely",
                   "Uniform hazard increase",
                   "High hazard increase more likely")

# ggplot
plotcap <- function(dist, n_each) {
  
  ggplot(data = dist, mapping = aes(x = capy, y = stat(count / n_each))) +
    geom_histogram(data = subset(dist, capy < 0), fill = "red", col="grey",
                   binwidth = 250, boundary = 0) +
    geom_histogram(data = subset(dist, capy >= 0), fill = "blue", col="grey",
                   binwidth = 250, boundary = 0) +
    facet_grid(cols = vars(haz)) +
    xlim(-1800, 1800) +
    xlab("Capital Level") + 
    ylab("Likelihood") +
    ggtitle("Will Capital Levels stay positive?") +
    theme(plot.title = element_text(hjust=0.5))
}



ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("n_cyc", "Number of cyclones per year", 1:10, selected = 6),
      selectInput("haz", "Hazard increase pattern", haz_lab, selected = "uniform")
           ),
    mainPanel( plotOutput("histogram"))
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("n_y1", "Number of cyclones in year 1", 1:10, selected = 6),
      selectInput("n_y2", "Number of cyclones in year 2", 1:10, selected = 6),
      selectInput("year", "Year of consideration", c("Year 1", "Year 2")),
      radioButtons("policy", "Capital increase in fund (policy)", 
                   choices = cap_lab),
           ),
    mainPanel( plotOutput("figure"))
  )
)

server <- function(input, output, session) {
  
  # histogram
  output$histogram <- renderPlot({
    plothist(input$n_cyc, input$haz)
  }, res = 96)
  
  
  # policy figures
  output$figure <- renderPlot({

    dist_all <- data.frame(capy = NULL, n_y1 = NULL, n_y2 = NULL, haz = NULL)    

      for (haz in haz_name){
      
        bn_model <- bn.fit(dag, data = data_all[data_all$haz == haz, 
                                                c("payout", "n_cyc", "cap")])
        
        
        # realized capital value
        payout1 <- cpdist(bn_model, nodes = "payout",
                      evidence = list(n_cyc = input$n_y1,
                                      cap = input$policy),
                      method = "lw")$payout
        
        payout2 <- cpdist(bn_model, nodes = "payout",
                      evidence = list(n_cyc = input$n_y2,
                                      cap = input$policy),
                      method = "lw")$payout
        # capital increase numbers derived from data, not model
        
        capy1 <- -177.60 - payout1 + 11.26 * as.numeric(input$policy)
        capy2 <- capy1 + 280.70 - payout2 + 5.743 * as.numeric(input$policy)
        
        if (input$year == "Year 1"){
          capy <- capy1
        } else {
          capy <- capy2
        }
        
        dist_all <-
          rbind(dist_all,
                cbind(capy = as.numeric(capy), 
                      n_y1 = input$n_y1,
                      n_y2 = input$n_y2,
                      haz = haz))
        
        
        # fix data type
        dist_all$haz <- factor(dist_all$haz,
                               levels = c("low", "uniform", "high"))
    }
      # total number of simulations for each case
    
    dist_all$capy <- as.numeric(dist_all$capy)
    
    
    n_each <- nrow(dist_all[dist_all$haz == "high", ])
    plotcap(dist_all, n_each)
  }, res = 96)
  
}


shinyApp(ui, server)
