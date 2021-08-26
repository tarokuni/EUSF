# generating new data using bootstrap

# import data
capital <- read.csv("capital_under_combs.csv") # main data
comb <- read.csv("combinations.csv") # combinations

n_scenario <- sum(capital$comb_number==0) # number of scenarios
comb1 <- comb[rep(1:nrow(comb), each = n_scenario), ]
names(comb1)[1] <- "scenario"
capital <- cbind(capital, comb1)

# fix float (data is not exactly integer valued for some reason)
capital$haz_incr <- round(capital$haz_incr)
capital$exp_incr <- round(capital$exp_incr)
capital$cap_incr <- round(capital$cap_incr)

# cyclones
cycl_year_1 <- c("Carlos", "Enawo", "Harvey", "Irma", "Maria", "Ophelia")
cycl_year_2 <- c("Berguitta", "Fakir", "Isaac", "Helene", "Kirk", "Leslie", "Ava")
cycl <- c(cycl_year_1, cycl_year_2)

# create simpler data including cases for 1 cyclone
capital_single <- capital[rowSums(capital[cycl]) == 1,]          

# bootstrap simulation - using all 13 cyclones
sim_data <- data.frame(payout = NULL, n_cyc = NULL, cap = NULL, haz = NULL, exp = NULL)
N <- 10000 # bootstrap number

for (cap in c(0, 30, 60, 90, 120, 150)){ # capital increase: policy choice
  print(cap)
  for (n_cyc in 1:10){ # number of cyclones per year
    print(n_cyc)
    
    cyc <- sample(cycl_year_1, n_cyc * N, replace = TRUE) 
    haz <- sample(c(1,3,5,7,9), N, replace = TRUE,  # hazard level increase # modeling uncertainty
                  prob = c(0, 0.1, 0.2, 0.3, 0.4)) # different hazard increase distributions
    exp <- sample(c(1,6,11,16,21), N, replace = TRUE) # GDP growth of region # homogeneous for a given realization
    for (i in 1:N){
      payout_temp <-  0
      for (j in 1:n_cyc) {
        
        if (cyc[(i-1) * n_cyc + j] %in% cycl_year_1){
          year = "payout1"
        } else {
          year = "payout2"
        }
        
        payout_temp <- payout_temp + capital_single[capital_single[cyc[(i-1) * n_cyc + j]] == 1 & 
                          capital_single$haz_incr == haz[i] &
                          capital_single$exp_incr == exp[i] &
                          capital_single$cap_incr == cap, 
                          year]           
      }
      sim_data <- rbind(sim_data, cbind(payout = payout_temp,
                                        n_cyc = n_cyc,
                                        cap = cap,
                                        haz = haz[i],
                                        exp = exp[i]))
    }
    
  }
}

write.csv(sim_data, "sim_data_high.csv")
