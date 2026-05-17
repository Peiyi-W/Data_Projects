library(ggplot2)
library(tidyr)
library(dplyr)
library(RColorBrewer)

# parameters
b <- 0.000373
d <- 0.000231
beta_i <- 0.1
beta_F <- 0.3
g <- 0.3
sigma <- 0.5



# t: week
t_vals <- seq(1,25, by=1)

# statement variables
S_popn <- rep(NA, times = length(t_vals))
I_popn <- rep(NA, times = length(t_vals))
F_popn <- rep(NA, times = length(t_vals))
R_popn <- rep(NA, times = length(t_vals))

# initial condition
S_popn[1] <- 9.99
I_popn[1] <- 0.01
F_popn[1] <- 0
R_popn[1] <- 0

# model
for (t in 2:length(t_vals)){
  S <- S_popn[t-1] - beta_i * S_popn[t-1]*I_popn[t-1] - 
    beta_F*S_popn[t-1]*F_popn[t-1] + b*S_popn[t-1] - d*S_popn[t-1]
  
  I <- I_popn[t-1] + beta_i * S_popn[t-1]*I_popn[t-1] + 
    beta_F*S_popn[t-1]*F_popn[t-1] - g*I_popn[t-1] + b*I_popn[t-1]
  
  F <- F_popn[t-1] + g*I_popn[t-1] - sigma*F_popn[t-1]
  
  R <- R_popn[t-1] + sigma*F_popn[t-1] + d*S_popn[t-1]
  
  # Set negative values to zero
  S_popn[t] <- max(S, 0)
  I_popn[t] <- max(I, 0)
  F_popn[t] <- max(F, 0)
  R_popn[t] <- max(R, 0)
}



df <- data.frame(
  Week = t_vals,
  Susceptible = S_popn,
  Infected = I_popn,
  Funeral = F_popn,
  Removed = R_popn
)

df_long <- pivot_longer(df, 
                        cols = -Week, 
                        names_to = "Statement_Variables", 
                        values_to = "Population")

ggplot(df_long, aes(x = Week, y = Population, color = Statement_Variables)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5) +
  scale_color_brewer(palette = "Dark2") + 
  labs(
    title = "Model Simulation",
    x = "Week",
    y = "Number of Population (Million)",
    color = "Population"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "gray"),
  )
