install.packages("quantmod")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("scales")
install.packages("viridis")

library(quantmod)  
library(dplyr) 
library(ggplot2) 
library(scales) 
library (viridis) 



# Baixar dados históricos de Bitcoin
getSymbols("BTC-USD", src = "yahoo", from = "2010-01-01")

# Extrair dados de preços de fechamento e volume
btc_data <- data.frame(Date = index(`BTC-USD`), coredata(`BTC-USD`)[, 4])
colnames(btc_data) <- c("Date", "Close")

# Função para calcular o Stock-to-Flow
calculate_s2f <- function(block_height, halving_interval = 210000, initial_reward = 50) {
  halvings <- floor(block_height / halving_interval)
  current_reward <- initial_reward / (2 ^ halvings)
  total_supply <- sum(initial_reward / (2 ^ (0:halvings)) * halving_interval)
  remaining_supply <- current_reward * (halving_interval - (block_height %% halving_interval))
  s2f <- total_supply / remaining_supply
  return(s2f)
}

# Calcular altura dos blocos e Stock-to-Flow para cada dia
btc_data <- btc_data %>%
  mutate(BlockHeight = cumsum(c(0, diff(as.numeric(Date))) * 144), # Aproximando 144 blocos por dia
         S2F = sapply(BlockHeight, calculate_s2f))

# Função para calcular os dias até o próximo halving
days_until_next_halving <- function(block_height, halving_interval = 210000) {
  next_halving_block = ((floor(block_height / halving_interval) + 1) * halving_interval)
  days_until = (next_halving_block - block_height) / 144 # Aproximando 144 blocos por dia
  return(days_until)
}

# Calcular os dias até o próximo halving
btc_data <- btc_data %>%
  mutate(DaysUntilHalving = sapply(BlockHeight, days_until_next_halving))

# Adicionar a data do próximo halving (aproximada)
halving_date_2024 <- as.Date("2024-04-01")

# Plotar Stock-to-Flow vs Preço do Bitcoin com coloração por dias até o próximo halving
ggplot(btc_data, aes(x = Date)) +
  geom_line(aes(y = S2F * 1000), color = "yellow") +
  geom_point(aes(y = Close, color = DaysUntilHalving), size = 1) +
  geom_vline(xintercept = as.numeric(halving_date_2024), linetype = "dashed", color = "red") + # Linha vertical para o halving de 2024
  annotate("text", x = halving_date_2024, y = 1000000, label = "Halving 2024", color = "red", angle = 90, vjust = -0.5) + # Texto indicando o halving
  scale_y_log10(
    name = "USD",
    sec.axis = sec_axis(~./1000, name = "Stock-to-Flow")
  ) +
  scale_color_gradientn(colors = viridis(10), name = "Days until next halving") +
  labs(title = "Stock-to-Flow Model for Bitcoin with Days Until Next Halving",
       x = "Date") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    plot.title = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    axis.line = element_line(color = "white"),
    axis.ticks = element_line(color = "white")
  )