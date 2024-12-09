# Datt Data procedure ----
# Data is from Sarveskhana, Vol IX n 4 (April 1986), same dataset used by Datt (1998).
# Function to calculate cumulative shares (p and L)
calculate_cumulative_shares <- function(data, area) {
  # Step 1: Calculate cumulative population share (p)
  data$p <- cumsum(data$percentage_of_persons) / sum(data$percentage_of_persons)

  # Step 2: Calculate class consumption
  data$class_consumption <- data$mean_monthly_pc_exp * (data$percentage_of_persons / 100)

  # Step 3: Calculate cumulative welfare share (L)
  total_consumption <- sum(data$class_consumption)
  data$L <- cumsum(data$class_consumption) / total_consumption

  # Add area identifier
  data$area <- area

  # Return the data frame with p, L, and area columns
  return(data[, setdiff(names(data), "class_consumption")])
}

# Rural data setup
rural_data <- data.frame(
  monthly_pc_exp = c("0 – 30", "30 – 40", "40 – 50", "50 – 60", "60 – 75", "75 – 85", "85 – 100",
                     "100 – 125", "125 – 150", "150 – 200", "200 – 250", "250 – 300", "300 & above"),
  mean_monthly_pc_exp = c(24.4, 35.8, 45.36, 55.10, 64.92, 77.08, 91.75, 110.64, 134.9, 167.76, 215.48, 261.66, 384.97),
  percentage_of_persons = c(0.92, 2.47, 5.11, 7.90, 9.69, 15.24, 13.64, 16.99, 10.00, 9.78, 3.96, 1.81, 2.49)
)

# Urban data setup
urban_data <- data.frame(
  monthly_pc_exp = c("0 – 30", "30 – 40", "40 – 50", "50 – 60", "60 – 75", "75 – 85", "85 – 100",
                     "100 – 125", "125 – 150", "150 – 200", "200 – 250", "250 – 300", "300 & above"),
  mean_monthly_pc_exp = c(21.87, 35.8, 45.63, 55.46, 65.13, 77.2, 92.26, 111.41, 136.27, 170.13, 219.76, 267.33, 424.6),
  percentage_of_persons = c(0.21, 0.40, 1.01, 1.40, 3.92, 9.21, 10.64, 13.13, 11.17, 8.75, 9.16, 5.92, 9.32)
)

# Apply function to rural and urban data
datt_rural <- calculate_cumulative_shares(rural_data, "rural")
datt_urban <- calculate_cumulative_shares(urban_data, "urban")


# Usethis
usethis::use_data(datt_rural, datt_urban, overwrite = TRUE)
