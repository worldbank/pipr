library(dplyr)
library(ggplot2)
# Ideas
## Multidimensional Poverty Index vs Extreme Monetary Poverty ----
### Get SSA countries codes ----
countries <- get_countries()
SSA <- countries |>
  filter(region_code == "SSA") |>
  select(country_code)


### Get Country profiles ----
cp_data <- get_cp()
ssa_cp <- cp_data |>
  filter(country_code %in% SSA$country_code) |>
  select(country_code, year, contains('headcount'), contains('mpm')) |>
  filter(!is.na(mpm_headcount), year %in% c(2015, 2018, 2021)) |>
  select(country_code, year, headcount_ipl, mpm_headcount)


ssa_cp |>
  ggplot(aes(x = mpm_headcount, y = headcount_ipl, label = country_code)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_text(vjust = -0.5) +
  facet_wrap(~year)

## SPL vs IPL: country-level ----
country_chosen <- "ITA"

country_spl <- get_stats(country = country_chosen)  |>
  select(year, spl)


country_spl_data <- map2_dfr(.x = country_spl$spl,
                             .y = country_spl$year,
                             .f = ~ get_stats(country = country_chosen,
                                              povline = .x,
                                              year = .y)) |>
  select(country_code, year, poverty_line, headcount)

country_pl_data <- get_stats(country = country_chosen) |>
  select(year, poverty_line, headcount)

ggplot() +
  geom_point(data = country_spl_data, aes(x = year, y = headcount), color = "blue") +
  geom_line(data = country_spl_data, aes(x = year, y = headcount), color = "blue") +
  geom_point(data = country_pl_data, aes(x = year, y = headcount), color = "red") +
  geom_line(data = country_pl_data, aes(x = year, y = headcount), color = "red") +
  labs(title = paste("Headcount ratio SPL vs PL in", country_chosen),
       x = "Year",
       y = "Headcount ratio") +
  theme_minimal() +
  theme(legend.position = "bottom")

## SPL vs IPL: regional ----
ohic <- countries |>
  filter(region_code == "OHI") |>
  select(country_code, country_name)

ohic_spl <- get_stats(country = ohic$country_code)  |>
  filter(year %in% c(2018, 2021)) |>
  select(country_code, year, spl)


ohic_spl_data <- ohic_spl |>
  purrr::pmap(~ get_stats(country = ..1,
                   year = ..2,
                   povline = ..3)) |>
  bind_rows()


ohic_ipl_data <- ohic_spl |>
  purrr::pmap(~ get_stats(country = ..1,
                   year = ..2)) |>
  bind_rows() |>
  select(country_code, country_name, year, poverty_line, headcount)

ohic_spl_data |>
  select(country_code, country_name, year, spl = poverty_line,
         spl_headcount = headcount) |>
  left_join(ohic_ipl_data, by = c("country_code", "country_name", "year")) |>
  ggplot(aes(x = spl_headcount, y = headcount, label = country_code)) +
  geom_point() +
  geom_text(vjust = -0.5) +
  labs(title = "Headcount ratio SPL vs IPL for OHICs in 2021",
       x = "SPL Headcount",
       y = "IPL Headcount") +
  theme_minimal() +
  theme(legend.position = "bottom")


## Regional headcount ----
df <- get_wb() |>
  filter(year > 1995 & year < 2024) |>
  mutate(
    pop_in_poverty = round(pop_in_poverty / 1000000, 0),
    headcount = round(headcount, 3)
  )

regions <- df |>
  filter(!region_code %in% c("WLD", "AFE", "AFW"))

ggplot(regions, aes(y = pop_in_poverty, x = year, fill = region_name)) +
  geom_area() +
  scale_y_continuous(
    limits = c(0, 2000),
    breaks = c(0, 500, 1000, 1500, 2000)
  ) +
  scale_fill_tableau(palette = "Tableau 10") +
  labs(
    y = "Number of poor (million)",
    x = ""
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(title = ""))


## Poverty line sensitivity ----
country_chosen <- "NGA"
country_data <- get_stats(country = country_chosen) |>
  filter(year == 2018) |>
  select(year, poverty_line, headcount, poverty_gap, mean, median)

## poverty line sensitivity around the median
ipls <- seq(from = 2,
            to = 10,
            length.out = 10)

ipls_sensitivity_data <- purrr::map(.x = ipls,
                        .f = ~ get_stats(country = country_chosen,
                                         year = country_data$year,
                                         povline = .x)) |>
  bind_rows() |>
  select(year, poverty_line, headcount, poverty_gap, mean, median)



### revised ----
library(tidyverse)

# ---- Get base data for chosen country and year ----
country_chosen <- "NGA"
country_data <- get_stats(country = country_chosen) |>
  filter(year == max(year)) |>
  select(year, poverty_line, headcount, poverty_gap, poverty_severity,
         watts, mld, gini, polarization, mean, median)

# Define a sequence of poverty lines (ipls)
ipls <- seq(from = 2, to = 10, length.out = 10)

ipls_sensitivity_data <- purrr::map(.x = ipls,
                                    .f = ~ get_stats(country = country_chosen,
                                                     year = country_data$year,
                                                     povline = .x)) |>
  bind_rows() |>
  select(year, poverty_line, headcount, poverty_gap, poverty_severity,
         watts, mean, median)

# ---- Graph 1: Headcount and Poverty Gap ----
# For these indicators we convert to percentages.
graph1_data <- ipls_sensitivity_data |>
  pivot_longer(cols = c(headcount, poverty_gap),
               names_to = "Measure",
               values_to = "Value") |>
  mutate(
    # Convert proportions to percentages
    Value = Value * 100,
    Measure = recode(Measure,
                     "headcount" = "Headcount (%)",
                     "poverty_gap" = "Poverty Gap (%)")
  )

# Get maximum y-value (for annotation placement)
max_y1 <- max(graph1_data$Value, na.rm = TRUE)

graph1 <- ggplot(graph1_data, aes(x = poverty_line, y = Value, color = Measure)) +
  geom_line(size = 1) +
  # Add vertical lines for median and mean poverty lines
  geom_vline(xintercept = country_data$median, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = country_data$mean, linetype = "dashed", color = "gray60") +
  # Annotate these lines with text labels
  annotate("text",
           x = country_data$median,
           y = max_y1,
           label = paste("Median =", round(country_data$median, 2)),
           angle = 90, vjust = -0.5, hjust = 1, size = 3) +
  annotate("text",
           x = country_data$mean,
           y = max_y1,
           label = paste("Mean =", round(country_data$mean, 2)),
           angle = 90, vjust = -0.5, hjust = 1, size = 3) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = paste("Poverty Sensitivity Analysis for", country_chosen),
    subtitle = paste("Year:", country_data$year),
    x = "Poverty Line($/day)",
    y = "",
    color = ""
  ) +
  # Format y-axis as percentages (values are already multiplied by 100)
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold")
  )


## Country-level: Changes in inequality and income distribution shifts ----
df_gini <- get_stats() |>
  filter((year == 2019 | year == 2020) & reporting_level == "national") |> # Select years and national data
  group_by(country_code, welfare_type, comparable_spell) |> # Group by country and welfare type
  filter(n() == 2) |> # Keep only countries with data in both years
  arrange(country_code, year) |> # Ensure sorted order
  mutate(ginichange = gini - lag(gini),
         country_code_chr = as.character(country_code)) |> # Calculate change in Gini
  filter(year == 2020) |> # Keep only 2020 data for plotting
  group_by(country_code, region_name) |> # Find mean for countries with two survey sources
  summarize(ginichange = mean(ginichange, na.rm = TRUE))

gini_plot <- ggplot(df_gini,
                    aes(x = reorder(country_code, ginichange),
                        y = ginichange * 100, fill = region_name)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Change in Gini Index (2019–2020)",
    subtitle = "Change in inequality (Gini points) for countries with comparable data",
    x = "Country",
    y = "Change in Gini Points"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
  )+
  # remove legend title
  guides(fill = guide_legend(title = NULL))


gini_plot


country_chosen <- c("USA", "PER")
deciles <- get_stats() |>
  filter(country_code %in% country_chosen) |>
  select(country_code, year, poverty_line, mean, median, headcount,
         contains('decile'))


deciles_long <- deciles |>
  filter(year %in% c(2019, 2020)) |>
  select(-c(mean, median, headcount))|>
  group_by(country_code, year)|>
  tidyr::pivot_longer(cols = contains('decile'), names_to = "decile",
                      values_to = "welfare_share") |>
  mutate(decile = as.numeric(stringr::str_remove(decile, "decile")))|>
  mutate(cum_share = cumsum(welfare_share))

deciles_long <- bind_rows(deciles_long,
                          tidyr::expand_grid(country_code = unique(deciles_long$country_code),
                                      year = unique(deciles_long$year),
                                      decile = 0, welfare_share = 0, cum_share = 0))



deciles_long |>
  ggplot(aes(x = decile, y = cum_share,
             group = year, colour = as.factor(year))) +
  geom_point() +
  geom_line() +
  geom_abline(slope = 1/10, intercept = 0, linetype = "dashed") +
  labs(title = paste("Cumulative Welfare shares for", country_chosen[1], " and", country_chosen[2]),
       x = "Income Decile",
       y = "Welfare share") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~country_code)










