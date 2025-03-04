#### Load data & packages ----

df <- read.csv (file = "processed data.csv", stringsAsFactors = TRUE)
library (ggplot2)
library (dplyr)
library (ggpubr)
library(plotly)

#### Statistic analysis ----
## Analysis the main effects and the interaction - multi - linear regression 

model_multi_lin <- lm (Sleep_night_duration_bb1 ~ CBTS_score * EPDS_score * HADS_score, data = df)

summary (model_multi_lin)

#Explain data in the word file: "Finale project - explain and answers"

#### Plots ----
##Main effects

# CBTS plot
scatter_plot_CBTS <- df |>
ggscatter (
           x = "CBTS_score",
           y = "Sleep_night_duration_bb1",
           color = "blue",
           title = "Relationship between CBTS score and Sleep Duration",
           xlab = "CBTS Score",
           ylab = "Sleep Night Duration",
           add = "reg.line",
           size = 1.5,
           cor.coef = TRUE,
           conf.int = TRUE,
           add.params = list (color = "red", fill = "grey"),
           cor.method = "pearson") +
  theme_minimal()
print (scatter_plot_CBTS)

# EPDS plot
scatter_plot_EPDS <- df |>
  ggscatter (
    x = "HADS_score",
    y = "Sleep_night_duration_bb1",
    color = "blue",
    title = "Relationship between HADS score and Sleep Duration",
    xlab = "HADS Score",
    ylab = "Sleep Night Duration",
    add = "reg.line",
    size = 1.5,
    cor.coef = TRUE,
    conf.int = TRUE,
    add.params = list (color = "red", fill = "grey"),
    cor.method = "pearson") +
  theme_minimal()
print (scatter_plot_EPDS)

# EPDS plot
scatter_plot_EPDS <- df |>
  ggscatter (
    x = "EPDS_score",
    y = "Sleep_night_duration_bb1",
    color = "blue",
    title = "Relationship between EPDS score and Sleep Duration",
    xlab = "EPDS Score",
    ylab = "Sleep Night Duration",
    add = "reg.line",
    size = 1.5,
    cor.coef = TRUE,
    conf.int = TRUE,
    add.params = list (color = "red", fill = "grey"),
    cor.method = "pearson") +
  theme_minimal()
print (scatter_plot_EPDS)

## Two-way interaction plots

# CBTS:EPDS
plot_CBTS_EPDS <- plot_ly(df, x = ~ CBTS_score, y = ~ EPDS_score, z = ~ Sleep_night_duration_bb1,
                          type = "scatter3d", mode = "markers",
                          marker = list(size = 5, color = ~Sleep_night_duration_bb1, colorscale = "Viridis")) |>
  layout(title = "3D Plot of CBTS, EPDS, and Sleep Duration",
         scene = list(xaxis = list(title = "CBTS Score"),
                      yaxis = list(title = "EPDS Score"),
                      zaxis = list(title = "Sleep Duration")))
print (plot_CBTS_EPDS)

# CBTS:HADS
plot_CBTS_HADS <- plot_ly(df, x = ~ CBTS_score, y = ~ HADS_score, z = ~ Sleep_night_duration_bb1,
        type = "scatter3d", mode = "markers",
        marker = list(size = 5, color = ~Sleep_night_duration_bb1, colorscale = "Viridis")) |>
  layout(title = "3D Plot of CBTS, HADS, and Sleep Duration",
         scene = list(xaxis = list(title = "CBTS Score"),
                      yaxis = list(title = "HADS Score"),
                      zaxis = list(title = "Sleep Duration")))
print (plot_CBTS_HADS)

# EPDS:HADS
plot_EPDS_HADS <- plot_ly(df, x = ~ EPDS_score, y = ~ HADS_score, z = ~ Sleep_night_duration_bb1,
                          type = "scatter3d", mode = "markers",
                          marker = list(size = 5, color = ~Sleep_night_duration_bb1, colorscale = "Viridis")) |>
  layout(title = "3D Plot of EPDS, HADS, and Sleep Duration",
         scene = list(xaxis = list(title = "EPDS Score"),
                      yaxis = list(title = "HADS Score"),
                      zaxis = list(title = "Sleep Duration")))
print (plot_EPDS_HADS)

## Three-way interaction plots

# CBTS:EPDS:HADS
df <- df |>
  mutate(CBTS_Tertile = cut(CBTS_score, 
                            breaks = quantile(CBTS_score, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 
                            labels = c("Low", "Medium", "High"),
                            include.lowest = TRUE))

interaction_plot <- df |>
  ggplot(aes(x = EPDS_score, y = Sleep_night_duration_bb1, color = HADS_score)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ CBTS_Tertile, scales = "free") +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Interaction of EPDS, HADS, and Sleep Duration Across CBTS Tertiles",
       x = "EPDS Score",
       y = "Sleep Night Duration",
       color = "HADS Score") +
  theme_minimal()

print(interaction_plot)

#### End of the script, continue in the script: Part D - Data analysis - log regression  ----