# ================================= NOTES =================================
# This script analyzes grid search results and visualizes F1-score
# performance across different model parameters:
#
# - Buffer
# - RemoveNA
# - InputLayer
# - Epochs
#
# Output: Faceted barplot with mean F1-score + SD for each parameter value.
#         /docs/grid_search/f1_score_comparison.png
#
# ============================== Set up ===================================
library(envimaR)
library(tidyverse)

rootDir <- paste0(getwd(), "/project_folder")

rootDir <- envimaR::alternativeEnvi(
  root_folder = rootDir,
  alt_env_id = "COMPUTERNAME",
  alt_env_value = "PCRZP",
  alt_env_root_folder = "F:/BEN/edu"
)

source(file.path(rootDir, "src", "00_geoAI_setup.R"))

# ============================== Parameters ===============================
params <- c("Buffer", "RemoveNA", "InputLayer", "Epochs")

# =========================== Load data ===================================
results_file <- file.path(envrmt$path_grid_search, "experiment_results.csv")
df <- read.csv(results_file)

# ============================== Reshape ==================================
df_long <- df %>%
  pivot_longer(
    cols = all_of(params),
    names_to = "Parameter",
    values_to = "Value",
    values_transform = list(Value = as.character)
  )

# ============================= Aggregate ================================
summary_df <- df_long %>%
  group_by(Parameter, Value) %>%
  summarise(
    Mean_F1 = mean(ValF1Score),
    SD_F1 = sd(ValF1Score),
    Count = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Mean_F1 = round(Mean_F1, 4),
    Parameter = factor(Parameter, levels = params)
  ) %>%
  arrange(Parameter, Value) %>%
  as.data.frame()

# ============================== Labels ==================================
label_df <- tibble(
  Parameter = factor(params, levels = params),
  label = LETTERS[1:4],
  x = -Inf,
  y = Inf
)

# =============================== Plot ===================================
p <- ggplot(summary_df, aes(x = Value, y = Mean_F1)) +
  
  geom_col(fill = "grey80", color = "black", width = 0.6) +
  
  geom_errorbar(
    aes(ymin = Mean_F1 - SD_F1, ymax = Mean_F1 + SD_F1),
    width = 0.2
  ) +
  
  geom_text(
    aes(label = Value, y = Mean_F1 * 0.85),
    size = 3,
    angle = 90
  ) +
  
  geom_label(
    aes(label = round(Mean_F1, 3), y = Mean_F1 + 0.01),
    size = 2.5,
    vjust = 1.3,
    fill = "white",
    label.size = 0.2
  ) +
  
  geom_text(
    data = label_df,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 5,
    hjust = -0.3,
    vjust = 1.2
  ) +
  
  facet_wrap(~Parameter, scales = "free_x") +
  
  labs(
    x = NULL,
    y = "F1 Score"
  ) +
  
  coord_cartesian(ylim = c(0.5, 0.70)) +
  
  scale_y_continuous(breaks = seq(0.5, 0.70, 0.1)) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

plot(p)

# ============================== Export ==================================
ggsave(
  file.path(envrmt$path_grid_search, "f1_score_comparison.png"),
  p,
  width = 6,
  height = 5,
  dpi = 300
)