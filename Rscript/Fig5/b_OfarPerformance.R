rm(list=ls())
library(readxl)
library(ggplot2)
library(ggpubr)
library(scales)
library(tidyr)
library(ggplot2)

# 设置CNS风格的主题
theme_cns <- theme(
  panel.background = element_rect(fill = "transparent"),
  # panel.grid.major = element_blank(),
  # panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color = "grey90", size = 0.5),
  panel.grid.minor = element_line(color = "grey95", size = 0.25),
  # axis.line = element_line(color = "black", size = 0.4),
  # axis.ticks = element_line(color = "black", size = 0.4),
  axis.text.x = element_text(color = "black", size = 7, face = "plain", 
                             hjust = 1), # angle = 30, 
  axis.text.y = element_text(color = "black", size = 7, face = "plain"),
  axis.title = element_text(color = "black", size = 7, face = "plain"),
  
  legend.position = "right",
  legend.title = element_blank(),
  legend.text = element_text(size = 7, face = "plain"),
  # legend.background = element_rect(fill = "white", color = NA),
  legend.key = element_rect(fill = "white", color = NA),
  
  strip.text = element_text(size = 7, face = "plain"),
  strip.background = element_blank(),
  
  plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
  plot.title = element_text(size = 10, face = 'plain', hjust = 0.5)
)

theme_backup <- theme(
  text = element_text(size = 12, face = "plain"),  # 统一字体大小，不加粗
  axis.title = element_text(size = 12, face = "plain"),
  axis.text = element_text(size = 12, face = "plain"),
  legend.title = element_text(size = 12, face = "plain"),
  legend.text = element_text(size = 12, face = "plain"),
  strip.text = element_text(size = 12, face = "plain"),
  strip.background = element_blank(),
  panel.grid.major = element_line(color = "grey90", size = 0.5),
  panel.grid.minor = element_line(color = "grey95", size = 0.25)
)


# load data
path <- './data/Fig5/g_Ofar.xlsx' 
sheet.name <- 'SBR'
rawdata <- data.frame(read_excel(path, sheet = sheet.name))
head(rawdata)

df <- rawdata[, c("Time", "NO3", "NO2", "NH4", "HCO3")]
df_long <- pivot_longer(df, cols = -Time, names_to = "Indicator", values_to = "Concentration")

scale_factor <- max(df$HCO3, na.rm = TRUE) / max(df$NO3, df$NO2, df$NH4, na.rm = TRUE)

df_long$Concentration_plot <- ifelse(df_long$Indicator == "HCO3",
       df_long$Concentration / scale_factor,
       df_long$Concentration)

custom_colors <- c(
  "NO3" = "#E1D7B7",
  "NO2" = "#7C93C3",
  "NH4" = "#1E2A5E",
  "HCO3" = "#FF204E"
)

ggplot() +
  geom_col(
  data = subset(df_long, Indicator == "HCO3"),
  aes(x = Time, y = Concentration_plot, fill = Indicator),
  width = 0.9,
  alpha = 0.3
  ) +
  geom_point(
  data = subset(df_long, Indicator != "HCO3"),
  aes(x = Time, y = Concentration_plot, color = Indicator),
  size = 2, alpha = 0.7
  ) +
  geom_line(
  data = subset(df_long, Indicator != "HCO3"),
  aes(x = Time, y = Concentration_plot, color = Indicator),
  size = 0.7
  ) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  scale_y_continuous(
  name = "N (mg L-1)",
  sec.axis = sec_axis(~ . * scale_factor, name = "C (mg L-1)")
  ) +
  labs(x = "Time (day)", color = "Indicator", fill = "Indicator") +

  theme_classic() +
  theme_cns
  # theme(
  #   plot.title = element_text(hjust = 0.5,size = 16, face = 'bold'),
  #   axis.line = element_line(size = 0.5),
  #   axis.text = element_text(size = 12, color = "black"),
  #   axis.title = element_text(size = 14),
  #   legend.title = element_blank(),
  #   legend.text = element_text(size = 12),
  #   plot.background = element_rect(fill = 'transparent', color = NA),
  #   panel.background = element_rect(fill = 'transparent'),
  #   panel.grid.major = element_blank(),
  #   panel.grid.minor = element_blank(),
  #   plot.margin = margin(t = 0.2, r = 1, b = 0.2, l = 0.5, unit = "cm"),
  #   axis.ticks.length = unit(0.1, "cm"),
  #   legend.background = element_blank(),
  #   legend.position = c(0.91, 0.75)
  # ) 
