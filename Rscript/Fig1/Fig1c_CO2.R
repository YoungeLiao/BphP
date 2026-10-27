rm(list=ls())
# load library
library(readxl)
library(ggplot2)
library(ggpubr)

# load data
path <- './data/Fig1/Fig1c_CO2.xlsx' # Fig1a.CO2 # Fig1a.co2.Bicarbonate
sheet.name <- 'CO2' # 'CO2' # Biomass
rawdata <- data.frame(read_excel(path, sheet = sheet.name))

data <- rawdata[is.finite(rawdata$Bicarbonate), ]

# 设置CNS风格的主题
theme_cns <- theme(
  panel.background = element_rect(fill = "transparent"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(color = "black"),
  axis.text.x = element_text(color = "black", size = 7, face = "plain", 
                             hjust = 1), # angle = 30, 
  axis.text.y = element_text(color = "black", size = 7, face = "plain"),
  axis.title = element_text(color = "black", size = 7, face = "plain"),
  legend.position = "right",
  legend.title = element_blank(),
  legend.text = element_text(size = 7, face = "plain"),
  legend.background = element_rect(fill = "white", color = NA),
  legend.key = element_rect(fill = "white", color = NA),
  plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
  plot.title = element_text(size = 14, face = 'plain', hjust = 0.5)
)

# Create the scatter plot with linear trend and confidence interval
ggplot(data, aes(x = Time, y = Bicarbonate, color = Group, fill = Group, shape = Group)) +  # Add 'shape' aestheBicarbonate
  geom_point(size = 2) +  # Adjust point size
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, linetype = "dashed", size = 0.7) +  # Polynomial regression (second-degree)
  scale_shape_manual(values = c("Dark" = 16, "IR" = 17)) +  # Custom shapes for groups
  scale_fill_manual(values = c("Dark" = "#4E79A7",  "IR" = "#E15759")) +  # Custom fill colors
  scale_color_manual(values = c("Dark" = "#4E79A7",  "IR" = "#E15759")) +  # Custom color for trend lines
  labs(title = "",
       x = "Time (h)",
       y = "CO2 equivalent (mg C L-1)",
       color = "Group",
       fill = "Group",
       shape = "Group") +  # Add shape legend
  theme_cns
  # theme_classic() +
  # theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
  #       plot.background = element_rect(fill = 'transparent', color = NA),
  #       panel.background = element_rect(fill = 'transparent'),
  #       panel.grid = element_blank(),
  #       axis.title = element_text(size = 16),
  #       axis.text = element_text(size = 16),
  #       # axis.Bicarbonateks = element_line(linewidth = 1),
  #       legend.position = c(0.21, 0.12),
  #       legend.title = element_blank(),
  #       legend.text = element_text(size = 16),
  #       legend.background = element_blank(),
  #       legend.key.width = unit(2, "cm"),  # Make legend lines longer
  #       legend.key = element_blank()  # Remove legend background
  # )


