# 加载必要的包
rm(list=ls())
library(readxl)
library(dplyr)
library(broom)
library(ggpmisc) 

# path <- './data/ex.fig1/d_CO2Rate.LinearFitting.xlsx' 
path <- './data/ex-fig1/a_CO2.MultiSludgeGroup.xlsx'
sheet.name <- 'MultiSludge'  # Sheet1
rawdata <- data.frame(read_excel(path, sheet = sheet.name))

filtered_data <- rawdata %>%  
  filter(Time >= 4)

fit_results <- filtered_data %>%
  group_by(Group) %>%
  do({
    model <- lm(HCO3 ~ Time, data = .)

    coef_data <- tidy(model)
    glance_data <- glance(model)

    data.frame(
      Group = first(.$Group),
      Intercept = coef_data$estimate[coef_data$term == "(Intercept)"],
      Slope = coef_data$estimate[coef_data$term == "Time"],
      Intercept_SE = coef_data$std.error[coef_data$term == "(Intercept)"],
      Slope_SE = coef_data$std.error[coef_data$term == "Time"],
      Intercept_p = coef_data$p.value[coef_data$term == "(Intercept)"],
      Slope_p = coef_data$p.value[coef_data$term == "Time"],
      R_squared = glance_data$r.squared,
      Adj_R_squared = glance_data$adj.r.squared,
      p_value = glance_data$p.value
    )
  })

print("The linear fitting results:")
print(fit_results)

palete <- c("#E69F00", "#D55E00", "#56B4E9", "#009E73")
# ,  "#F0E442", "#0072B2", "#363062","#B31312", "#67729D", "#E7BCDE", "#BBE2EC",  "#F6EACB"


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

theme_set(theme_cns)
# theme_set(theme_classic() +
#             theme(
#               axis.text = element_text(size = 12, color = "black"),
#               axis.title = element_text(size = 14, face = "bold"),
#               legend.position = "right",
#               legend.text = element_text(size = 12),
#               legend.title = element_blank(),
#               plot.margin = unit(c(1, 1, 1, 1), "lines"),
#               plot.background = element_rect(fill = 'transparent', color = NA),
#               panel.background = element_rect(fill = 'transparent'),
#               panel.grid.major = element_blank(),
#               panel.grid.minor = element_blank(),
#               strip.text = element_text(size = 12, face = "bold")
#             ))

plot <- ggplot(filtered_data, aes(x = Time, y = HCO3, color = Group, fill = Group)) +
  geom_point(size = 2, shape = 21, stroke = 1, alpha = 0.8) +  # 添加数据点，使用带边框的圆点
  geom_smooth(method = "lm", se = TRUE, alpha = 0.3, size = 1.2) +  # 添加拟合线和置信区间
  facet_wrap(~ Group, scales = "free") +  # 按组分面
  labs(
    title = "",
    x = "Time (d)",
    y = "HCO3"
  ) +
  scale_color_manual(values = palete) +  
  scale_fill_manual(values = palete) +   

  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    parse = TRUE,
    size = 2.5,
    label.x = 0.05,
    label.y = 0.95
  ) +
  guides(color = guide_legend(override.aes = list(fill = palete, alpha = 0.5)))  # 调整图例

print(plot)

