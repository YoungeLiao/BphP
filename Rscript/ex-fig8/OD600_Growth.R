rm(list=ls())
library(readxl)
library(ggplot2)
library(ggpubr)
library(dplyr)

path <- './data/ex-fig8/Growth_IR5Strains.xlsx' 
sheet.name <- 'R1.2-round2' # R1.1-round1
rawdata <- data.frame(read_excel(path, sheet = sheet.name))

# 获取所有菌株名称
strain_names <- unique(rawdata$Strain)

# 计算每个时间点、每个菌株和光照条件下OD600的平均值和标准误
data_summary <- rawdata %>%
  group_by(Time, Strain, Light) %>%
  summarise(
    mean_OD600 = mean(OD600, na.rm = TRUE),
    se_OD600 = sd(OD600, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

# 计算全局的纵坐标范围
y_min <- min(data_summary$mean_OD600 - data_summary$se_OD600, na.rm = TRUE)
y_max <- max(data_summary$mean_OD600 + data_summary$se_OD600, na.rm = TRUE)

# 设置CNS风格的主题
theme_cns <- theme(
  panel.background = element_rect(fill = "transparent"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(color = "black", size = 0.5),
  axis.ticks = element_line(color = "black", size = 0.5),
  axis.text.x = element_text(color = "black", size = 7, face = "plain"), # hjust = 1 # angle = 30, 
  axis.text.y = element_text(color = "black", size = 7, face = "plain"),
  axis.title = element_text(color = "black", size = 7, face = "plain"),
  legend.position = "none", # "right",
  legend.title = element_blank(),
  legend.text = element_text(size = 7, face = "plain"),
  legend.background = element_rect(fill = "white", color = NA),
  legend.key = element_rect(fill = "white", color = NA),
  plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
  plot.title = element_text(size = 7, face = 'plain', hjust = 0.5)
)

# 为每个菌株创建生长曲线图
plot_list <- list()
for (i in 1:length(strain_names)) {
  strain_name <- strain_names[i]
  strain_data <- data_summary[data_summary$Strain == strain_name, ]
  
  p <- ggplot(strain_data, aes(x = Time, y = mean_OD600, color = Light, group = Light)) +
    # 添加点和线
    geom_point(size = 2.5, alpha = 0.5) +
    geom_line(size = 0.5) +
    # 添加误差棒
    geom_errorbar(aes(ymin = mean_OD600 - se_OD600, ymax = mean_OD600 + se_OD600),
                  width = 0.3, size = 0.5) +
    # 设置颜色
    scale_color_manual(values = c("Dark" = "#377EB8", "IR" = "#E41A1C"),
                       labels = c("Dark" = "Dark", "IR" = "IR")) +
    # 设置坐标轴范围
    coord_cartesian(ylim = c(y_min, y_max)) +
    # 设置坐标轴标签
    labs(x = "Time (h)", y = "OD600", title = strain_name) +
    # 应用CNS主题
    theme_cns +
    # 设置图例标题
    guides(color = guide_legend(title = "Light condition"))
  
  plot_list[[i]] <- p
}

# 将所有子图拼到一张大图中，设置为一行五列
combined_plot <- ggarrange(plotlist = plot_list, ncol = 5, nrow = 1, 
                           common.legend = TRUE, legend = "right",
                           align = "hv", vjust = 1, hjust = 1)

# 显示图形
print(combined_plot)

# 保存图形
# ggsave("./output/Fig4_OD600_growth.pdf", combined_plot, width = 20, height = 6, dpi = 300)
