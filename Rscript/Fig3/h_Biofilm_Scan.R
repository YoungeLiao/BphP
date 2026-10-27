# rm(list=ls())
# library(readxl)
# library(ggplot2)
# library(ggpubr)
# 
# path <- './data/Fig4/Biofim_CrystalViolet.xlsx' # Opto3 # Growth_Opto2.xlsx
# sheet.name <- 'Scan'
# rawdata <- data.frame(read_excel(path, sheet = sheet.name))
# head(rawdata)
# # Wavel.      Strain Light    Abs
# # 1    300 Deinococcus  Dark 0.5512
# # 2    302 Deinococcus  Dark 0.5060
# # 3    304 Deinococcus  Dark 0.4787
# # 4    306 Deinococcus  Dark 0.4540
# # 5    308 Deinococcus  Dark 0.4319
# # 6    310 Deinococcus  Dark 0.4150
# 
# # 设置要绘制的菌株名称
# strain_name <- "Aliidiomarina"  # Aliidiomarina ; Deinococcus
# 
# # 筛选指定菌株的数据
# plot_data <- rawdata[rawdata$Strain == strain_name, ]
# 

## -------------------- v2 ------------------------
rm(list=ls())
library(readxl)
library(ggplot2)
library(ggpubr)

path <- './data/Fig3/Biofim_CrystalViolet.xlsx'
sheet.name <- 'Scan'
rawdata <- data.frame(read_excel(path, sheet = sheet.name))

# 获取所有菌株名称
strain_names <- unique(rawdata$Strain)

# 计算全局的纵坐标范围
y_min <- min(rawdata$Abs, na.rm = TRUE)
y_max <- max(rawdata$Abs-0.35, na.rm = TRUE)

# 设置CNS风格的主题
theme_cns <- theme(
  panel.background = element_rect(fill = "transparent"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(color = "black", size = 0.4),
  axis.ticks = element_line(color = "black", size = 0.4),
  axis.text.x = element_text(color = "black", size = 7, face = "plain", 
                             angle = 30, hjust = 1), # 
  axis.text.y = element_text(color = "black", size = 7, face = "plain"),
  axis.title = element_text(color = "black", size = 7, face = "plain"),
  legend.position = "right", # "right"
  legend.title = element_blank(),
  legend.text = element_text(size = 7, face = "plain"),
  legend.background = element_rect(fill = "white", color = NA),
  legend.key = element_rect(fill = "white", color = NA),
  plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
  plot.title = element_text(size = 8, face = 'plain', hjust = 0.5)
)


# 为每个菌株创建光谱图
plot_list <- list()
for (i in 1:length(strain_names)) {
  strain_name <- strain_names[i]
  plot_data <- rawdata[rawdata$Strain == strain_name, ]
  
  p <- ggplot(plot_data, aes(x = Wavel., y = Abs, color = Light, fill = Light)) +
    # 添加均值线和置信区间
    stat_summary(fun = mean, geom = "line", size = 1.2) +
    stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.3, linetype = "blank") +
    # 设置颜色
    scale_color_manual(values = c("Dark" = "#023270", "IR" = "#D34947"),
                       labels = c("Dark" = "Dark", "IR" = "IR")) +
    scale_fill_manual(values = c("Dark" = "#023270", "IR" = "#D34947"),
                      labels = c("Dark" = "Dark", "IR" = "IR")) +
    # 设置坐标轴范围
    coord_cartesian(ylim = c(y_min, y_max)) +
    # 设置坐标轴标签
    labs(x = "Wavelength (nm)", y = "Absorbance", title = strain_name) +
    # 应用CNS主题
    theme_cns +
    # 设置图例标题
    guides(color = guide_legend(title = "Light condition"),
           fill = guide_legend(title = "Light condition"))
  
  plot_list[[i]] <- p
}

# 将所有子图拼到一张大图中
combined_plot <- ggarrange(plotlist = plot_list, 
                           ncol = 3, nrow = 1, 
                           common.legend = TRUE, legend = "right",
                           align = 'hv', vjust = 1, hjust = 1)

# 显示图形
print(combined_plot)

# 保存图形
# ggsave("./output/Fig4_all_strains_spectrum.pdf", combined_plot, width = 20, height = 6, dpi = 300)
