rm(list=ls())
library(readxl)
library(ggplot2)
library(ggpubr)

path <- './data/ex-fig12/EPS.xlsx'
sheet.name <- 'Spectra.R1'
rawdata <- data.frame(read_excel(path, sheet = sheet.name))
head(rawdata)
# Wavel.      Strain Light    Abs
# 1    300 Deinococcus  Dark 0.5512
# 2    302 Deinococcus  Dark 0.5060
# 3    304 Deinococcus  Dark 0.4787
# 4    306 Deinococcus  Dark 0.4540
# 5    308 Deinococcus  Dark 0.4319
# 6    310 Deinococcus  Dark 0.4150

# 设置要绘制的菌株名称
strain_name <- "Aliidiomarina"  # Aliidiomarina ; Deinococcus

# 筛选指定菌株的数据
plot_data <- rawdata[rawdata$Strain == strain_name, ]
plot_data <- plot_data[plot_data$Wavel. <= 600, ]


# 设置CNS风格的主题
theme_cns <- theme(
  panel.background = element_rect(fill = "transparent"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(color = "black", size = 0.4),
  axis.ticks = element_line(color = "black", size = 0.4),
  axis.text.x = element_text(color = "black", size = 7, face = "plain",), # 
  axis.text.y = element_text(color = "black", size = 7, face = "plain"),
  axis.title = element_text(color = "black", size = 7, face = "plain"),
  legend.position = "right", # "right"
  legend.title = element_blank(),
  legend.text = element_text(size = 7, face = "plain"),
  legend.background = element_rect(fill = "white", color = NA),
  legend.key = element_rect(fill = "white", color = NA),
  plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
  plot.title = element_text(size = 9, face = 'plain', hjust = 0.5)
)


# 绘制光谱图
p <- ggplot(plot_data, aes(x = Wavel., y = Abs, color = Day, fill = Day)) +
  # 添加均值线和置信区间
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.3, linetype = "blank") +
  # 设置颜色
  scale_color_manual(values = c('Control' = '#AA4736',"Day 1" = "#FF836E", "Day 7" = "#FFB5A8")
                     # , labels = c("Dark" = "Dark", "IR" = "IR")
                     ) +
  scale_fill_manual(values = c('Control' = '#AA4736',"Day 1" = "#FF836E", "Day 7" = "#FFB5A8")
                    # , labels = c("Dark" = "Dark", "IR" = "IR")
                    ) +
  # 设置坐标轴标签
  labs(x = "Wavelength (nm)", y = "Absorbance", title = strain_name) +
  # 应用CNS主题
  theme_cns +
  # 设置图例标题
  guides(color = guide_legend(title = "Day"),
         fill = guide_legend(title = "Day"))

# 显示图形
print(p)
# 
# # 保存图形
# ggsave(paste0("./output/Fig4_", strain_name, "_spectrum.pdf"), 
#        p, width = 10, height = 6, dpi = 300)

