rm(list=ls())
library(readxl)
library(ggplot2)
library(ggpubr)

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
  plot.title = element_text(size = 14, face = 'plain', hjust = 0.5)
)

# load data
path <- './data/Fig5/Ofar_Continuous.xlsx' 
sheet.name <- 'C' # C # N_multiple
rawdata <- data.frame(read_excel(path, sheet = sheet.name))
head(rawdata)
#         Samples   Time    Values Light     Flow   Ion             Group
# 1   Ofar-D-6.24       0 0.1928412  Dark Effluent HCO3- DarkEffluentHCO3-
# 2        Ofar-I       0 0.2109722    IR Effluent HCO3-   IREffluentHCO3-
# 3 Ofar-D-6.27-1       3 0.1912643  Dark Effluent HCO3- DarkEffluentHCO3-
# 4        Ofar-I       3 0.2200367    IR Effluent HCO3-   IREffluentHCO3-
# 5 Ofar-D-6.27-2       3 0.2144484  Dark Effluent HCO3- DarkEffluentHCO3-
# 6        Ofar-I       3 0.2289726    IR Effluent HCO3-   IREffluentHCO3-


library(scales)
library(tidyr)
library(ggplot2)

# 修改后的绘图代码
p <- ggplot(rawdata, aes(x = Time, y = Values, color = Group, shape = Flow)) +
  geom_point(size = 2, alpha = 0.7) +
  # geom_line(aes(group = Group), alpha = 0.5) +  # 添加趋势线
  
  # 设置颜色
  scale_color_manual(values = c("#3C5488", "#4DBBD5", "#E64B35", "#A90A00",  "#4DBBD5", "#00A087", "#3C5488", '#6E77C8','#AAC2E8',
                                "#A90A00", "#9DBBD5", "#90A087", "#4F7488", '#9E77C8','#BAC2E8')) +
  # 设置形状
  scale_shape_manual(values = c(16, 17)) +  # 使用圆形和三角形
  # 设置坐标轴
  labs(x = "Time (days)",
       y = "Carbonate (mg C L-1)") + # Carbonate (mg C L-1)
  theme_cns
  # # 设置图例
  # theme(legend.position = "right",
  #       legend.title = element_text(size = 14),
  #       legend.text = element_text(size = 12),
  #       axis.text = element_text(size = 12, color = "black"),
  #       axis.title = element_text(size = 14, color = "black"),
  #       axis.line = element_line(size = 0.5))
p

# 保存图片
ggsave("Ofar_Performance.pdf", p, width = 10, height = 6, dpi = 300)

# ================================ Multiple N ================================
path.N <- './data/Fig1/Ofar_Continuous.xlsx' 
sheet.name.N <- 'N_multiple' 
rawdata.N <- data.frame(read_excel(path.N, sheet = sheet.name.N))
head(rawdata.N)

# 修改后的绘图代码
p <- ggplot(rawdata.N, aes(x = Time, y = Values, color = Group, shape = Flow)) +
  geom_point(size = 5, alpha = 0.7) +
  # geom_line(aes(group = Group), alpha = 0.5) +  # 添加趋势线

  # 设置颜色
  scale_color_manual(values = c( "#4F7488", "#525787","#90A087", "#4DBBD5", "#3C5488", "#00A087",
                                 "#FF625A", "#FF8454", "#FFB12C","#61272F","#C8693B", "#FFD491")) +
  # 设置坐标轴
  labs(x = "Time (days)",
       y = "Nitrogen (mg N L−1)") +
  theme_cns
  # # 设置图例
  # theme(legend.position = "right",
  #       legend.title = element_text(size = 14),
  #       legend.text = element_text(size = 12),
  #       axis.text = element_text(size = 12, color = "black"),
  #       axis.title = element_text(size = 14, color = "black"),
  #       axis.line = element_line(size = 0.5))
p

