rm(list=ls())
library(readxl)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(scales)

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


# 读取数据
path <- './data/Fig4/Extracellular.xlsx'
sheet.name <- 'Heatmap'
rawdata <- data.frame(read_excel(path, sheet = sheet.name))
head(rawdata)
# Name       Formula     Dark3       Dark2      Dark1        IR3       IR2        IR1
# 1                Indole-3-acetic acid   C10 H9 N O2  547559.8    65185.03   366745.2   107916.4   59023.1 14864618.5
# 2               4-Hydroxybenzaldehyde      C7 H6 O2 6864104.9  4424280.36  7815973.7  6558873.5 3994204.9 33575225.0
# 3 1,2-Dipalmitoylphosphatidylglycerol C38 H75 O10 P  182079.6   466329.74   167038.2   143078.2 1134446.0   401037.1
# 4               4-Hydroxybenzoic acid      C7 H6 O3 1093322.7  1456154.11  3258387.0  4301627.4 1225752.0  6122651.5
# 5                Quinazoline-2,4-diol   C8 H6 N2 O2 1151751.0  1305286.45   932074.3   848913.7 1240624.9  4279761.3
# 6               4-Phenolsulfonic acid    C6 H6 O4 S 1338308.1 13776386.94 11279030.7 33612504.4 2805932.7  9571652.9

library(pheatmap)
library(reshape2)

# 数据预处理
# 提取代谢物名称和数值列
heatmap_data <- rawdata[, c(1, 3:ncol(rawdata))]

# 将代谢物名称设置为行名
rownames(heatmap_data) <- heatmap_data$Name
heatmap_data <- heatmap_data[, -1]

# 数据标准化（按行标准化）
heatmap_data_scaled <- t(scale(t(heatmap_data)))

# 处理可能的NA值
heatmap_data_scaled[is.na(heatmap_data_scaled)] <- 0

# 创建分组信息
# 获取实际列数
num_cols <- ncol(heatmap_data)

# 根据实际列数创建分组信息
if (num_cols == 6) {
  # 假设有6列，前3列为Dark，后3列为IR
  annotation_col <- data.frame(
    Condition = c(rep("Dark", 3), rep("IR", 3)),
    row.names = colnames(heatmap_data)
  )
} else {
  # 如果列数不是6，根据列名自动判断分组
  dark_cols <- grep("^Dark", colnames(heatmap_data), value = TRUE)
  ir_cols <- grep("^IR", colnames(heatmap_data), value = TRUE)
  
  annotation_col <- data.frame(
    Condition = c(rep("Dark", length(dark_cols)), rep("IR", length(ir_cols))),
    row.names = c(dark_cols, ir_cols)
  )
}

# 绘制热图
p <- pheatmap(
  heatmap_data_scaled,
  annotation_col = annotation_col,
  show_rownames = TRUE,
  show_colnames = TRUE,
  scale = "none",  # 已经预先标准化
  clustering_distance_rows = "euclidean",
  clustering_distance_cols = "euclidean",
  clustering_method = "complete",
  color = colorRampPalette(c("#164A78", "#F5F2ED", "#D86664"))(100), # "#3F7F9F", "#F5F3EE", "#D9654F"
  # "#5B8FA8", "#F7F7F5", "#C95A4A")
  border_color = NA,
  fontsize = 9,
  fontsize_row = 8,
  fontsize_col = 8,
  angle_col = "45",
  main = ""
)

# scale_fill_gradient2(
#   low = "#164A78",
#   mid = "#F5F2ED",
#   high = "#D86664",
#   midpoint = 0
# )
# scale_fill_gradient2(
#   low = "#D8E3EC",
#   mid = "#F7F5F0",
#   high = "#D34947",
#   midpoint = 0
# )
# 
# scale_fill_gradient2(
#   low = "#4F86A8",
#   mid = "#F7F4EE",
#   high = "#D96865",
#   midpoint = 0
# )

# 保存热图
# png("./Fig4_Metabolite_Heatmap.png", width = 12, height = 10, units = "in", res = 300)
# p
# dev.off()


# 保存热图
# png("./Fig4_Metabolite_Heatmap.png", width = 12, height = 10, units = "in", res = 300)
# p
# dev.off()
