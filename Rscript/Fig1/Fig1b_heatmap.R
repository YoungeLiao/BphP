rm(list=ls())
# load library
library(readxl)
library(ggplot2)
library(ggpubr)

# load data
path <- './data/Fig1/Fig1b_CO2.MultiSludgeGroup.xlsx' 
sheet.name <- 'MultiSludge' # 'MultiSludge' # CN
rawdata <- data.frame(read_excel(path, sheet = sheet.name))
head(rawdata)
# Samples Time.raw Time    Group Batch               HCO3 Raw.CO2.IC           Raw
# 1  M1-0618        0    0 MAB_25°C   Low 485.69037656903765  0.2000000       Default
# 2  M2-0618        0    0 MAB_25°C   Low 485.69037656903765  0.2000000       Default
# 3  M3-0618        0    0 MAB_37°C   Low 485.69037656903765  0.2000000       Default
# 4  5%-0618        0    0       PD   Low 485.69037656903765  0.2000000       Default
# 5 FAB-0618        0    0      FAB   Low 485.69037656903765  0.2000000       Default
# 6  M1-0618        1    1 MAB_25°C   Low 405.39810900348164  0.1760127 SalineAnammox

# 数据处理：先转换为数值，然后过滤NA值
rawdata$HCO3 <- as.numeric(as.character(rawdata$HCO3))  # 先转为字符再转数值
rawdata <- rawdata[!is.na(rawdata$HCO3), ]  # 过滤NA值
rawdata$Time <- as.factor(rawdata$Time)
rawdata$Group <- factor(rawdata$Group, levels = unique(rawdata$Group))


# 设置CNS风格的主题
theme_cns <- theme(
  panel.background = element_rect(fill = "transparent"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(color = "black", size = 0.4),
  axis.ticks = element_line(color = "black", size = 0.4),
  axis.text.x = element_text(color = "black", size = 7, face = "plain", 
                             angle = 30, hjust = 1), # angle = 30, hjust = 0
  axis.text.y = element_text(color = "black", size = 7, face = "plain"),
  axis.title = element_text(color = "black", size = 7, face = "plain"),
  legend.position = "right",
  legend.title = element_text(color = "black", size = 7, face = "plain"),
  legend.text = element_text(size = 7, face = "plain"),
  legend.background = element_rect(fill = "white", color = NA),
  legend.key = element_rect(fill = "white", color = NA),
  plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
  plot.title = element_text(size = 8, face = 'plain', hjust = 0.5)
)


# 创建CNS风格的热图函数
create_heatmap <- function(data_subset, title) {
  ggplot(data_subset, aes(x = Group, y = Time, fill = HCO3)) +  # 交换x和y
    geom_tile(color = "white", size = 0.5) +
    scale_fill_gradient2(low = "#3F4965", mid = "#FFEAC8", high = "#a50026", # v2[low = "#3F4965", mid = "#FFEAC8", high = "#a50026"] v1["#ffffbf" "#313695" ]
                         midpoint = median(data_subset$HCO3, na.rm = TRUE),
                         name = "HCO3 (mg/L)") +
    labs(x = "", y = "Time (days)", title = title) +  # 更新标签
    theme_classic() +
    theme_cns
}

# 按Batch分组数据
low_batch <- subset(rawdata, Batch == "Low")
high_batch <- subset(rawdata, Batch == "High")

# 创建两个热图
p1 <- create_heatmap(low_batch, "Low")
p2 <- create_heatmap(high_batch, "High")

# 合并两个热图，共用y轴标签
final_plot <- ggarrange(p1, p2, ncol = 2, common.legend = TRUE, legend = "right",
                        align = "v",
                        widths = c(1, 1))

# 显示图形
print(final_plot)

# # 保存图形（可选）
# ggsave("./results/Fig1c_heatmap.pdf", final_plot, width = 12, height = 6, dpi = 300)

