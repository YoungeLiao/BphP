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
sheet.name <- 'PCA'
rawdata <- data.frame(read_excel(path, sheet = sheet.name))
head(rawdata)
# Name   W.OP5.D3   W.OP5.D2  W.OP5.D1 W.OP5.I3   W.OP5.I2 W.OP5.I1      W.QC6      W.QC5      W.QC4      W.QC3      W.QC2       W.QC1
# 1        D-(-)-Mannitol   65200.88   54123.21   56856.3   176715   155363.7  4396724   443651.3   561977.2   433703.2   395263.9    79592.2    90418.97
# 2            Isoleucine  380344.40  672515.88  306677.0  7260509 14904896.0 12910394  9332705.3  9363914.9  7765744.6 13638625.5 10957140.3 13627334.22
# 3 4-Phenolsulfonic acid  430760.64 1920218.04 1638524.0 19563807 55611860.8 11609921 14292529.4 15936549.1 16415517.3 19207349.1 19943360.7 22342439.49
# 4          Salicylamide  684321.30  571303.59  361191.5  2601483  1579406.8  2335993  1711803.5  1102158.2  1156995.7  1365011.3  1887840.7  1013992.87
# 5    6-Methoxyquinoline 1326736.46  517270.68 3114455.5  4660896  6099136.6  4750900  2154053.9  2627371.1  2635832.4  3179480.0   700044.0   343998.25
# 6        Nipecotic acid 1968219.54 2355431.38 2223583.5  5653966  6845615.0  7884235 54031765.0 51636973.4 49910345.3 66003871.1 66709958.2 66534907.63

library(factoextra)
library(FactoMineR)
library(pls)

# -------------------- 1. Without QC ----------------------

# 数据预处理
# 提取代谢物名称和数值列
pca_data <- rawdata[, -1]  # 移除Name列

# 转置数据，使样品为行，代谢物为列
pca_data_t <- as.data.frame(t(pca_data))

# 创建样品信息数据框
sample_info <- data.frame(
  Sample = rownames(pca_data_t),
  stringsAsFactors = FALSE
)

# 根据样品名称添加分组信息（只保留Dark和IR组）
sample_info <- sample_info %>%
  filter(grepl("D[123]$|I[123]$", Sample)) %>%
  mutate(Group = ifelse(grepl("D[123]$", Sample), "Dark", "IR"))

# 筛选数据，只保留Dark和IR组
pca_data_t <- pca_data_t[rownames(pca_data_t) %in% sample_info$Sample, ]

# 确保数值列为数值类型
for (col in names(pca_data_t)) {
  pca_data_t[[col]] <- as.numeric(pca_data_t[[col]])
}

# 数据标准化（对数转换和中心化）
# 对数转换（添加1避免log(0)）
pca_data_log <- log10(pca_data_t + 1)

# 中心化标准化
pca_data_scaled <- scale(pca_data_log)

# 执行PCA分析
pca_result <- PCA(pca_data_scaled, 
                  graph = FALSE, 
                  ncp = 5,  # 保留前5个主成分
                  scale.unit = FALSE)  # 已经标准化

# 提取样品得分
sample_scores <- as.data.frame(pca_result$ind$coord)
colnames(sample_scores) <- c("t1", "t2", "t3", "t4", "t5")  # 重命名列

# 只保留前两个主成分
sample_scores <- sample_scores[, c("t1", "t2")]

# 添加分组信息
sample_scores$Group <- sample_info$Group

# 计算每个主成分解释的方差百分比
variance_explained <- pca_result$eig[, 2]
variance_explained <- c(
  t1 = round(variance_explained[1], 1),
  t2 = round(variance_explained[2], 1)
)

# 手动计算椭圆参数的函数
calculate_ellipse <- function(x, y, level = 0.95) {
  # 计算均值和协方差矩阵
  center <- c(mean(x), mean(y))
  cov_matrix <- cov(cbind(x, y))
  
  # 计算椭圆参数
  chi_sq <- qchisq(level, 2)
  eigen_values <- eigen(cov_matrix)$values
  eigen_vectors <- eigen(cov_matrix)$vectors
  
  # 计算椭圆的长轴和短轴
  a <- sqrt(chi_sq * eigen_values[1])
  b <- sqrt(chi_sq * eigen_values[2])
  
  # 计算椭圆的旋转角度
  angle <- atan2(eigen_vectors[2, 1], eigen_vectors[1, 1])
  
  # 生成椭圆的点
  theta <- seq(0, 2 * pi, length.out = 100)
  ellipse_x <- center[1] + a * cos(theta) * cos(angle) - b * sin(theta) * sin(angle)
  ellipse_y <- center[2] + a * cos(theta) * sin(angle) + b * sin(theta) * cos(angle)
  
  return(data.frame(x = ellipse_x, y = ellipse_y))
}

# 为每个组计算椭圆
ellipses <- data.frame()
for (group in unique(sample_scores$Group)) {
  group_data <- sample_scores[sample_scores$Group == group, ]
  if (nrow(group_data) >= 3) {  # 至少需要3个点才能计算椭圆
    ellipse <- calculate_ellipse(group_data$t1, group_data$t2)
    ellipse$Group <- group
    ellipses <- rbind(ellipses, ellipse)
  }
}

# 创建PCA图
p <- ggplot(sample_scores, aes(x = t1, y = t2, color = Group, shape = Group)) +
  geom_point(size = 3, alpha = 0.7) +
  # 添加手动计算的95%置信椭圆
  geom_path(data = ellipses, aes(x = x, y = y, color = Group), 
            linetype = "dashed", size = 0.5, show.legend = FALSE) +
  # 设置坐标轴标签
  labs(x = paste0("t1 (", variance_explained[1], "%)"),
       y = paste0("t2 (", variance_explained[2], "%)"),
       color = "Group", shape = "Group") +
  # 设置颜色
  scale_color_manual(values = c("Dark" = "#023270", "IR" = "#D34947")) +
  scale_shape_manual(values = c("Dark" = 16, "IR" = 17)) +
  # 应用CNS风格主题
  theme_cns +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 7, face = "plain"),
    legend.key = element_rect(fill = "white", color = NA)
  )

# 显示PCA图
print(p)

# 保存PCA图（可选）
# ggsave("./Fig4_PCA.png", p, width = 8, height = 6, dpi = 300)

# 显示主成分贡献度
print("Variance explained by each principal component:")
print(pca_result$eig[, 1:2])


# # -------------------- 2. With QC ----------------------
# # 数据预处理
# # 提取代谢物名称和数值列
# pca_data <- rawdata[, -1]  # 移除Name列
# 
# # 转置数据，使样品为行，代谢物为列
# pca_data_t <- as.data.frame(t(pca_data))
# 
# # 创建样品信息数据框
# sample_info <- data.frame(
#   Sample = rownames(pca_data_t),
#   stringsAsFactors = FALSE
# )
# 
# # 根据样品名称添加分组信息（包含Dark、IR和QC组）
# sample_info$Group <- ifelse(grepl("D[123]$", sample_info$Sample), "Dark",
#                             ifelse(grepl("I[123]$", sample_info$Sample), "IR",
#                                    "QC"))
# 
# # 确保数值列为数值类型
# for (col in names(pca_data_t)) {
#   pca_data_t[[col]] <- as.numeric(pca_data_t[[col]])
# }
# 
# # 数据标准化（对数转换和中心化）
# # 对数转换（添加1避免log(0)）
# pca_data_log <- log10(pca_data_t + 1)
# 
# # 中心化标准化
# pca_data_scaled <- scale(pca_data_log)
# 
# # 执行PCA分析
# pca_result <- PCA(pca_data_scaled, 
#                   graph = FALSE, 
#                   ncp = 5,  # 保留前5个主成分
#                   scale.unit = FALSE)  # 已经标准化
# 
# # 提取样品得分
# sample_scores <- as.data.frame(pca_result$ind$coord)
# colnames(sample_scores) <- c("t1", "t2", "t3", "t4", "t5")  # 重命名列
# 
# # 只保留前两个主成分
# sample_scores <- sample_scores[, c("t1", "t2")]
# 
# # 添加分组信息
# sample_scores$Group <- sample_info$Group
# 
# # 计算每个主成分解释的方差百分比
# variance_explained <- pca_result$eig[, 2]
# variance_explained <- c(
#   t1 = round(variance_explained[1], 1),
#   t2 = round(variance_explained[2], 1)
# )
# 
# # 手动计算椭圆参数的函数
# calculate_ellipse <- function(x, y, level = 0.95) {
#   # 计算均值和协方差矩阵
#   center <- c(mean(x), mean(y))
#   cov_matrix <- cov(cbind(x, y))
#   
#   # 计算椭圆参数
#   chi_sq <- qchisq(level, 2)
#   eigen_values <- eigen(cov_matrix)$values
#   eigen_vectors <- eigen(cov_matrix)$vectors
#   
#   # 计算椭圆的长轴和短轴
#   a <- sqrt(chi_sq * eigen_values[1])
#   b <- sqrt(chi_sq * eigen_values[2])
#   
#   # 计算椭圆的旋转角度
#   angle <- atan2(eigen_vectors[2, 1], eigen_vectors[1, 1])
#   
#   # 生成椭圆的点
#   theta <- seq(0, 2 * pi, length.out = 100)
#   ellipse_x <- center[1] + a * cos(theta) * cos(angle) - b * sin(theta) * sin(angle)
#   ellipse_y <- center[2] + a * cos(theta) * sin(angle) + b * sin(theta) * cos(angle)
#   
#   return(data.frame(x = ellipse_x, y = ellipse_y))
# }
# 
# # 为每个组计算椭圆
# ellipses <- data.frame()
# for (group in unique(sample_scores$Group)) {
#   group_data <- sample_scores[sample_scores$Group == group, ]
#   if (nrow(group_data) >= 3) {  # 至少需要3个点才能计算椭圆
#     ellipse <- calculate_ellipse(group_data$t1, group_data$t2)
#     ellipse$Group <- group
#     ellipses <- rbind(ellipses, ellipse)
#   }
# }
# 
# # 创建PCA图
# p <- ggplot(sample_scores, aes(x = t1, y = t2, color = Group, shape = Group)) +
#   geom_point(size = 3, alpha = 0.7) +
#   # 添加手动计算的95%置信椭圆
#   geom_path(data = ellipses, aes(x = x, y = y, color = Group), 
#             linetype = "dashed", size = 0.5, show.legend = FALSE) +
#   # 设置坐标轴标签
#   labs(x = paste0("t1 (", variance_explained[1], "%)"),
#        y = paste0("t2 (", variance_explained[2], "%)"),
#        color = "Group", shape = "Group") +
#   # 设置颜色
#   scale_color_manual(values = c("Dark" = "#023270", "IR" = "#D34947", "QC" = "grey50")) +
#   scale_shape_manual(values = c("Dark" = 16, "IR" = 17, "QC" = 15)) +
#   # 应用CNS风格主题
#   theme_cns +
#   theme(
#     legend.position = "right",
#     legend.title = element_blank(),
#     legend.text = element_text(size = 7, face = "plain"),
#     legend.key = element_rect(fill = "white", color = NA)
#   )
# 
# # 显示PCA图
# print(p)
# 
# # 保存PCA图（可选）
# # ggsave("./Fig4_PCA.png", p, width = 8, height = 6, dpi = 300)
# 
# # 显示主成分贡献度
# print("Variance explained by each principal component:")
# print(pca_result$eig[, 1:2])
# 
