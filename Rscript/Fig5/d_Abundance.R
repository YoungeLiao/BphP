rm(list=ls())
library(readxl)
library(ggplot2)
library(ggpubr)
library(scales)
library(tidyr)
library(ggplot2)

taxa_colors <- c(
  "#D34947",
  "#315F91",
  "#6FA6C1",
  "#9BC3D3",
  "#7FB9AD",
  "#A8CFB8",
  "#C9C19F",
  "#D9A58C",
  "#B4A8C1",
  "#D0D0CA"
)

taxa_colors <- c(
  "#E0524F",  # 1  IR-responsive
  "#1769AA",  # 2  core blue
  "#3F8FC0",  # 3  bright blue
  "#72B7D2",  # 4  cyan-blue
  "#45A995",  # 5  teal
  "#7BC47F",  # 6  green
  "#B8C94A",  # 7  yellow-green
  "#F0A35B",  # 8  orange
  "#C77BC7",  # 9  purple
  "#A8A8A8"   # 10 gray
)


# 设置CNS风格的主题
theme_cns <- theme(
  panel.background = element_rect(fill = "transparent"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(color = "black", size = 0.4),
  axis.ticks = element_line(color = "black", size = 0.4),
  axis.text.x = element_text(color = "black", size = 7, face = "plain" # angle = 30, hjust = 1
                             ), # 
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

# load data
path <- './data/Fig5/d_taxa.percents.table.xlsx' 
sheet.name <- 'Enriched'
rawdata <- data.frame(read_excel(path, sheet = sheet.name))
head(rawdata, n=30)
# Genus   Relative_Abundance Time Light
# 1               Marinobacter  0.00365980807285571   T0  Dark
# 2              Aliidiomarina   0.0012341213268932   T0  Dark
# 3                Marinicella   0.0339170585356512   T0  Dark
# 4                    Thauera  0.00146817881992467   T0  Dark
# 5                 Paracoccus  0.00500031916930867   T0  Dark
# 6        Candidatus_Brocadia 0.000574504755622699   T0  Dark
# 7                Pseudomonas  0.00046811498606294   T0  Dark
# 8  norank_o__Actinomarinales  0.13490222780177458   T0  Dark
# 9                  Halomonas  0.00606421686490627   T0  Dark
# 10      Candidatus_Scalindua  0.11419877864544545   T0  Dark
# 11              Marinobacter  0.00365980807285571   T0    IR
# 12             Aliidiomarina   0.0012341213268932   T0    IR
# 13               Marinicella   0.0339170585356512   T0    IR
# 14                   Thauera  0.00146817881992467   T0    IR
# 15                Paracoccus  0.00500031916930867   T0    IR
# 16       Candidatus_Brocadia 0.000574504755622699   T0    IR
# 17               Pseudomonas  0.00046811498606294   T0    IR
# 18 norank_o__Actinomarinales  0.13490222780177458   T0    IR
# 19                 Halomonas  0.00606421686490627   T0    IR
# 20      Candidatus_Scalindua  0.11419877864544545   T0    IR
# 21              Marinobacter  0.13773219567206418   Tt  Dark
# 22             Aliidiomarina   0.0572164180692384   Tt  Dark
# 23               Marinicella  0.12128433729812542   Tt  Dark
# 24                   Thauera   0.0743451709683596   Tt  Dark
# 25                Paracoccus   0.0415345660361299   Tt  Dark
# 26       Candidatus_Brocadia  0.10241079217822414   Tt  Dark
# 27               Pseudomonas 0.021043896418920356   Tt  Dark
# 28 norank_o__Actinomarinales   0.0076813413622146   Tt  Dark
# 29                 Halomonas   0.0323637679000787   Tt  Dark
# 30      Candidatus_Scalindua   0.0103623635551205   Tt  Dark

# 在library部分修改
library(ggplot2)
library(ggpubr)
library(scales)
library(tidyr)
library(dplyr)
library(ggalluvial)

# 在load data部分后添加
# 设置光照条件
light_condition <- "Dark"  # 可选 "Dark" 或 "IR"

# 筛选对应条件的数据
selected_data <- rawdata[rawdata$Light == light_condition, ]

# 确保相对丰度是数值型
selected_data$Relative_Abundance <- as.numeric(as.character(selected_data$Relative_Abundance))

# 将相对丰度转换为百分比
selected_data$Relative_Abundance <- selected_data$Relative_Abundance * 100

# 为每个物种创建唯一标识符，确保流动线正确连接
selected_data <- selected_data %>%
  group_by(Time) %>%
  arrange(Genus) %>%
  mutate(Genus_id = paste(Genus, row_number(), sep = "_")) %>%
  ungroup()

# 创建堆叠柱状图和流动线
p <- ggplot(selected_data, 
            aes(x = Time, 
                stratum = Genus, 
                alluvium = Genus_id, 
                y = Relative_Abundance, 
                fill = Genus,
                label = Genus)) +
  # 使用geom_flow绘制流动区域
  geom_flow(alpha = 0.6, width = 0.5, knot.prop = TRUE) +
  # 使用geom_stratum绘制柱子，添加更细的边框线
  geom_stratum(width = 0.5, alpha = 0.8, 
               color = "white", linewidth = 0) +
  # # 添加物种标签
  # geom_text(stat = "stratum", size = 2, angle = 90, hjust = 1) +
  # 设置y轴范围为0-60
  scale_y_continuous(labels = percent_format(scale = 1), 
                     limits = c(0, 61),
                     expand = expansion(mult = c(0, 0.05))) +
  # 设置颜色
  scale_fill_manual(values = taxa_colors) +
  labs(y = "Relative Abundance (%)") +
  theme_cns
# + theme(
#   legend.position = "right",
#   legend.text = element_text(size = 6),
#   axis.text.x = element_text(size = 8),
#   axis.text.y = element_text(size = 8),
#   axis.title = element_text(size = 8),
#   panel.grid = element_line(color = "grey90", size = 0.3)
# )


# 显示图形
print(p)

# # 保存图形
# output_path <- paste0("./output/Fig5/d_Abundance_", light_condition, ".pdf")
# ggsave(output_path, p, width = 6, height = 4, dpi = 300)
