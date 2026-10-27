rm(list=ls())
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggbeeswarm)
library(ggpubr)

# load data
path.global <- "./data/Fig5/e_sample_site2025.07.20.xlsx"

raw.global <- data.frame(read_excel(path.global))
head(raw.global)
# > head(raw.global)
# Sample.ID Project.ID Assembly.ID   Sample.name       Sample.type Environment Host
# 1 CNS0700704 CNP0004049  CNA0055107  GOMC.bin.208 marine metagenome       Ocean   --
#   2 CNS0700971 CNP0004049  CNA0055374  GOMC.bin.475 marine metagenome       Ocean   --
#   3 CNS0701338 CNP0004049  CNA0055967  GOMC.bin.869 marine metagenome       Ocean   --
#   4 CNS0701392 CNP0004049  CNA0056144  GOMC.bin.923 marine metagenome       Ocean   --
#   5 CNS0701563 CNP0004049  CNA0056673 GOMC.bin.1100 marine metagenome       Ocean   --
#   6 CNS0702009 CNP0004049  CNA0058811 GOMC.bin.1546 marine metagenome       Ocean   --
#   Species
# 1                                 d__Bacteria;p__Actinobacteriota;c__Acidimicrobiia;o__UBA5794;f__UBA5794;g__;s__
# 2                      d__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__GCA-2729495;f__GCA-2729495;g__;s__
# 3                                          d__Bacteria;p__Acidobacteriota;c__Aminicenantia;o__S016-38;f__;g__;s__
# 4                                           d__Bacteria;p__Atribacterota;c__JS1;o__SB-45;f__34-128;g__UBA9904;s__
# 5 d__Bacteria;p__Planctomycetota;c__Phycisphaerae;o__Phycisphaerales;f__UBA1924;g__UBA1924;s__UBA1924 sp016764895
# 6                                    d__Archaea;p__Thermoplasmatota;c__Thermoplasmata;o__RBG-16-68-12;f__;g__;s__
# Technology Latitude.and.longitude       Location Country City Collected.date
# 1 metagenome       45.93 N 129.98 W  Pacific Ocean      --   --          42242
# 2 metagenome       36.61 N 122.62 W  Pacific Ocean      --   --          42811
# 3 metagenome       47.76 N 127.76 W  Pacific Ocean      --   --          40736
# 4 metagenome       27.10 N 111.12 W  Pacific Ocean      --   --          40148
# 5 metagenome        22.80 N 46.05 W Atlantic Ocean      --   --          41373
# 6 metagenome       35.69 N 123.08 W  Pacific Ocean      --   --          42809
# Isolation.source Platform Altitude Host.disease
# 1               --       --       --           --
#   2               --       --       --           --
#   3               --       --       --           --
#   4               --       --       --           --
#   5               --       --       --           --
#   6               --       --       --           --
# ... existing code ...

library(maps)
library(ggrepel)
library(stringr)

# 1. 提取纬度和经度
raw.global <- raw.global %>%
  mutate(
    Latitude = as.numeric(str_extract(Latitude.and.longitude, "^[0-9.]+")),
    Lat_dir = str_extract(Latitude.and.longitude, "[NS]"),
    Longitude = as.numeric(str_extract(Latitude.and.longitude, "(?<= )[0-9.]+")),
    Lon_dir = str_extract(Latitude.and.longitude, "[EW]"),
    Latitude = ifelse(Lat_dir == "S", -Latitude, Latitude),
    Longitude = ifelse(Lon_dir == "W", -Longitude, Longitude)
  )

# # 2.1 提取属（Genus）信息
# raw.global <- raw.global %>%
#   mutate(
#     Genus = str_extract(Species, "g__[^;]+"),
#     Genus = str_replace(Genus, "g__", ""),
#     Genus = ifelse(is.na(Genus) | Genus == "", "Unknown", Genus)
#   )

# 2.2 approach 2.0: 兼容性
raw.global <- raw.global %>%
  mutate(
    Order = str_extract(Species, "c__[^;]+"),
    Order = str_replace(Order, "c__", ""),
    Order = ifelse(is.na(Order) | Order == "", "Unknown", Order),
    Genus = str_extract(Species, "g__[^;]+"),
    Genus = str_replace(Genus, "g__", ""),
    Genus = ifelse(is.na(Genus) | Genus == "", "Unknown", Genus),
    # 创建新的分类标识，优先使用Genus，如果没有则使用Order
    Taxonomy = ifelse(Genus != "Unknown", Genus, Order)
  )

# 3. 获取世界地图数据
world_map <- map_data("world")

# 4. 绘制分布图（推荐用geom_polygon）
p <- ggplot() +
  geom_polygon(
    data = world_map,
    aes(x = long, y = lat, group = group),
    fill = "gray95", color = "gray70", linewidth = 0.2
  ) +
  geom_point(
    data = raw.global,
    aes(x = Longitude, y = Latitude, color = Genus),
    size = 1.5, alpha = 0.7
  ) +
  theme_minimal() +
  labs(
    title = "Global Distribution of Genus",
    x = "Longitude", y = "Latitude", color = "Genus"
  ) +
  theme(
    legend.position = "none"
  )

print(p)


# ===== Fun2: Visualize interested taxonomy (CNS style, customizable colors) ======
# c__Anaerolinea # Nitrospira # Aggregatilinea # Halomonas  # Wenzhouxiangella # Woeseia # Hydrogenedentes

# 可自定义高亮属及其颜色
highlight_taxa <- c(
                  # --- Major ---
                   # 'Marinobacter', 'g__Marinobacter',
                   # 'Wenzhouxiangella', 'g__Wenzhouxiangella',
                   'Aliidiomarina',
                   'Paracoccus',
                   'Actinomarinales', 'o__Actinomarinales',
                   # 'Vicingus', 
                   # 'UBA6055',
                   # --- Middle ---
                   # 'Aequorivita', 'g__Aequorivita',
                   # 'Kangiella',
                   # 'Nitrospira_F', 'g__Nitrospira_F',
                   # 'Scalindua', 'g__Scalindua',
                   # --- decrease/Minor ---
                   'Halomonas', 'g__Halomonas'
                   # "Marinicella", "g__Marinicella",
                   # 'Rhodanobacter',
                   # 'Nitrosomonas'
                   )

highlight_colors <- c(
  # "Marinobacter" = '#FF7533',# ,   
  # 'Wenzhouxiangella' = '#FFC107',
  'Aliidiomarina' = 'red',
  # 'Vicingus' = 'red', 
  'Paracoccus' = '#FF9D78',
  # 'UBA6055' = '#FF9D78',
  'Actinomarinales' = '#AAC2E8',
  # --- Middle ---
  # 'Aequorivita'= '#AAC2E8',
  # 'Kangiella' = '#2D58A7',
  # "Scalindua"    = "#5A87DC",   
  # 'Nitrospira_F' = '#8BB7E7',
  
  # --- decrease/Minor ---
  # "Marinicella"  = "#1B9E77",   
  'Halomonas' = '#62714B'
  # 'Rhodanobacter'= '#70C82F',
  # 'Nitrosomonas' = '#2D816A'
)

# 过滤出感兴趣的属的数据
highlight_data <- raw.global %>%
  filter(Taxonomy %in% highlight_taxa | Order %in% highlight_taxa | Genus %in% highlight_taxa)
# highlight_data <- raw.global %>%
#   filter(Genus %in% highlight_genera)
table(highlight_data$Genus)
# Halomonas      Marinicella     Marinobacter        Scalindua 
# 6                5               26                3 
# Wenzhouxiangella 
# 8 


# CNS正刊风格主题
cns_theme <- theme(
  panel.background = element_rect(fill = "white", color = NA),
  plot.background = element_rect(fill = "white", color = NA),
  # panel.grid.major = element_line(color = "gray90", size = 0.3),
  # panel.grid.minor = element_blank(),
  axis.text = element_text(size = 7, color = "black"),
  axis.title = element_text(size = 7, color = "black"),
  plot.title = element_text(size = 8, hjust = 0.5),
  legend.title = element_text(size = 7),
  legend.text = element_text(size = 7),
  legend.key = element_rect(fill = "white", color = NA),
  legend.position = "right"
)

# 绘制地图
p2 <- ggplot() +
  # 世界地图底图
  geom_polygon(
    data = world_map,
    aes(x = long, y = lat, group = group),
    fill = "gray98", color = "gray80", linewidth = 0.3
  ) +
  # 所有点（背景点，淡灰色）
  geom_point(
    data = raw.global,
    aes(x = Longitude, y = Latitude),
    color = "gray80", size = 1.5, alpha = 0.4
  ) +
  # # 高亮感兴趣的属
  # geom_point(
  #   data = highlight_data,
  #   aes(x = Longitude, y = Latitude, color = Taxonomy), # color = Genus
  #   size = 2.5, alpha = 0.7, stroke = 0.2
  # ) +
  # scale_color_manual(
  #   values = highlight_colors,
  #   breaks = highlight_taxa,
  #   name = "Taxonomy" # name = "Genus"
  # ) +
  # 高亮感兴趣的属
  geom_point(
    data = highlight_data,
    aes(x = Longitude, y = Latitude, color = Taxonomy, fill = Taxonomy), # 添加fill映射
    size = 2.5, alpha = 0.4, stroke = 1.5, shape = 21  # 使用shape=21实现填充点和边框
  ) +
  # 添加填充颜色和边框颜色
  scale_fill_manual(
    values = highlight_colors,
    breaks = highlight_taxa,
    name = "Taxonomy"
  ) +
  scale_color_manual(
    values = highlight_colors,
    breaks = highlight_taxa,
    name = "Taxonomy"
  ) +
  labs(
    title = "Global Distribution of IR-repsonsive Taxa",
    x = "Longitude", y = "Latitude"
  ) +
  cns_theme +
  coord_fixed(1.3) +
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1)))

print(p2)

# 计算每个属的数量
genus_counts <- as.data.frame(table(highlight_data$Genus))
colnames(genus_counts) <- c("Genus", "Count")

# 按数量从大到小排序
genus_counts <- genus_counts[order(genus_counts$Count, decreasing = TRUE), ]
# 将Genus转换为因子，并按数量排序
genus_counts$Genus <- factor(genus_counts$Genus, levels = genus_counts$Genus)

# 创建柱状图
p3 <- ggplot(genus_counts, aes(x = Genus, y = Count, fill = Genus)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = highlight_colors) +
  labs(
    title = "",
    x = "",
    y = "Counts"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 7, color = "black"),
    legend.position = "none"
  )

print(p3)

