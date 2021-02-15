# set colors

organ.cols <- setNames(c("#9D0142", "#9D0142", "#a84b99", "#5D4EA2", "#FCB163", "#1B7635", "#4ea7b0", "#d9d9d9"), 
                       c("Intestine", "SI", "Colon", "Stomach", "Lung", "Esophagus", "Liver", "Pancreas"))
blue.cols <- c("#eff3ff", "#bdd7e7", "#6baed6", "#3182bd", "#08519c")

tissue.cols <- setNames(c("#F9C961","#faae65", "#c89721", "#8f6c19", "#227539", "#9288eb", "#5b4e9f", "#9d0142", "#f392be", "#fb75b1", "#a84b99", "#9d0142", "#9F3778", "#7960A9","#4EA7B0","#D9D9D9"),
                        c("Lung-tracheal-epi","Lung-airway-trachea", "Lung-airway", "Lung-distal", "Esophagus", "Stomach-corpus", "Stomach-antrum",
                          "Duodenum", "Jejunum", "Ileum", "Colon", "Proximal-small-intestine", "Small-intestine", "Stomach","Liver", "Pancreas"))
major_cell_type.cols <- setNames(c('#40A190','#F7E59A','#356EAC','#C45B6E','#B182B7','#F692AC','#EBA973'),
                                 c('Mesenchymal','Erythroid',"Immune", "Epithelial", "Endothelial", "Neuronal", "Hepatocyte"))

cell_type.cols <- setNames(c('#F1F1FA','#ECB8AE','#0B3D20','#DF9591','#BF5457','#B281B7','#FDDBCD','#F6E49B',
                             '#CE7473','#EEAB70','#9F1B26','#0F4790','#6F97C2','#1A5130','#649475','#4B7D59',
                             '#9FC2A6','#84AA8B','#C1DCC4','#90171D', '#E2F0DB','#DC2686','#F691AD','#306746',
                             '#3770AA','#ABC2E2','#AE383F'),
                           c("B cell","Basal like","Chondrocyte","Ciliated",                   
                             "Distal lung epithelium","Endothelial","Enteroendocrine","Erythroblast",               
                             "Gastrointestinal epithelium","Hepatocyte","Intestinal epithelium","Macrophage/monocyte 1",      
                             "Macrophage/monocyte 2","Mesenchyme subtype 1","Mesenchyme subtype 2","Mesenchyme subtype 3",       
                             "Mesenchyme subtype 4","Mesenchyme subtype 5","Mesothelial cell","MUC2+ goblet",               
                             "Pericyte","PNS glia","PNS neuron","Proliferative mesenchyme",   
                             "T cell/NK cell 1" ,"T cell/NK cell 2", "Undefined"))

 
age.cols = setNames(viridis(13),c("4","7","8","10","11","12","14","15","17","18","19","21","Adult"))
blue.cols <- c("#eff3ff", "#bdd7e7", "#6baed6", "#3182bd", "#08519c")

colorPal <- grDevices::colorRampPalette(blue.cols)

cellColor <- setNames(adjustcolor(colorPal(30), alpha = .8),seq(1,30,1))


color_list <- list(Organ = setNames(c("#9D0142", "#9D0142", "#a84b99", "#5D4EA2", "#FCB163", "#1B7635", "#4ea7b0", "#d9d9d9"), 
                                          c("Intestine", "SI", "Colon", "Stomach", "Lung", "Esophagus", "Liver", "Pancreas")),
                   Tissue = setNames(c("#F9C961","#faae65", "#c89721", "#8f6c19", "#227539", "#9288eb", "#5b4e9f", "#9d0142", "#f392be", "#fb75b1", "#a84b99", "#9d0142", "#9F3778", "#7960A9","#4EA7B0","#D9D9D9"),
                                           c("Lung-tracheal-epi","Lung-airway-trachea", "Lung-airway", "Lung-distal", "Esophagus", "Stomach-corpus", "Stomach-antrum",
                                             "Duodenum", "Jejunum", "Ileum", "Colon", "Proximal-small-intestine", "Small-intestine", "Stomach","Liver", "Pancreas")),
                   # Age = setNames(c("#440154FF","#482576FF","#414487FF","#35608DFF","#2A788EFF","#21908CFF","#22A884FF","#43BF71FF","#7AD151FF","#BBDF27FF","#FDE725FF"),
                   #              c('7','8','10','11', '12', '14', '15', '17', '18', '19', '21')),
                   Age_week = setNames(viridis(13),c("4","7","8","10","11","12","14","15","17","18","19","21","Adult")),
                   Major_cell_type = setNames(c('#40A190','#F7E59A','#356EAC','#C45B6E','#B182B7','#F692AC','#EBA973'),
                                            c('Mesenchymal','Erythroid',"Immune", "Epithelial", "Endothelial", "Neuronal", "Hepatocyte")))





# plotting functions


prepareTreeAndHeatmapInput <- function(expr.mat = hio.expr, cor.method = "pearson", hc.method = "average", norm.method = "quantile", 
                                       genes.to.highlight = NULL, column.reorder = TRUE, row.reorder = TRUE){
  hc.col <- hclust(as.dist(1 - cor(expr.mat, method = cor.method)), method = hc.method)
  col.orders <- hc.col$labels[hc.col$order]
  hc.row <- hclust(as.dist(1 - cor(t(expr.mat), method = cor.method)), method = hc.method)
  row.orders <- hc.row$labels[hc.row$order]
  
  if (column.reorder & row.reorder) {
    expr.mat <- expr.mat[row.orders, col.orders]
    max.ct <- colnames(expr.mat)[apply(expr.mat, 1, which.max)]
    idx <- unlist(lapply(colnames(expr.mat), function(x){
      which(max.ct == x)
    }))
    expr.mat <- expr.mat[idx,]
  } else if (!column.reorder & row.reorder) {
    original.col.names <- colnames(expr.mat)
    expr.mat <- expr.mat[row.orders, col.orders]
    max.ct <- colnames(expr.mat)[apply(expr.mat, 1, which.max)]
    idx <- unlist(lapply(colnames(expr.mat), function(x){
      which(max.ct == x)
    }))
    expr.mat <- expr.mat[idx,]
    expr.mat <- expr.mat[,original.col.names]
  }
  if (norm.method == "quantile") {
    normed.expr <- t(apply(expr.mat, 1, function(vec){(vec - min(vec))/(max(vec) - min(vec))}))
  }else if (norm.method == "scale") {
    normed.expr <- t(scale(t(expr.mat)))
  }else if (is.null(norm.method)) {
    normed.expr <- expr.mat
  }
  
  highlight.idx <- ifelse(rownames(normed.expr) %in% genes.to.highlight, 1, NA)
  if (sum(is.na(highlight.idx)) == nrow(normed.expr)) {
    input <- normed.expr
    cat("No highlighted items\n")
  }else{
    input <- cbind(normed.expr, highlight.idx)
    cat("Highlight index added\n")
  }
  
  res <- list("heatmap_input" = input,
              "hc_row" = hc.row,
              "hc_col" = hc.col,
              "highlighted_genes" = intersect(rownames(normed.expr), genes.to.highlight))
  return(res)
}

edit_label <- function(x){
  label <-  strsplit(x,'/>')
  return(label[[1]][2] %>% str_remove('<br '))
}   

remove_labels <- function(p){
  entries <- p$x$data %>% length()
  for (i in c(1:entries)) {
    if (p$x$data[i][[1]]$yaxis %in% c('y2','y3')) {
      if (!is.null(p$x$data[i][[1]]$text)) {
        if (!is.matrix(p$x$data[i][[1]]$text)) {
          p$x$data[i][[1]]$text <- edit_label(p$x$data[i][[1]]$text)
        }
      }
    }
  }
  return(p)
}

set_bivariate_pal <- function(scale){
  bivariates_1 <- expand.grid(x = seq(0, 1, 0.01), y = seq(0, 1, 0.01)) %>%
    mutate(fill_val = atan(y/x),
           transparency = x + y)
  bivariates_2 <- ggplot(bivariates_1, aes(x, y, fill = fill_val)) +
    geom_tile() +
    scale_fill_viridis_c(option = scale, na.value = 'grey70') +
    theme_void() +
    theme(legend.position = "none")
  
  bivariate_pal <- ggplot_build(bivariates_2)$data[[1]] %>%
    select(x, y, fill) %>%
    mutate(x = as.character(x),
           y = as.character(y))
  return(bivariate_pal)
}


color_pals <- list(A = viridis(30, option = 'A'),
                   B = viridis(30, option = 'B'),
                   C = viridis(30, option = 'C'),
                   D = viridis(30, option = 'D'),
                   blues = colorPal(30))

plot_umap_preview <- function(df){
  ggplot(df, aes(x = UMAP_X, y = UMAP_Y)) +
    theme_void() +
    coord_fixed() +
    theme(plot.title = element_text(hjust = 0.5),
          aspect.ratio = 1) +
    geom_point(size = .25, col = 'grey70')
}

plot_umap <- function(df, mapping, main=NULL, legend.title=NULL, bivariate_pal=NULL, color_pal='blues', point.size=.25, filter=NULL) {
 
  cell_type.cols <- df %>% distinct(Cell_type_color, Cell_type) %>%
    select(Cell_type, Cell_type_color) %>% deframe()
  if (!is.null(filter)) {
   df <-  highlight_data(df,str_to_sentence(mapping),filter)
  }
  df <- df %>%
    mutate(Age_week = factor(Age_week,
                             levels = rev(c("4","7","8","10","11","12","14","15","17","18","19","21","Adult"))))
  if (mapping == 'bivariate') {
    df <- df %>%
      mutate(x = as.character(round(scales::rescale(receptor), 2)),
             y = as.character(round(scales::rescale(ligand), 2))) %>%
      left_join(bivariate_pal,by = c("x", "y")) 
    umap <- ggplot(df, aes(x = UMAP_X, y = UMAP_Y)) +
      theme_void() +
      coord_fixed() +
      theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1) + 
      geom_point(size = point.size, aes(col = fill)) +
      scale_color_identity() +
      guides(color = "none") +
      ggtitle(main)
    return(umap)
  } else {
    p <- ggplot(df, aes(x = UMAP_X, y = UMAP_Y)) +
      theme_void() +
      coord_fixed() +
      theme(plot.title = element_text(hjust = 0.5),
            aspect.ratio = 1) +
      ggtitle(main)
    if (mapping == 'gene') {
      p <- p + geom_point(size = point.size, aes(col = gene)) +
        scale_color_gradientn(colours = adjustcolor(color_pals[[color_pal]], alpha = .8)) +
        labs(col = legend.title)
    }
    if (mapping == 'organ') {
      p <- p + geom_point(size = point.size, aes(col = Organ)) +
        scale_color_manual(values = organ.cols, na.value = 'grey70') +
        labs(col = 'Organ') +
        guides(col = guide_legend(override.aes = list(size = 5)))  
    }
    if (mapping == 'tissue') {
      p <- p + geom_point(size = point.size, aes(col = Tissue)) +
        scale_color_manual(values = tissue.cols, na.value = 'grey70') +
        labs(col = 'Tissue') +
        guides(col = guide_legend(override.aes = list(size = 5)))
    }
    if (mapping == 'cell_type') {
      
      p <- p + geom_point(size = point.size, aes(col = Cell_type)) +
        scale_color_manual(values = cell_type.cols, na.value = 'grey70') +
        labs(col = 'Cell Type') +
        guides(col = guide_legend(override.aes = list(size = 5)))
    }
    if (mapping == 'major_cell_type') {
      p <- p + geom_point(size = point.size, aes(col = Major_cell_type)) +
        labs(col = 'Major Cell Type') +
        scale_color_manual(values = major_cell_type.cols, na.value = 'grey70') +
        guides(col = guide_legend(override.aes = list(size = 5)))
    }
    if (mapping == 'age_week') {
      p <- p + geom_point(size = point.size, aes(col = Age_week)) +
        scale_color_manual(values = age.cols, na.value = 'grey70') +
        labs(color = 'Age[weeks]') +
        guides(col = guide_legend(override.aes = list(size = 5)))
    }
    if (mapping == 'cluster') {
      cluster.cols <- df %>% distinct(cluster_col,sorted_cluster_idx) %>% 
        select(sorted_cluster_idx,cluster_col) %>% deframe()
      p <- p + geom_point(size = point.size, aes(col = as.factor(sorted_cluster_idx))) +
        scale_color_manual(values = cluster.cols, na.value = 'grey70') +
        labs(col = 'Cluster') +
        guides(col = guide_legend(override.aes = list(size = 5)))
    }
    
  }
  return(p)
}

plot_legend <- function(bivariate_pal){
  legend <- ggplot(bivariate_pal, aes(x, y)) +
    geom_tile(aes(fill = fill)) +
    scale_fill_identity() +
    labs(x = "Receptor",
         y = "Ligand") +
    theme_void() +
    theme(axis.title = element_text(size = 10),
          axis.title.y = element_text(angle = 90)) +
    coord_fixed()
  return(legend)
}


plot_umap_3d <- function(seu_obj,meta=NULL, gene=NULL) {
  if (meta == 'gene') {
   p <-  plot_ly(seu_obj$meta.data,
            x = ~UMAPCSS3D_1,
            y = ~UMAPCSS3D_2,
            z = ~UMAPCSS3D_3,
            size = 1, name = gene,
            color = seu_obj$normed.expr[gene,],
            colors = blue.cols,
            sizes = c(10, 200),
            type = 'scatter3d',
            mode = 'markers') %>%
      layout(title = list(text = gene, y = 0.95))
  }
  if (meta == 'cell_type') {
   p <-  plot_ly(seu_obj$meta.data,
            x = ~UMAPCSS3D_1,
            y = ~UMAPCSS3D_2,
            z = ~UMAPCSS3D_3,
            size = 1, 
            color = ~Cell_type,
            colors = ~Cell_type_color,
            sizes = c(10, 200),
            type = 'scatter3d',
            mode = 'markers') %>%
      layout(title = list(text = 'Cell type', y = 0.95))
  }
  if (meta == 'age') {
   p <-  plot_ly(seu_obj$meta.data,
            x = ~UMAPCSS3D_1,
            y = ~UMAPCSS3D_2,
            z = ~UMAPCSS3D_3,
            size = 1,
            color = ~Age_week,
            colorscale = 'Viridis',
            sizes = c(10, 200),
            type = 'scatter3d',
            mode = 'markers') %>%
      layout(title = list(text = 'Age', y = 0.95))
  }
  return(p)
}


summarize_dataset <- function(seu_obj){
  n_genes <- seu_obj$normed.expr %>% row.names() %>% length()
  n_cells <- seu_obj$normed.expr %>% colnames() %>% length()
  paste('The selected dataset contains', n_cells , 'cells with expression data for', n_genes, 'genes.')
}



plot_distributions <- function(df,
                               type=NULL,
                               mapping=NULL,
                               group=NULL,
                               gene=NULL,
                               gene.clip=FALSE,
                               palette=NULL, filter=NULL){
  
  df <- df %>% na.omit()
  
  if (isTruthy(group == 'Age')) {
    df <- df %>% mutate(Age = as.factor(Age_week))
  }
  
  if (!is.null(gene) & gene.clip == TRUE) {
    df <- df %>% filter(gene > 0)
  }
  if (type == 'histogram') {
    x <- df %>% select_(mapping) %>% pull()
    breaks <- pretty(range(x), n = nclass.FD(x), min.n = 1)
    bwidth <- breaks[2] - breaks[1]
    if (is.null(gene)) {
      p <- ggplot(df) + 
        geom_histogram(aes_string(x = mapping), binwidth = bwidth) +
        theme_article() +
        ylab("") 
    } else {
      p <- ggplot(df) +
        ylab("")  +
        theme_article() +
        theme(legend.position = "none") +
        xlab(gene) 
      if (!is.null(palette)) {
        p <- p +
          geom_histogram(aes_string(x = mapping, fill = "..x.."),  binwidth = bwidth) +
          scale_fill_gradientn(" ", colours = adjustcolor(color_pals[[palette]], alpha = .95))
      } else {
        p <- p + geom_histogram(aes_string(x = mapping), binwidth = bwidth) 
      }
    }
    return(p)
  }
  if (type == 'violine') {
    if (group == 'Cell_type') {
      color_list <- list(Cell_type = df %>% distinct(Cell_type_color, Cell_type) %>%
                           select(Cell_type, Cell_type_color) %>% deframe())
    }
   p <-  ggplot(df) +
      geom_violin(aes_string(x = group, y = 'gene', fill = group)) +
      theme_article() +
      scale_fill_manual(values = color_list[[group]], na.value = 'grey50') +
     ylab(gene) +
     xlab(group %>% str_replace_all('_',' ')) +
     theme(legend.position = "none",
           axis.text.x = element_text(angle = 45, hjust = 1))
   return(p)
  }
  if (type == 'boxplot') {
    p <-  ggplot(df) +
      geom_boxplot(aes(x = Organ, y = gene, fill = Organ)) +
      theme_article() +
      scale_fill_manual(values = organ.cols, na.value = 'grey50') +
      ylab(gene)
    return(p)
  }
  if (type == 'barplot') {
    if (group == 'Cell_type') {
      color_list <- list(Cell_type = df %>% distinct(Cell_type_color, Cell_type) %>%
        select(Cell_type, Cell_type_color) %>% deframe())
    }
    
    p <-  ggplot(df) +
      geom_bar(aes_string(y = group, group = group, fill = group)) +
      theme_article() +
      scale_fill_manual(values = color_list[[group]], na.value = 'grey50') +
      ylab('') +
      xlab('') +
      theme(legend.position = "none")
    return(p)
  }
}

sort.genes <- function(x){
  sorted <- x %>% sort()
  numerics <- sorted %>% str_detect('^[[:digit:]]')
  return(c(sorted[!numerics],sorted[numerics]))
}


label_plot <- function(x){
  if (x$annotation != 'gene') {
    return(x$annotation %>% str_to_sentence() %>% str_replace_all('_',' '))
  } else {
    return(x$gene)
  }
}

label_inspector <- function(x){
  if (x$annotation %in% c('bivariate','ligand','receptor')) {
    interaction <- x$pair
    receptor <- interaction %>% filter(Interaction == 'Receptor') %>% pull(Gene)
    ligand <- interaction %>% filter(Interaction == 'Ligand') %>% pull(Gene)
    if (x$annotation == 'bivariate') {
      return(paste(ligand, receptor, sep = '-'))
    }
    if (x$annotation == 'receptor') {
      return(paste(receptor))
    }
    if (x$annotation == 'ligand') {
      return(paste(ligand))
    }
  } else {
    return(paste(x$annotation %>% str_to_sentence() %>% str_replace_all('_',' ')))
  }
  
}

prepare_umaps <- function(umaps, windows){
  plots <- reactiveValuesToList(umaps)
  plots <- plots[order(names(plots))]
  return(plots[windows])
}

sample_dataset <- function(seu_obj, performance, sampling_rate, gene=NULL){
  set.seed(19190)
  if (is.null(gene)) {
    if (performance == TRUE) {
      return(seu_obj$meta.data %>% sample_frac(sampling_rate))
    } else {
      return(seu_obj$meta.data)
    }
  } else {
    if (performance == TRUE) {
      return(seu_obj$meta.data %>% sample_frac(sampling_rate) %>% mutate(gene = seu_obj$normed.expr[gene,cells]) %>% arrange(gene))
    } else {
      return(seu_obj$meta.data %>% mutate(gene = seu_obj$normed.expr[gene,]) %>% arrange(gene))
    }
  }
}


sample_dataset_bivariate <- function(seu_obj, performance, sampling_rate, genes=NULL){
  set.seed(19190)
  if (is.null(genes)) {
    if (performance == TRUE) {
      return(seu_obj$meta.data %>% sample_frac(sampling_rate))
    } else {
      return(seu_obj$meta.data)
    }
  } else {
    if (performance == TRUE) {
      return(seu_obj$meta.data %>% 
               sample_frac(sampling_rate) %>%
               mutate(ligand = seu_obj$normed.expr[genes[1],cells],
                      receptor = seu_obj$normed.expr[genes[2],cells]) %>%
               arrange(ligand + receptor))
    } else {
      return(seu_obj$meta.data %>%
               mutate(ligand = seu_obj$normed.expr[genes[1],],
                      receptor = seu_obj$normed.expr[genes[2],]) %>%
               arrange(ligand + receptor))
    }
  }
}

add_cell_id <- function(x){
  x$meta.data  <-  x$meta.data %>% as_tibble(rownames = 'cells')
  return(x)
}

bivariate_distribution <- function(df, group, genes, gene.clip=FALSE, scale='D', rows=3, filter=NULL) {
  if (group != 'All') {
    if (isTruthy(!is.null(filter))) {
      df <-  df %>% filter(.data[[group]] %in% filter)
    }
    df <- df %>%
      select(c(group, 'ligand', 'receptor')) %>%
      gather(type, value, -c(group)) %>%
      mutate(type = str_to_sentence(type))
  } else {
    df <- df %>%
      select(c('ligand', 'receptor')) %>%
      gather(type, value) %>%
      mutate(type = str_to_sentence(type))
  }
  
  
  if (gene.clip == TRUE) {
    df <- df %>% filter(value > 0)
  }
  x <- df %>% select(value) %>% pull()
  breaks <- pretty(range(x), n = nclass.FD(x), min.n = 1)
  bwidth <- breaks[2] - breaks[1]
 
  p <- ggplot(df, aes(x = value, group = type, fill = type)) +
    geom_histogram(position = 'dodge', binwidth = bwidth) +
    scale_fill_viridis_d(option = scale, direction = -1, labels = genes) +
    labs(fill = NULL,
         y = NULL,
         x = 'Normalized Expression') +
    theme(legend.title = element_blank()) +
    theme_article()
  
  if (group != 'All') {
    p <-  p + facet_wrap(group, nrow = rows)
  } 
  return(p)
}

scale_by_row <- function(input.norm) {
  scaled <- t(apply(input.norm, 1, rescale))
  return(scaled)
}
scale_by_col <- function(input.norm) {
  scaled <- apply(input.norm, 2, rescale)
  return(scaled)
}

distinct_genes <- function(input.norm){
  interactions <- input.norm %>% row.names()
  genes <- tibble(interaction = interactions) %>%
    separate(interaction, c('ligand','receptor'), sep = '_') %>%
    mutate(ligand = str_remove(ligand,'L:'), receptor = str_remove(receptor,'R:')) %>%
    gather(type, gene) %>% distinct(gene) %>% pull() 
  return(genes)
}

filter_heatmap <- function(input.norm, genes){
  interactions <- input.norm %>% row.names()
  selected <- str_detect(interactions,paste(genes,collapse = '|'))
  return(input.norm[selected,])
}

highlight_data <- function(df,group,filter){
  df <- df %>%  mutate('{group}' := ifelse(.data[[group]] %in% filter,.data[[group]],NA)) 
  df_2 <- df %>% filter(is.na(.data[[group]]))
  df_3 <- df %>% filter(!is.na(.data[[group]]))
  return(bind_rows(df_2,df_3))
}


load_data <- function(file){
  file <-  readRDS(file) 
  if (file$meta.data %>% colnames() %in% 'cells' %>% any()) {
    return(file)
  } else {
    file$meta.data <- file$meta.data %>% as_tibble(rownames = 'cells')
    return(file)
  }
}

all_genes <- readRDS('all_genes.rds')
