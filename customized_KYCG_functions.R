library(sesame)
library(shinybusy)
# library(plotly)
library(tidyverse)
library(ggiraph)
library(stringr)
library(tibble)
library(glue)


#### Modified versions of plot functions #################
# defining a slightly modified version of KYCG_plotEnrichAll so that we can tell it to include a tooltip
KYCG_plotEnrichAll_interactive <- function(
    df, fdr_max = 25, n_label = 15, min_estimate = 0) {
  
  gp_size <- sort(table(df$group))
  gp_width <- log(2+gp_size)
  e1 <- df[order(factor(df$group, levels=names(gp_size)), df$dbname),]
  e1$inc <- (gp_width / gp_size)[e1$group]
  e1$inc1 <- c(0,ifelse(e1$group[-1] != e1$group[-nrow(e1)], 1, 0))
  e1$inc2 <- cumsum(e1$inc + e1$inc1)
  
  e1$group <- str_replace(e1$group,"KYCG.","")
  e1$group <- vapply(strsplit(e1$group, "\\."),
                     function(x) paste0(x[2:(length(x)-1)], collapse="."), character(1))
  if ("gene_name" %in% colnames(e1)) {
    e1$dbname[e1$group == "gene"] <- e1$gene_name[e1$group == "gene"] }
  
  e2 <- e1[e1$estimate > min_estimate & e1$FDR < 0.01 ,]
  e2$FDR[e2$FDR < 10**-fdr_max] <- 10**-(fdr_max*1.1)
  
  e3 <- rownames_to_column(as.data.frame(do.call(rbind, lapply(
    split(e1$inc2, e1$group), function(x)
      c(beg=min(x), middle=mean(x), end=max(x))))), "group")
  
  inc2 <- FDR <- estimate <- group <- dbname <- beg <- middle <- NULL
  requireNamespace("ggrepel")
  
  
  # create separate dataframe with rows that have Inf value for estimate column
  # e2_inf <- e2 %>% filter(estimate == Inf)
  # if(nrow(e2_inf) > 0){
  #   #make any special changes to dataframe
  #   #e2_inf$estimate = 30    
  # }
  
  #create tooltip column
  if(nrow(e2)==0){
    e2$tooltip <- character(0)    
  }
  else{
    e2$tooltip <- paste(ifelse(str_detect(e2$group,'\\.gene'),e2$gene_name,e2$dbname),"\n-log10(FDR):",format(round(-log10(e2$FDR) ,digits=4),nsmall=4))
    
  }
  
  if(nrow(e2)>0){
    #remove rows that have Inf value for estimate column
    e2 <- e2 %>% filter(estimate != Inf)    
  }
  
  ggplot(e2, aes(inc2, -log10(FDR))) +
    geom_point_interactive(aes(size=estimate, color=group,tooltip = tooltip),  alpha=0.5) +
    ggrepel::geom_text_repel(data = e2[head(order(e2$FDR), n = n_label),],
                             aes(label=dbname, color=group), size = 3,
                             ## box.padding = unit(0.35, "lines"),
                             ## point.padding = unit(0.3, "lines"),
                             direction="y", nudge_y=0.2, max.overlaps=100) +
    annotate("text", -1, fdr_max*0.96,
             label="Values above this line are capped.",
             hjust=0, vjust=1, color="grey60") +
    geom_hline(yintercept = fdr_max, linetype="dotted", color="grey60") +
    geom_segment(aes(x = beg, y = 0, xend = end, yend = 0, color=group),
                 size=3, data=e3) +
    geom_text(data=e3,aes(middle, -1, label=group, color=group),
              vjust=1, hjust=1, angle=30) + scale_color_discrete(guide="none") +
    ylim(-6, fdr_max*1.2) + xlab("") +
    scale_size_continuous(guide=guide_legend(title="log2(OR)")) +
    coord_cartesian(clip="off") + theme_minimal() +
    theme(text = element_text(family = "rubrik", size = 13),
          axis.title.x = element_blank(),
          #axis.text.x = element_blank(),
          #axis.ticks.x = element_blank(),
          #panel.grid.minor.x = element_blank()
          )
}

preparePlotDF_interactive <- function(df, n, order_by) {
  stopifnot("estimate" %in% colnames(df) && "FDR" %in% colnames(df))
  df <- df[df$nD >0,]
  df$FDR[df$FDR==0] <- .Machine$double.xmin # cap FDR
  ord <- df[[order_by]]
  if (order_by == "estimate") { ord <- -ord; }
  df <- df[order(ord, -df$estimate),]
  df1 <- head(df, n=n)
  
  if ("group" %in% colnames(df1)) {
    gp <- sprintf("%s~", vapply(str_split(
      df1$group, "\\."), function(x) x[3], character(1)))
  } else {
    gp <- ""
  }
  numGroups<- unique(df1$group)
  ## TODO: make everything use dbname
  if ("dbname" %in% colnames(df1)) {
    df1$db1 <- ifelse(str_detect(df1$group,'\\.gene'),df1$gene_name,df1$dbname)
    
    # if more than one group is passed in, the below may be needed, but currently, when deployed, the if block gets 
    # executed even though there's only one group
    #if(numGroups>1){
    #  df1$db1 <- paste0(gp, df1$db1)
    #}
    
    #df1$db1 <- paste0(gp, df1$dbname)
  } else if ("feat" %in% colnames(df1)) { # genome-wide data
    df1$db1 <- paste0(gp, df1$feat)
  }
  df1$db1 <- factor(df1$db1, levels=rev(df1$db1))
  df1
}
KYCG_plotDot_interactive <- function(df, y = "-log10(FDR)",
                                     n = 20, order_by = "FDR",
                                     size_by = "overlap", color_by = "estimate") {
  
  df1 <- preparePlotDF_interactive(df, n, order_by)
  if (y == "-log10(FDR)") {
    df1[["-log10(FDR)"]] <- -log10(df1$FDR)
  }
  ggplot(df1) +
    geom_point_interactive(aes_string("db1", y, size=size_by, color=color_by, tooltip=size_by)) +
    coord_flip() + ggtitle("Enriched Databases") +
    scale_color_gradient(low="blue",high="red") +
    ylab(y) + xlab("") +
    theme(text = element_text(family = "rubrik", size = 13))
  
}