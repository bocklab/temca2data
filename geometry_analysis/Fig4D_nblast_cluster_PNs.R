
# original script: 170322-nblast_cluster_PN_fig4d_FAFB17.R

# source("~/myscripts/bocklab_git/bocklab/zhihao/r/scripts/170213-concentric_territoriesz.R")


# general packages
library(magrittr)

library(igraph)
library(rgl)
# library(grid)
library(dplyr)
library(ggplot2)
library(reshape2)
# library(Heatplus)
# neuro

# library(caTools)
library(nat.templatebrains)
library(nat.nblast)
library(dendroextras)
library(dendextend)

pndps = dotprops(uPN, k=5, resample=1e3) %>%
  xform_brain(sample=FAFB13, ref=JFRC2)

pndps[,'glom'] = gsub("glomerulus ", "", pndps[,'glomerulus'])  

# allbyall for all PNs
pn.aba = nblast_allbyall(pndps, .progress='text')

pnhc=nhclust(scoremat=pn.aba)

# pdf("170322-PN_glom_nhcluster_FAFB2017.pdf", width=20, height=12)
# plot(pnhc, labels = pndps[,'glom'])
# dev.off()

# produce sensilla category to prepare color-----------

all_sen = catmaid_query_by_annotation("^sensilla_type$",  type="annotation", conn=fafb_conn)$name

color_pal = c(large_basiconic="blue4", thin_basiconic="skyblue1", 
              small_basiconic="royalblue", T1_trichoid="red", 
              T2_trichoid="orangered2", T3_trichoid="darkorange",
              maxillary_palp_basiconic="springgreen1", antennal_coeloconic="yellow3", 
              antennal_intermediate="purple", unknown="deeppink2")

pn_colors = list()
for (i in all_sen) {
  gloms = catmaid_query_by_annotation(paste0("^", i, "$"),  type="annotation", conn=fafb_conn)$name
  
  skids = lapply(gloms, function(x) paste0("^", x, "$") %>% 
                   catmaid_query_by_annotation(type="neuron", conn=fafb_conn) %>% 
                   .$skid %>%
                   intersect(pn_skids)) %>%
         {do.call(c, .)}
  
  pn_colors = rep(i, length(skids)) %>%
    setNames(unname(skids)) %>%
    c(pn_colors)
}

pn_colors = setdiff(names(pndps), names(pn_colors)) %>% 
  {setNames(rep("unknown",length(.)), .)} %>%
  c(pn_colors)

# function------
height_for_ngroups<-function(hc, k) {
  s=sort(hc$height, decreasing=TRUE)
  s[k]-1e-6
}

# plotting the dendrogram----------
hc_plot = pnhc
hc_col = hc_plot$order %>% {hc_plot$labels[.]} %>% {pn_colors[.]} %>% unlist %>% {color_pal[.]}
hc_plot$height=hc_plot$height %>% sqrt
t3 = colour_clusters(hc_plot, k=length(pndps), col=unname(hc_col)) %>%
  color_labels(k=length(pndps), col=unname(hc_col)) %>%
  set("branches_lwd", 4)

labels(t3) = pndps[,'glom'][hc_plot$order]
plot(t3)

# pdf("170327-PN_glom_nhcluster_FAFB2017.pdf", width=20, height=6)
# plot(t3, ylab='Height')
# axis(2, lwd = 4)
# dev.off()


