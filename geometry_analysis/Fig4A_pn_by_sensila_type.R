
# source("/Users/zhengz11/myscripts/bocklab_git/bocklab/zhihao/r/scripts/170326-process_pn_colors_by_sensilla.R")

pn_list = list()
for (i in seq_along(all_sen)) {
  sen = all_sen[[i]]
  gloms = glom_sen[[sen]]
  pn_list[[i]] = subset(pns, glomerulus %in% gloms)
  pn_list[[i]][,'sensillum'] = rep(sen, length(pn_list[[i]]))
}
PNs = do.call(c, pn_list)
blk_list = c(543, 1171, 1162, 1193, 1217, 1604, 1246, 1248, 1249, 1325, 2636, 2884)

t1 = pns[!(names(pns) %in% names(PNs))]
t1[,'sensillum'] = rep('unknown', length(t1))

all_pns = c(PNs, t1)

color_pal = c(large_basiconic="blue4", thin_basiconic="skyblue1", 
              small_basiconic="royalblue", T1_trichoid="red", 
              T2_trichoid="orangered2", T3_trichoid="darkorange",
              maxillary_palp_basiconic="springgreen1", antennal_coeloconic="yellow3", 
              antennal_intermediate="purple", unknown="deeppink2")

pns_t = nlapply(all_pns, smooth_spikes, blk_list)

plot_pns = pns_t
t_sen = plot_pns[,'sensillum'] %>% unique()
for (i in seq_along(t_sen)) {
  plot3d(plot_pns, sensillum==t_sen[[i]], col = color_pal[[t_sen[[i]]]])
}

rgl.postscript("170519-pn_colored_sensilla_wo_soma.pdf", "pdf")
rgl.snapshot("170519-pn_colored_sensilla_wo_soma.png")
