
# source("~/myscripts/bocklab_git/bocklab/zhihao/r/scripts/170321-concentric_Fig4B_FAFB2017.R")

# This one only produces the skeletons of the concentric gloms

# general packages
# library(nat)
library(catmaid)
library(elmr)
library(magrittr)


library(igraph)
library(rgl)
library(grid)
library(dplyr)
library(ggplot2)
library(reshape2)
# neuro

library(caTools)

library(nat.templatebrains)

library(mushroom)

source("~/myscripts/bocklab_git/bocklab/zhihao/r/scripts/170222-gloms_funs_for_EMvsLM_calyx_tightness.R")
load("~/myscripts/data_results/170321-concentric_fig4b_FAFB2017/bb_rca_fb_170321.rda")


bb_rca_fb=bb_rca_fb_170321

# source(getOption('gjanalysissuite.startup'))
fetchn_ca_coll <- function(annos, rs=1e3, bb=bb_rca_fb) {
  fetch_mb_neurons(annos, fafb_conn, "Glomerulus", ref=NULL) %>%
    .[names(.) %>% as.integer %in% pns_skids] %>%
    nlapply(resample, rs) %>%
    nlapply(spine, UseStartPoint=TRUE, invert=TRUE, rval="neuron") %>%
    nlapply(subset, in_bbox(bb))
}

xform_rs_xyz <- function(nl, ref_brain="JFRC2", rs=1) {
  xform_brain(nl, sample=FAFB13, reference=ref_brain) %>% 
    nlapply(resample, rs) %>% 
    lapply(xyzmatrix) %>%
    {do.call(rbind, .)}
}

sm_pj <- function(n, ...) {
  smooth3d(n, nbins=c(50,50,25), sigma2=diag(6, 3), ...) %>%
    im3d %>%
    projection(projdim = "z", projfun = "integrate") %>%
    image(asp=TRUE, col = grey(seq(0, 1, length = 256)))
}

sm_save <- function(n, fname, ...) {
  bb = apply(n[,1:3],2,range)
  bb[1,1:3] = bb[1,1:3] - 10
  bb[2,1:3] = bb[2,1:3] + 10
  smooth3d(n, nbins=c(100,100,50), sigma2=diag(6, 3), bounds=bb) %>%
    im3d %>%
    write.im3d(fname)
}

extract_boutons <- function(n) subset(n, do.call(c, n$AnaSeg))

fetch_boutons <- function(annos, conn=fafb_conn) {
  fetch_mb_neurons(annos, conn, "Glomerulus", ref = NULL, border_tag='bouton border') %>%
    .[names(.) %>% as.integer %in% pns_skids] %>%
    nlapply(extract_boutons) %>%
    nlapply(resample, 1e3)
  #  %>% xform_brain(sample=FAFB13, reference=JFRC2013)
}

sync_sl <- function(n) {
  n$SegList = unlist(n$SubTrees, recursive=FALSE)
  n
}

proj_mt <- function(xyz, ang=-0.4) {
  R=sqrt(xyz[[1]]^2 + xyz[[2]]^2 + xyz[[3]]^2)
  theta = acos(xyz[[3]]/R) + ang
  phi = atan2(xyz[[2]], xyz[[1]])
  n_xyz = c()
  n_xyz[[1]] = R*sin(theta)*cos(phi)
  n_xyz[[2]] = R*sin(theta)*sin(phi)
  n_xyz[[3]] = R*cos(theta)
  n_xyz
}

proj_plane <- function(n) {
  t1 = xyzmatrix(n)
  n$d[,c("X","Y","Z")] = apply(xyzmatrix(n), 1, proj_mt, -0.4) %>% t
  n
}

t3d <- function(n, tm) {
  n$d[,c("X","Y","Z")] = n$d[,c("X","Y","Z")]*tm
  n
}
# t9 = nlapply(outer_j2, t3d, tm)

# 

pns_skids = catmaid_query_by_annotation("^PN$",  type="neuron", conn=fafb_conn)$skid

# gloms that are used---------
inner_gloms = c("glomerulus DA1", "glomerulus DC3", "glomerulus VA1d")
mid_gloms = c("glomerulus DL1", "glomerulus VA6")
outer_gloms = c("glomerulus DM1", "glomerulus VA4", "glomerulus VC1", "glomerulus VM2")

# calyx boutons------------
inner_btns = fetch_boutons(inner_gloms)
mid_btns = fetch_boutons(mid_gloms)
outer_btns = fetch_boutons(outer_gloms)

# plot into 2d--------
bb_rca_j2 = boundingbox(subset(JFRC2NP.surf, "MB_CA_R"))
outer_j2 = xform_brain(outer_btns, sample=FAFB13, reference=JFRC2) %>% nlapply(sync_sl) %>% nlapply(proj_plane)
mid_j2 = xform_brain(mid_btns, sample=FAFB13, reference=JFRC2) %>% nlapply(sync_sl) %>% nlapply(proj_plane)
inner_j2 = xform_brain(inner_btns, sample=FAFB13, reference=JFRC2) %>% nlapply(sync_sl) %>% nlapply(proj_plane)
bb = boundingbox(c(outer_j2, mid_j2, inner_j2))

pdf(file="170404-concentric_Tanaka04_lwd2p5.pdf")
# par(bg = "black")
plot(mid_j2, col = 'blue', lwd=2.5, WithNodes=FALSE, boundingbox=bb, axes = FALSE, ann=FALSE)
plot(inner_j2, col='red', lwd=2.5, WithNodes=FALSE, add=TRUE, axes = FALSE)
plot(outer_j2, col='green', lwd=2.5, WithNodes=FALSE, add=TRUE, axes = FALSE)
dev.off()



pdf(file="170404-concentric_Tanaka04_lwd2_w_ax.pdf")
# par(bg = "black")
plot(mid_j2, col = 'blue', lwd=2, WithNodes=FALSE, boundingbox=bb, axes = FALSE, ann=FALSE)
plot(inner_j2, col='red', lwd=2, WithNodes=FALSE, add=TRUE, axes = FALSE)
plot(outer_j2, col='green', lwd=2, WithNodes=FALSE, add=TRUE, axes = FALSE)
dev.off()

