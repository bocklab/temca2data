
# Flycircuit data - "fc_pns0219"
# load("~/myscripts/FAFB2017_paper/data/fc_pns0219.rda")
# 
# Jefferis07 and part of Flycircuit data - allpns.FCWB.rda
# #  load("~/myscripts/FAFB2017_paper/data/allpns.FCWB.rda")

library(igraph)
library(grid)
library(caTools)

ca_2d_plot <- function(nl) {
  plot(nl, WithNode=FALSE, xlab="", ylab="", axes=FALSE, lwd=3, col=my_pal(length(nl)))
}

all_3d_plot <- function(nl) {
  plot3d(nl, lwd=4, col=my_pal(length(nl)), soma=2)
  plot3d(JFRC2NP.surf, "MB_CA_R", col='grey', alpha=.25)
}

gj_pns = allpns.FCWB[!names(allpns.FCWB) %in% "JBC4L"]

subset_xform_ca <- function(glom_name="DM2") {
  gj = subset(gj_pns, Glomerulus==glom_name) %>%
    mirror_brain(brain=FCWB, transform='flip')
  fc = subset(fc_pns0219, glom==glom_name)

  xform_brain(c(gj, fc), sample=FCWB, reference=JFRC2) %>% 
    nlapply(subset, in_j2_rca)
}

### MZ19 in FAFB (1st column, 2nd column)

# DA1----------
da1_black_list = c(1249, 1325, 2636, 2884)
  
da1 = subset(uPN, glomerulus=="glomerulus DA1")

da1_sm = nlapply(da1, smooth_spikes, da1_black_list) %>% 
  xform_brain(sample='FAFB13', reference=JFRC2)

# DC3---------
dc3 = subset(uPN, glomerulus=="glomerulus DC3")

dc3_sm = nlapply(dc3, smooth_spikes, da1_black_list) %>% 
  xform_brain(sample='FAFB13', reference=JFRC2)

# plot3d(dc3_sm)

# VA1d------------
va1d = subset(uPN, glomerulus=="glomerulus VA1d")

va1d_sm = nlapply(va1d, smooth_spikes, da1_black_list) %>% 
  xform_brain(sample='FAFB13', reference=JFRC2)

# plot3d(va1d_sm)

# mz19 in total-------
fb_j2_mz19 = c(da1_sm, dc3_sm, va1d_sm)

# plotting mz19 (1st row)----------
open3d()
all_3d_plot(fb_j2_mz19)
rgl.snapshot("170418-EM_mz19_fig5A_FAFB17.png")

# plotting mz19 (2nd row)-------
bb_rca_j2 = boundingbox(subset(JFRC2NP.surf, "MB_CA_R"))
bb_rca_j2[1,1] = 205
bb_rca_j2[2,1] = 272
in_j2_rca = in_bbox(bb_rca_j2)

em_mz19_plot = nlapply(fb_j2_mz19, subset, in_j2_rca)
pdf("170418-EM_mz19_ca_fig5A_FAFB17.pdf")
ca_2d_plot(em_mz19_plot)
dev.off()

### MZ19 in LM data
# DA1 - Flycircuit, Jefferis07(11)
# DC3 - Jefferis07(4)
# VA1d - Flycicuit(3), Jefferis07(11)
# Flycircuit data - "fc_pns0219"
# load("~/myscripts/FAFB2017_paper/data/fc_pns0219.rda")
# 
# Jefferis07 data - allpns.FCWB.rda
# #  load("~/myscripts/FAFB2017_paper/data/allpns.FCWB.rda")

gj_mz19 = subset(allpns.FCWB, Glomerulus %in% c("DA1", "DC3", "VA1d")) %>%
  mirror_brain(brain=FCWB, transform='flip')
fc_mz19 = subset(fc_pns0219, glom %in% c("DA1","VA1d"))

lm_mz19 = c(gj_mz19, fc_mz19) %>% 
  xform_brain(sample=FCWB, reference=JFRC2) %>% 
  nlapply(subset, in_j2_rca)

pdf("170418-LM_mz19_ca_fig5A_FAFB17.pdf")
ca_2d_plot(lm_mz19)
dev.off()
### DM2---------
# EM plot------
dm2_black_list = c(1217, 1162, 1604, 2636, 1246, 1248, 1249)


dm2 = subset(uPN, glomerulus=="glomerulus DM2")

dm2_sm = nlapply(dm2, smooth_spikes, dm2_black_list) %>% 
  xform_brain(sample=FAFB13, reference='JFRC2')

# DM2 EM whole PNs---
open3d()
all_3d_plot(dm2_sm)
rgl.snapshot("170418-EM_dm2_fig5A_FAFB17.png")

# DM2 EM calyx---
em_dm2_plot = nlapply(dm2_sm, subset, in_j2_rca)
pdf("170418-EM_dm2_ca_fig5A_FAFB17.pdf")
ca_2d_plot(em_dm2_plot)
dev.off()

# DM2 LM calyx---
lm_dm2 = subset_xform_ca(glom_name="DM2")
pdf("170418-LM_dm2_ca_fig5A_FAFB17.pdf")
ca_2d_plot(lm_dm2)
dev.off()

### VM2----------
vm2_black_list = c(1171, 1162, 1193, 1217, 1325, 1604, 1246, 1248, 1249, 2636)

vm2 = subset(uPN, glomerulus=="glomerulus VM2")

vm2_sm = nlapply(vm2, smooth_spikes, vm2_black_list) %>% 
  xform_brain(sample=FAFB13, reference='JFRC2')

# VM2 EM whole PNs---
open3d()
all_3d_plot(vm2_sm)
rgl.snapshot("170418-EM_vm2_fig5A_FAFB17.png")

# VM2 EM calyx---
em_vm2_plot = nlapply(vm2_sm, subset, in_j2_rca)
pdf("170418-EM_vm2_ca_fig5A_FAFB17.pdf")
ca_2d_plot(em_vm2_plot)
dev.off()

# VM2 LM calyx---
lm_vm2 = subset_xform_ca(glom_name="VM2")
pdf("170418-LM_vm2_ca_fig5A_FAFB17.pdf")
ca_2d_plot(lm_vm2)
dev.off()
