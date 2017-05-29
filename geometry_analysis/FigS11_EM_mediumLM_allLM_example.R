
# The LM DA3 is excluded
# Assume VM7 in MyNeurons is VM7d, since 1 is VM7v
# VA3 and VM1 have only 1 neuron in MyNeurons


# all gloms ranked in std_glom : 
all_glom_rank = c("DL2d", "DA2", "VA1v", "VM7v", "DL1", "DM2", 
  "VM7d", "DM6", "DM5", "VM5d", "DA1", "VA7m", "VA1d", "D", 
  "DC3", "VC4", "DL2v", "VM2", "VM3", "DC2", "VM5v", "VM1", 
  "DL3", "VC3m", "DA3", "V", "VA3", "VA5", "VL1")

gj_glom_rank = all_glom_rank[all_glom_rank %in% unique(gj_coll_0223[,'std_glom'])]

if (FALSE) {
  # the scripts to generate fb_ca_skln_170403, gj_ca_skln_170403 just in case
  fb_ca_skln = xform_brain(uPN, sample=FAFB13, reference=FCWB) %>%
    mirror_brain(FCWB) %>%
    get_calyx_skeletons
  
  fb_ca_skln[,'std_glom']=glom_data[fb_ca_skln[,'glomerulus']]
  
  # Jefferis 07 PNs
  load("~/myscripts/git_clone/r/gj/AnalysisSuite/R/Data/MyNeurons.rda")
  source("~/myscripts/bocklab_git/bocklab/zhihao/r/scripts/170220-clean_up_TKA7R_from_MyNeurons.R")
  gj_ca_skln = subset(MyNeurons, Glomerulus %in% gj_gloms) %>%
    xform_brain(sample="Cell07", reference=FCWB) %>%
    mirror_brain(brain=FCWB, transform='flip') %>%
    get_calyx_skeletons
}

which.median <- function(x) which.min(abs(x - median(x)))

plot_glom <- function(nl, p_fix="", to_save=TRUE, ...) {
  # supply ylim=c(24e4, 2e5) if needed
  if (to_save) pdf(paste0(format(Sys.Date(), format="%y%m%d"), "-2dSk_", p_fix, '.pdf'))
  plot(nl, WithNode=FALSE, xlab="", ylab="", axes=FALSE, lwd=1, col=my_pal(length(nl)), ...)
  if (to_save) dev.off()
} 
  

fb_nl = fb_ca_skln_170403
gj_nl = gj_ca_skln_170403

gj_nl[,'std_glom']=glom_data[gj_nl[,'Glomerulus'] %>% as.character]

gj_tbl = summarize_pair_wise(gj_nl, gj_std_gloms, 'Jefferis07', get_dist_summary)
results = gj_tbl

# pdf("170403-EMvsLMvsMedian_first8.pdf", width = 8, height = 12)
pdf("170403-EMvsLMvsMedian_last8.pdf", width = 8, height = 12)
n_row = 8
par(mfrow = c(n_row, 3), mar=c(0,2.2,2,0), oma=c(0,2.2,2,0))

# gloms = gj_glom_rank[1:8]
gloms = gj_glom_rank[9:16]
for (i in seq_along(gloms))  {

glom = gloms[[i]]
subset(fb_nl, std_glom==glom) %>%
  plot_glom(to_save=FALSE)

mtext(glom, side=2, line=2, outer=TRUE, adj = 0, at=(1 - 1/16 - (1/8)*(i-1)), las=2)
  
filter(results, type==glom, groups=='Jefferis07') %>% 
  .[which.median(.$dist_mean), c("neuron1", "neuron2")] %>% 
  sapply(as.character) %>%
  {plot_glom(gj_nl[.], to_save=FALSE)}

subset(gj_nl, std_glom==glom) %>%
  plot_glom(to_save=FALSE)
}

mtext("EM", side=3, outer=TRUE, at=1/6)
mtext("median distance LM", side=3, outer=TRUE, at=1/6 + 1/3)
mtext("all LM", side=3, outer=TRUE, at=1/6 + 2/3)

dev.off()