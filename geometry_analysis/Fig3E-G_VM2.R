
blk_list = c(1171, 1162, 1193, 1217, 1604, 1246, 1248, 1249, 2636, 1325, 4417)

fb_vm2 = subset(uPN, glomerulus=="glomerulus VM2") %>%
        nlapply(smooth_spikes, blk_list) %>%
        nlapply(resample, 1e3)
        
vm2 = subset(uPN, "51886") %>%
  xform_brain(sample="FAFB13", reference = FCWB) %>%
  mirror_brain(FCWB)

# VM2 PNs skids: 51886, 54072
allpndps=flycircuit::load_si_data('allpndps.rds')
options(nat.default.neuronlist='allpndps')
t1 = nblast_fafb(vm2, db=allpndps) %>%
  summary

# 51886 vs DvGlutMARCM-F002668_seg001
# "DvGlutMARCM-F002629_seg003" "DvGlutMARCM-F002668_seg001" "DvGlutMARCM-F002401_seg001" "DvGlutMARCM-F003228_seg001"

fc_450=read.neurons("~/myscripts/FAFB2017_paper/data/VGlut-F-500450.swc", 
                    neuronnames="DvGlutMARCM-F002668_seg001", 
                    df=subset(allpndps, "DvGlutMARCM-F002668_seg001")[,])

subset(allpndps, rownames(t1)[1:4]) %>%
  mirror_brain(brain=FCWB, transform='flip') %>%
  xform_brain(sample=FCWB, reference=JFRC2) %>% 
plot3d
  
# plot---------
open3d()

t1 = subset(fb_vm2, "51886") %>%
  xform_brain(sample=FAFB13, reference=JFRC2) %>% 
    plot3d(lwd=4, soma=2, col='black')

# npop3d(t2)
# mirror_brain(fc_450, brain=JFRC2, transform='flip') %>% plot3d(lwd=2, soma=2, col='red')

t2 = subset(allpndps, "DvGlutMARCM-F002668_seg001") %>%
  mirror_brain(brain=FCWB, transform='flip') %>%
  xform_brain(sample=FCWB, reference=JFRC2) %>% 
  plot3d(lwd=4, soma=2, col='red')
plot3d(JFRC2.surf, col='grey', alpha=.25)
# rgl.snapshot("170329-fig4f_vm2_fb_lwd4.png", fmt="png")
# rgl.snapshot("170329-fig4f_vm2_j2_lwd4.png", fmt="png")
# rgl.snapshot("170329-fig4f_vm2_fb_j2_lwd4.png", fmt="png")

