#### Begin of test ####

# working directory
# wd <- "S:/PTR-MS/Magali Proffit/Candice/Lavandes2021/sem23_48h_3series/3x4chb/analyse reduite"
wd <- "C:/Users/huguenin/Documents/R/provoc2/data test/miscalenous" # sans "/" final
#wd <- "S:/PTR-MS/Laurent Dormont/Caroline/seq_cancer"
setwd(wd)

# import
sp <- import.h5(wd)

# meta
sp <- import.meta("meta_1")

# workflow
saveRDS(sp$workflow, "workflow.rds")
wf <- readRDS("workflow.rds")
#   launch.workflow <- function(L){
#   for(i in 1:length(L$workflow)){
#     do.call(names(L$workflow),c(L$workflow$sample))
#   }
# }
# fmr <- lapply(sp$workflow,names)

# time gestion
sp <- re.calc.T.para(sp)
sp <- re.init.T.para(sp)

# spectra plot
# a dynamic plot :
dy.spectra(sel_sp = sp$mt$meta[sp$acq,"end"], new_color = FALSE)
# a standart plot :
fx.spectra(sel_sp = sp$mt$meta[sp$acq,"end"], pkm = 137, pkM = 137, leg = "l")
fx.spectra(sel_sp = 1, pkm = 59, pkM = 150)

# kinetic plot
monitor_plot_AUC(M_num = c(69.055, 205.158, 157.021),
                 each_mass = FALSE,
                 group = "grp1",
                 graph_type = "dy",
                 L = sp,
                 Y_exp = FALSE,
                 time_format = "date")

# Liste des variables :
# M_num         les masses analysees. M.Z(c(69, 205, 157)) ou c(69.055, 205.158, 157.021)
# each_mass     un graphe pour chaque masse ou non. Logical TRUE or FALSE
# group         groupe avec le nom de la colonne en argument. Ex : "grp1". Or FALSE
# graph_type    choisi soit des graphe fixe "fx" au format tiff soit des graphe dynamique "dy" au format html
# Y_exp         L'ordonnée exponentielle. Logical. TRUE or FALSE
# time_format   L'abscisse est une duree ("time") ou une date ("date").

# analyse

search.peak <- function(mp = 137, L = sp){
  pkl <- colnames(L$peaks)
  pk_mean <- colMeans(L$peaks)
  fmr <- which((pkl > (mp-0.5))&(pkl < (mp+0.5)))
  pk_mat <- names(pk_mean)[fmr] %>% as.numeric() %>% rbind(pk_mean[fmr],fmr)
  row.names(pk_mat) <- c("mass","mean intensity", "index")
  colnames(pk_mat) <- rep(" ",ncol(pk_mat))
  return(pk_mat)
}



#### PCA
PCA_plot_scores <- function(MSpca,PCx,PCy,L){

  ax.pca <- c(PCx, PCy)

  vnames <- L$names_acq[L$Sacq]
  if(length(vnames) > 5){
    l_ech <- length(vnames)
    titre <- c("PCA/PC",ax.pca[1],ax.pca[2],"of",vnames[1],"to",vnames[l_ech]) %>% str_flatten("_") %>% paste0(".tiff")
    legende <- c(vnames[1:3], vnames[(l_ech-2):l_ech])
    p_ch <- c(NA, rep(16,6))
    col_l <- c(NA, alpha(MSpca$mt_col[c(1:3,(l_ech-2):l_ech)],0.5))
  }else{
    titre <- c("PCA/PC",ax.pca[1],ax.pca[2],"of",vnames) %>% str_flatten("_") %>% paste0(".tiff")
    legende <- c(vnames)
    p_ch <- c(NA, rep(16,length(vnames)))
    col_l <- c(NA, alpha(MSpca$mt_col,0.5))
  }

  tiff(file = titre, width = 680, height = 450,units = "px")
  par(mar = c(5,5,1,15), cex.main=2, cex.lab = 2, cex.axis = 2,mgp = c(3.5,1.5,0),xpd = FALSE)
  plot(MSpca$Tr[,ax.pca[1]], MSpca$Tr[,ax.pca[2]], pch = 16, col = alpha(MSpca$mt_col,0.5),
       xlab = paste("PC",ax.pca[1],"(",MSpca$EV[ax.pca[1]],"%)"),
       ylab = paste("PC",ax.pca[2],"(",MSpca$EV[ax.pca[2]],"%)"))
  abline(h =0, v = 0, lty = 2)


  legend("topright", bty = "n", cex = 1.5, xpd = NA, inset = c(-0.5,0),
         legend = c("Sample(s) :",legende) , pch = p_ch, col = col_l, pt.cex = 1)
  dev.off()
}
# plot des scores (mt$PCx et mt$PCy)

PCA_plot_loadings <- function(pc, MSpca, L){

  vnames <- L$names_acq[L$Sacq]
  if(length(vnames) > 5){
    l_ech <- length(vnames)
    titre <- c("PCA/Loading_PC",pc,"of",vnames[1],"to",vnames[l_ech]) %>% str_flatten("_") %>% paste0(".tiff")
    legende <- c(vnames[1:3], vnames[(l_ech-2):l_ech])
    p_ch <- c(NA, rep(16,6))
    col_l <- c(NA, alpha(MSpca$mt_col[c(1:3,(l_ech-2):l_ech)],0.5))
  }else{
    titre <- c("PCA/Loading_PC",pc,"of",vnames) %>% str_flatten("_") %>% paste0(".tiff")
    legende <- c(vnames)
    p_ch <- c(NA, rep(16,length(vnames)))
    col_l <- c(NA, alpha(MSpca$mt_col,0.5))
  }

  tiff(file = titre, width = 800, height = 350,units = "px")
  par(mar = c(5,5,2.5,0.2), cex.main=2, cex.lab = 2, cex.axis = 2,mgp = c(3.5,1.5,0),xpd = FALSE)

  xpk <- names(MSpca$xmeans) %>% as.numeric()

  matplot(xpk, MSpca$P[,pc], type = "l",
          # xlim = c(mt$pca_plot_xmin, mt$pca_plot_xmax),
          # ylim = c(min(MSpca$P[,pc]), max(MSpca$P[,pc]))*1.2,
          xlab = "m/z", ylab = "Relative intensity (u.a.)",
          main = paste0("loadings of PC", pc, " (", MSpca$EV[pc], " %)"))
  dev.off()
}


# PCA ####
if(mt$PCA == TRUE){
  PCA_npc = 7

  if(("PCA" %in% dir())==FALSE){
    dir.create("PCA")
  }

  if(mt$PCA_AUC == TRUE){
    pca_mat <- sp$peaks[L$Sacq,]

    fmr <- apply(pca_mat,2, sd)
    fmr <- which(fmr > 0)
    pca_mat <- pca_mat[,fmr]

    xpk <- colnames(pca_mat) %>% as.numeric()
  }else{
    # pca_mat <- sp$MS[coor_mt(mt$acq),]
    # sp$xMS_PCA <- sp$xMS
  }

  # selection de variables
  matcor <- cor(pca_mat)
  estim_var <- function(matcor, thr){
    rep <- NULL
    fmr <- 1
    for(i in 1:(ncol(matcor)-1)){
      if(i >= fmr){
        rep <- c(rep, fmr)
        fmr <- min(which(matcor[i,i:ncol(matcor)] < thr), na.rm = TRUE)+i-1
      }
    }
    return(rep)
  }
  varsel95 <- estim_var(matcor, 0.95)
  varsel99 <- estim_var(matcor, 0.99)
  #stepAIC()

  MSpca <- rnirs::pca(pca_mat[,varsel99], ncomp = PCA_npc)

  MSpca$EV <- MSpca$explvar[,3] %>% multiply_by(100) %>% round(2)
  MSpca$mt_col <- rep_mtm("color", L)

  if(mt$PCA_report == TRUE){
    titrepdf <-  dir("PCA") %>% grep("rapport_pca",.) %>% length() %>%
      add(1) %>% paste0(sp$h5$wd,"/PCA/rapport_pca",.,".pdf")
    rmarkdown::render("S:/PTR-MS/Rpackages/provoc/R/rapport_pca.Rmd", output_file = titrepdf)
  }

  if(mt$PCA_plot_scores == TRUE){

    npc_plot <- seq(1:PCA_npc)

    for(i in 1:(PCA_npc-1)){
      PCx <- i
      for(j in (i+1):PCA_npc){
        PCy <- j
        PCA_plot_scores(MSpca,PCx,PCy,L)
      }
    }
  }

  if(mt$PCA_plot_loading == TRUE){
    # if(mt$PCA_plot_loading_PC[1] == "all"){
    #   mt$PCA_plot_loading_PC <- seq(1:mt$PCA_npc)
    # }
    #
    npc_plot <- PCA_npc
    for(i in 1:PCA_npc) PCA_plot_loadings(pc = i, MSpca, L)

  }
}


###########################

br <- c(det_c(251.5,sp$xMS):det_c(252.5,sp$xMS))
tiff("test.tiff")
matplot(sp$xMS[br],sp$MS[br,], type = "l", lty = 1, col = viridis(n = ncol(sp$MS), alpha = 0.2), ylim=c(0,200))
dev.off()
