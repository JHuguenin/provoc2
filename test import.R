# functions
{

#read.h5
read.h5 <- function(num_fil=1, ll = f_h5){

  # find the name of file
  name_h5 <- nm_ls(num_fil)

  # files import
  act_h5 <- paste0("h5/",dir("h5")[num_fil]) %>% H5Fopen()

  # abscissa extraction [~160 000 pts]
  xMS <- act_h5$FullSpectra$MassAxis

  # intensity extraction
  all_MS <- act_h5$FullSpectra$TofData[,1,,]
                # intensities extraction [ acquisition number * 160 000 pts]
  fmr <- dim(all_MS)
  dim(all_MS) <- c(fmr[1], fmr[2] * fmr[3]) # for 2d array

  # timing extraction
  all_timing <- act_h5$TimingData$BufTimes
  dim(all_timing) <- c(fmr[2] * fmr[3])

  # date extraction
  all_date <- acq.time(act_h5) + all_timing

  #TPS2
  all_TPS2 <- act_h5$TPS2$TwData
  fmr <- dim(all_TPS2)
  dim(all_TPS2) <- c(fmr[1], fmr[2] * fmr[3])
  row.names(all_TPS2) <- act_h5$TPS2$TwInfo

  # Files close
  H5Fclose(act_h5)

  # reduction
  fmr <- 1:det_c(xMS,50)
  xMS <- as.vector(xMS[-fmr])
  MS <- all_MS[-fmr,]

  # print the working progress and the time code
  print.h(paste0(name_h5, " # ",which(num_fil == ll), "/", length(ll)))

  # return
  list("name" = name_h5,
       "xMS" = xMS,
       "MS" = MS,
       "date" = all_date,
       "timing" = all_timing,
       "nbr_sp" = ncol(MS),
       "meta" = all_TPS2)
}

#concatenation
conc.lst <- function(list_n, elem = 1){
  list_n[[elem]]
}

#concatenation
dim.lst <- function(list_n, elem = 1){
  dim(list_n[[elem]])
}

#names of acquisition
prep.names <- function(L){
  fmr <- log10(L$nbr_sp) %>% floor() %>% add(1)
  rbind(fmr, L$names, L$nbr_sp)
}

names.samples <- function(vec)  str_pad(1:vec[3],vec[1], pad = "0") %>% paste(vec[2],.,sep = "_")

convertStr2List <- function(L){
  plip <- function(vec) return(vec)
  fmr <- unlist(L$names_acq) %>% lapply(plip)
  names(fmr) <- unlist(L$names_acq)
  return(fmr)
}

# acq.time
acq.time <- function(ls.t = ls_h5[[1]]){
  oldw <- getOption("warn")
  options(warn = -1)

  eph <- ls.t$AcquisitionLog$Log$timestring[1] %>%
    as.POSIXct(format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

  options(warn = oldw)
  # return(T_acq)
  return(eph)
}

# mass shift ####
length.xMS <- function(splist) length(splist$xMS)

mass.shift <- function(Li){
  min_xMS <- lapply(Li, length.xMS) %>% unlist()

  if(length(unique(min_xMS)) == 1){
    return(Li[[1]]$xMS)
  }else{
    min_xMS <- min(min_xMS)

    mat_xMS <- Li[[1]]$xMS[1:min_xMS]
    for(i in 2:length(Li)) mat_xMS <- rbind(mat_xMS, Li[[i]]$xMS[1:min_xMS])

    fmr <- seq(50,500,10) %>% sapply(det_c, vec = mat_xMS[1,])
    diff_xMS <- t(mat_xMS[,fmr])-mat_xMS[1,fmr]
    ind_diff <- which(diff_xMS[1,] != 0)
    n_df <- length(ind_diff)
    sp_name <- sapply(Li,conc.lst, elem = 1)

    png("Figures/Control/files_shifted.png", width = 400, height = 350)
      par(mar = c(3,3,3,0.1), mgp = c(2,1,0), cex = 1.5)
      matplot(mat_xMS[1,fmr], diff_xMS[,ind_diff], type = "l", lty = 1,
              col = viridis(n_df, direction = -1), lwd = 2,
              ylab = "Mass shift", xlab = "m/z",
              main = paste("file(s) shifted"))
      legend("right",legend = ,sp_name[ind_diff], bty = "n", lty = 1, lwd = 2,
             col = viridis(n_df, direction = -1))
    dev.off()
    print.h("There is a shift in m/z. Look the figure control")
    return(mat_xMS[1,])
  }
}

# concatenation MS ####
return.MSa <- function(splist, brn) return(t(splist$MS[1:brn,]))

# create Mass Spectrum objet ####
create_local_MS <- function(MS, xMS) createMassSpectrum(xMS,MS)

# return to spectra
mat.spectra <- function(spobj) spobj@intensity
mass.spectra <- function(spobj) spobj@mass

# Fonction inutile : ####
citation_list <- {list(
  c("Il faut aller trop loin pour decouvrir les limites.", "Joris Huguenin"),
  c("Les trous dans les pantalons c'est comme les enfants. ca n'arrete pas de grandir", "Joris Huguenin"),
  c("Dieu, aie pitie de nous, nous sommes a la merci des ingenieurs !", 'Dr.Malcom, Jurassic Park'),
  c("Grab a brush and put on a little make-up.","System of a Down"),
  c("The Sun Machine is Coming Down, and We're Gonna Have a Party.", "David Bowie"),
  c("I'm just a poor boy, I need no sympathy.", "Queen"),
  c("Au village, sans pretention, J'ai mauvaise reputation.","Georges Brassens"),
  c("Debout les gars, reveillez-vous ! On va au bout du monde.","Huges Aufray"),
  c("Tu dis qu'si les elections ca changeait vraiment la vie
Y'a un bout d'temps, mon colon, qu'voter ca s'rait interdit","Renaud"),
  c("Ready or not, here I come, you can't hide. Gonna find you and make you want me.","The Fugees"),
  c("Emancipate yourselves from mental slavery.","Bob Marley"),
  c("Parce que c'est notre BROCHEEEET !!!.", "Manuel Macro"),
  c("Hey DJ met nous donc du Funk, que je danse le MIA. Je danse le MIA.", "IAM"),
  c("Doo, doo, doo, doo, doo, doo, doo, doo.", "Lou Reed"),
  c("L'obscurite ne peut pas chasser l'obscurite, seule la lumiere le peut. La haine ne peut pas chasser la haine, seul l'amour le peut.", "Martin Luther King"),
  c("La vie, ce n'est pas d'attendre que les orages passent, c'est d'apprendre a danser sous la pluie.", "Seneque"),
  c("Nos vies sont pleines de catastrophes qui n'ont jamais eu lieu.", "Auteur inconnu"),
  c("S'il y a un probleme, il y a une solution. S'il n'y a pas de solution, alors ce n'est pas un probleme.", "Auteur inconnu"),
  c("Si vous pouvez le rever, vous pouvez le faire.", "Walt Disney"),
  c("Ils ne savaient pas que c'etait impossible, alors ils l'ont fait.", "Mark Twain"),
  c("J'ai decide d'etre heureux parce que c'est bon pour la sante.", "Voltaire"),
  c("Si vous pensez que l'aventure est dangereuse, essayez la routine, elle est mortelle.", "Paulo Coelho"),
  c("Les gens les plus heureux n'ont pas tout ce qu'il y a de mieux. Ils font juste de leur mieux avec tout ce qu'ils ont.", "Auteur inconnu"),
  c("Le veritable voyage ne consiste pas a chercher de nouveaux paysages, mais a avoir de nouveaux yeux.", "Marcel Proust"),
  c("Avec trop on se perd. Avec moins on se trouve.", "Tchouang Tseu"),
  c("N'aie pas peur d'avancer lentement. Aie peur de rester immobile.", "Proverbe chinois"),
  c("Ne cherche pas le bonheur, cree-le.", "Auteur inconnu"),
  c("Ne t'inquiete pas de l'echec. Inquiete-toi de ce que tu manques si tu n'essayes meme pas.", "Jack Canfield"),
  c("Mieux vaut fait que parfait.", "Auteur inconnu"),
  c("Dieu existe-elle ?", "Patrick Sebastien"),
  c("Lorsqu'on regarde dans la bonne direction, il ne reste plus qu'a avancer.", "Proverbe bouddhiste"),
  c("Un objectif bien defini est a moitie atteint.", "Abraham Lincoln"),
  c("Quand on ose, on se trompe souvent. Quand on n'ose pas, on se trompe toujours.", "Romain Rolland"),
  c("La vie c'est comme une bicyclette, il faut avancer pour ne pas perdre l'equilibre.", "Albert Einstein"),
  c("Il y a deux facons de penser. L'une est de croire que les miracles n'existent pas. L'autre est de croire que chaque chose est un miracle.", "Albert Einstein"),
  c("Fais de ta vie un reve et d'un reve une realite.", "Antoine de St Exupery"),
  c("Il y a plus de courage que de talent dans la plupart des reussites.", "Felix Leclerc"),
  c("Ce que nous sommes est le resultat de ce que nous avons pense.", "Bouddha"),
  c("Les gagnants cherchent des moyens, les perdants des excuses.", "Franklin Roosevelt"),
  c("Un voyage de mille lieues commence toujours par un premier pas.", "Lao Tseu"),
  c("Tous les jours a tous points de vue, je vais de mieux en mieux.", "Emile Coue"),
  c("Il faut toujours viser la lune car meme en cas d'echec on atterrit dans les etoiles.", "Oscar Wilde"),
  c("Ce n'est pas parce que les choses sont difficiles que nous n'osons pas les faire, c'est parce que nous n'osons pas les faire qu'elles sont difficiles.", "Seneque"),
  c("N'attendez pas d'etre heureux pour sourire. Souriez plutot afin d'etre heureux.", "Edward L. Kramer"),
  c("Si tu fais ce que tu as toujours fait, tu obtiendras ce que tu as toujours obtenu.", "Tony Robbins"),
  c("Redemarrage de l'evaluation d'une promesse interrompue.", "R peotic warning message"),
  c("Je suis gentil avec tout le monde, celui qui dit le contraire je lui foutrai mon poing dans la gueule.", "Leo Ferre"),
  c("Le desespoir est une forme superieure de la critique.", "Leo Ferre"),
  c("Les diplomes sont faits pour les gens qui n'ont pas de talent.","Pierre Desproges"),
  c("Bal tragique a Colombey, un mort","Hara Kiri"),
  c("Si la matiere grise etait plus rose, le monde aurait moins les idees noires.","Pierre Dac"),
  c("J'ai pris la decision de ne plus etre influencable. Qu'est-ce que vous en pensez ?","Patrick Sebastien"),
  c("Est-il indispensable d'etre cultive quand il suffit de fermer sa gueule pour briller en societe ?","Pierre Desproges"),
  c("On ne discute pas recettes de cuisine avec des anthropophages.", "Jean-Pierre Vernant"))}

# Make the name of samples. The date (20yymmdd_hhmmss.h5) is deleting and
# the acquisitions with the same name.
nm_ls <- function(f_h5){
  nm_h5 <- str_remove_all(dir("h5")[f_h5],"_20......_......")
  nm_h5 <- str_remove_all(nm_h5,"20......_......_")
  nm_h5 <- str_remove_all(nm_h5,".h5")
  if(length(nm_h5) != length(unique(nm_h5))){
    unm <- unique(nm_h5)
    for (i in 1:length(unm)){
      inm <- which(nm_h5 == unm[i])
      if(length(inm) > 1){
        eph <- log(length(inm),10) %>% floor() %>% add(1)
        nm_h5[inm] <- paste0("000", 1:length(inm)) %>% str_sub(-eph) %>% paste(nm_h5[inm], ., sep = "_")
      }
    }
  }
  return(nm_h5)
}

# retourne l'index valable d'une borne le long d'un vecteur.
det_c <- function(brn,vec){
  subtract(vec,brn) %>% sapply(abs) %>% which.min()
}

# print le texte suivi de l'heure
print.h <- function(txt = "hello there") heure() %>% paste0(txt,", ",.) %>% print()

# donne l'heure
heure <- function() str_split(Sys.time(),pattern = " ")[[1]][2]

#### import ####
import.h5 <- function(wdir = wd){

  if(("Figures" %in% dir())==FALSE){
    dir.create("Figures")
    dir.create("Figures/Control")
  }

  if(("h5" %in% dir())==FALSE){
    cat("Sorry but the import can't continue. Create a \"h5\" folder with all .h5 fills that you
        want analyse.")
  }

  # Data importation ####
  f_h5 <- dir("h5") %>% grep(".h5",.)     # localise h5 files

  length(citation_list) %>% sample(1) %>% citation_list[[.]] %>% cat()
  list_h5 <- lapply(f_h5, read.h5, ll = f_h5)

  # formating of sp list ####
  sp <- list()
  sp$names <- sapply(list_h5,conc.lst, elem = 1)
  sp$date <- sapply(list_h5,conc.lst, elem = 4, simplify = FALSE)
  sp$timing <- sapply(list_h5,conc.lst, elem = 5, simplify = FALSE)
  sp$nbr_sp <- sapply(list_h5,conc.lst, elem = 6)
  sp$meta <- sapply(list_h5,conc.lst, elem = 7, simplify = FALSE)
  sp$wd <- wdir

  sp$xMS <- mass.shift(list_h5)
  print.h("Concatene MS")
  sp$MS <- sapply(list_h5, return.MSa, brn = length(sp$xMS), simplify = FALSE) %>% do.call(rbind,.)

  remove(list_h5)

  sp$names_acq <- prep.names(sp) %>% apply(2,names.samples)

  # Create MassSpectrum object
  print.h("Create MassSpectrum object")
  spectra <- apply(sp$MS,1, create_local_MS, xMS = sp$xMS)

  ## smooth spectra
  print.h("Smooth spectra")
  oldw <- getOption("warn")
  options(warn = -1)
    spectra <- smoothIntensity(spectra,
                             method = "SavitzkyGolay",
                             halfWindowSize = 3)
  options(warn = oldw)

  ## align spectra
  print.h("Align spectra")
  spectra <- alignSpectra(spectra, tolerance = 0.02)
  avgSpectra <- averageMassSpectra(spectra, labels = convertStr2List(sp), method="mean")

  # peak detection
  print.h("Peak detection")
  peaks <- detectPeaks(avgSpectra, method="MAD", halfWindowSize=20, SNR=5)
  peaks <- binPeaks(peaks, tolerance=0.01)
  peaks <- MALDIquant::filterPeaks(peaks, minFrequency=0.25)
  sp$peaks <- intensityMatrix(peaks, avgSpectra)

  sp$MS <- sapply(avgSpectra, mat.spectra)
  sp$xMS <- sapply(avgSpectra, mass.spectra) %>% rowMeans() %>% round(3)

  rownames(sp$MS) <- sp$xMS
  colnames(sp$MS) <- unlist(sp$names_acq)

  print.h("Import is completed")
  return(sp)
}

#library
library(tidyverse)
library(rhdf5)
library(magrittr)
library(MALDIquant)
}

######

  # wd <- "S:/PTR-MS/Magali Proffit/Candice/Lavandes2021/sem23_48h_3series/3x4chb/analyse reduite"
  # wd <- "C:/Users/huguenin/Documents/R/provoc2/data test/miscalenous" # sans "/" final
  #

  setwd(wd)
  sp <- import.h5()

