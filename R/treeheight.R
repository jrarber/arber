#' Maximum Tree Height Model
#'
#' Model maximum tree height for an individual given wood density, assimilation, and respiration.
#'
#' @param wood.dens.stem Stem wood density
#' @param wood.dens.root Root wood density
#' @param assimilation Leaf-level rate of CO2 assimilation, in umol_m-2_s-1
#' @param leaf.mass.area Leaf mass area
#' @param growing.season Growing season length in days
#' @param daylength Daylength in hours during the growing season
#' @param years A specified number of years to let the individual grow. If left blank, will grow until it reaches maximum height.
#' @param respir.leaf.m Mass-based leaf respiration rate, in umol_g-1_s-1.
#' @param respir.stem.m Mass-based stem respiration rate, in umol_g-1_s-1.
#' @param respir.root.m Mass-based root respiration rate, in umol_g-1_s-1.
#' @param respir.leaf.a Area-based leaf respiration rate, in umol_m-2_s-1.
#' @param respir.stem.v Volume-based stem respiration rate, in umol_m-3_s-1.
#' @param respir.root.v Volume-based root respiration rate, in umol_m-3_s-1.
#' @param keep.years boolean. If true will return a dataframe with each years associated growth and respiration broken down by main organs (roots, stems, leaves).
#'
#' @return Returns the maximum height, how long it took to grow, and a data frame of each years growth assessment (optional).
#' @export

treeheight <- function(wood.dens.stem,
                       wood.dens.root,
                       assimilation,
                       leaf.mass.area = 60,
                       growing.season = 210,
                       daylength = 10,
                       years = NULL,
                       echo = F,
                       keep.years = T,
                       respir.leaf.m = 0.002136,
                       respir.stem.m = 9.23333E-05,
                       respir.root.m = 0.001051667,
                       respir.leaf.a = 0.2,
                       respir.stem.v = 60,
                       respir.root.v = 100){
  ## Define Constants
  CnstrCost = 1.5 #    Carbon cost of construction (g_g-1)
  CnstrResp = 0.42 # Respiration cost of construction (g_g-1)
  MWCO2 = 44.009 #  Molecular weight of carbon dioxide (g_mol-1)


  if(is.null(years)) years = 1000

  Hbyear <- matrix(nrow = years, ncol = 17, dimnames = list(seq_len(years),
                                                            c("Year", "Height",
                                                              "Photosynthesis",
                                                              "Leaf_mass",
                                                              "Branch_mass",
                                                              "Stem_mass",
                                                              "Root_mass",
                                                              "Assimilate",
                                                              "Respir_leaf",
                                                              "Respir_stem",
                                                              "Respir_root",
                                                              "Leaf_Construction",
                                                              "Remaining_assimilate",
                                                              "Remaining_assimilate_grams",
                                                              "Allocation_roots",
                                                              "Allocation_stems",
                                                              "Water_potential")))

  # Define the first year
  year = 1
  dbh = 1
  Wpot = dbh*(-0.01)

  height = (10^(2.29+0.6*log10(dbh)))/100 # meters
  phosyn = ((26.771*Wpot+96.916)/100)*assimilation # photosynthesis (micromoles/m^2/s)

  # Calculate mass of individual
  leaf.mass = (10^(1.6+1.75*log10(dbh))) # grams
  stem.mass = (10^(2.31+2.26*log10(dbh)))*wood.dens.stem # grams
  branch.mass = (10^(1.418+2.393*log10(dbh)))
  root.mass = sum(c(leaf.mass, stem.mass, branch.mass))*0.25 # grams

  # Calculate assimilated and respired carbon
  assimilate = growing.season*daylength*phosyn*3600* #
    ((10^(1.6+1.75*log10(dbh)))/leaf.mass.area)
  leaf.resp = (24-daylength)*growing.season*respir.leaf.m*3600*leaf.mass # micromoles
  stem.resp = 24*365*respir.stem.m*3600*(stem.mass + branch.mass) # micromoles
  root.resp = 24*365*respir.root.m*3600*root.mass # micromoles
  leaf.construction = (((((leaf.mass*CnstrCost)+(leaf.mass*CnstrResp))
                         /0.68))/MWCO2)*(10^6) # micromoles

  # Calculate remaining assimilate
  rem.assimilate = assimilate - (leaf.resp + stem.resp + root.resp + leaf.construction)

  # convert remaining assimilate to grams
  rem.assim.glucose = ((rem.assimilate/(10^6)*MWCO2)*0.68)/(CnstrCost + CnstrResp) # grams

  # Allocate (some) remaining assimilate to growth
  root.allocation = 0.2593*rem.assim.glucose
  stem.allocation = (rem.assim.glucose-root.allocation)/2

                      # 1 Year      # 2 Height
  Hbyear[1,] <- c(year,height, phosyn, leaf.mass, branch.mass,
                      stem.mass, root.mass, assimilate,
                      leaf.resp, stem.resp,
                      # 11 Root respir        # 12 Leaf consturction
                      root.resp, leaf.construction,
                      rem.assimilate,             # 13
                      rem.assim.glucose,    # 14
                      root.allocation,                # 15
                      stem.allocation,                # 16
                      Wpot)                            # 17


  for(t in 2:years){
    prevyear = t-1
    this.year <- t

    # Calculate values for new biomass allocation in branches and stems
    branch.mass = Hbyear[prevyear, 5] + Hbyear[prevyear, 14]
    stem.mass = Hbyear[prevyear, 6] + Hbyear[prevyear, 14]
    root.mass = Hbyear[prevyear, 7] + Hbyear[prevyear, 15]

    # Incroproate previous years growth into DBH and height
    dbh = 10^((log10(stem.mass/wood.dens.stem)-2.31)/2.26)
    height = (10^(2.29+0.6*log10(dbh)))/100

    if(echo){
      print(paste0("Year", "     ", "Height", "     ", "Root Mass", "     ", "Stem Mass", "     ", "Leaf Mass"))
      print(paste(t, "     ", round(height, 2),"      ", round(root.mass, 2),"   ", round(stem.mass,2),"   ", round(leaf.mass,2)))
    }

    # We got TALLER, more negative water botentials
    Wpot = dbh*(-0.01)
    if(length(Wpot) == 0)stop("Wpot")
    # Our new photosynthetic capacity
    phosyn = ((26.771*Wpot+96.916)/100)*assimilation # photosynthesis (micromoles/m^2/s)
    if(length(phosyn) == 0) stop("phosyn")
    ## Grow new leaves this year
    leaf.mass = (10^(1.6+1.75*log10(dbh))) # grams

    # Calculate assimilated and respired carbon
    assimilate = growing.season*daylength*phosyn*3600* #
      ((10^(1.6+1.75*log10(dbh)))/leaf.mass.area)
    leaf.resp = (24-daylength)*growing.season*respir.leaf.m*3600*leaf.mass # micromoles
    stem.resp = 24*365*respir.stem.m*3600*sum(c(stem.mass, branch.mass)) # micromoles
    root.resp = 24*365*respir.root.m*3600*root.mass # micromoles
    leaf.construction = (((((leaf.mass*CnstrCost)+(leaf.mass*CnstrResp))
                           /0.68))/MWCO2)*(10^6) # micromoles

    # Calculate remaining assimilate
    rem.assimilate = assimilate - (leaf.resp + stem.resp + root.resp + leaf.construction)

    # convert remaining assimilate to grams
    rem.assim.glucose = ((rem.assimilate/(10^6)*MWCO2)*0.68)/(sum(c(CnstrCost, CnstrResp))) # grams

    # Allocate (some) remaining assimilate to growth
    root.allocation = 0.2593*rem.assim.glucose
    if(is.na(root.allocation)) stop("root.all")
    stem.allocation = (rem.assim.glucose-root.allocation)/2
    if(is.na(stem.allocation)) stop("stem.all")


    Hbyear[t,] <- c(this.year,height, phosyn, leaf.mass, branch.mass,
                      stem.mass, root.mass, assimilate,
                      leaf.resp, stem.resp,
                      # 11 Root respir        # 12 Leaf consturction
                      root.resp, leaf.construction,
                      rem.assimilate,             # 13
                      rem.assim.glucose,    # 14
                      root.allocation,                # 15
                      stem.allocation,                # 16
                      Wpot)                            # 17


    # STOP when our height is no longer growing
    if(height == Hbyear[prevyear, 2]) break
  }
  Hbyear <- Hbyear[complete.cases(Hbyear),]

  max.height = max(Hbyear[,2])
  year.at.max = length(Hbyear[,1])

  cat("\n\nMaximum height (", max.height,"meters ) reached after", year.at.max, "years.\n\n")

  out <- list(max.height, year.at.max)
  names(out) <- c("Max.height", "Year")
 if(keep.years) out <- append(out, list(Hbyear)); c("Max.height", "Years.till.max", "Yearly.growth") -> names(out)

  return(out)

}

