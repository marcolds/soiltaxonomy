#
# set function to variable
#
# takes args to init empty spc, input data after
#
# pedons = vector of pedon ids
# nhz = vector of num of hzs matched to each pedon id
# hz_tops = list of hz_tops for each pedon id
# hz_bots = list of hz_bots for each pedon id
#
# defaults to 1 row
#


hz_init <- function(pedons = c(1),
                     nhz = c(1),
                     tops = list(0),
                     bots = list(1))
  {

  hz_properties <- c("id",
                     "hz_top",
                     "hz_bot",
                     "hz_name",
                     "texture",
                     "sand",
                     "silt",
                     "clay_tot",
                     "clay_carb",
                     "silt_f",
                     "silt_c",
                     "sand_vf",
                     "sand_f",
                     "sand_m",
                     "sand_c",
                     "sand_vc",
                     "cole",
                     "bulk_d",
                     "lin_ext",
                     "hue_wet",
                     "val_wet",
                     "chroma_wet",
                     "hue_dry",
                     "val_dry",
                     "chroma_dry",
                     "whc",
                     "wrd",
                     "acid_t",
                     "base_sum",
                     "al_ext",
                     "cec_7",
                     "cec_82",
                     "cec_eff",
                     "al_sat",
                     "base_sat_82",
                     "base_sat_7",
                     "ca_ex",
                     "mg_ex",
                     "na_ex",
                     "k_ex",
                     "ph_h2o",
                     "ph_cacl2",
                     "ph_kcl",
                     "ph_paste",
                     "ph_ox",
                     "ph_naf",
                     "gyp",
                     "anhyd",
                     "caco3",
                     "ec",
                     "esp",
                     "sar",
                     "c_tot",
                     "n_tot",
                     "p_ret",
                     "s_tot",
                     "al_ex_amox",
                     "fe_ex_amox",
                     "si_ex_amox",
                     "fe_ex_dici",
                     "al_ex_dici",
                     "org_c",
                     "org_mat",
                     "nap_hue_wet",
                     "nap_val_wet",
                     "nap_chroma_wet",
                     "nap_hue_dry",
                     "nap_val_dry",
                     "nap_chroma_dry",
                     "rub_fib",
                     "melanic_idx")

  nprops <- length(hz_properties)
  df <- setNames(data.frame(matrix(ncol=nprops,nrow=sum(nhz))),hz_properties) # empty df

  r <- 1
  for(i in c(1:length(pedons))){
    idp <- pedons[i]
    n <- nhz[i]
    top <- tops[[i]] # vector of tops for hzs in pedon
    bot <- bots[[i]] # vector of bots for hzs in pedon

    df$id[r:(r+n-1)] <- idp # fills in df for pedon id
    df$hz_top[r:(r+n-1)] <- top # fills in df for top depths
    df$hz_bot[r:(r+n-1)] <- bot # fills in df for bottom depths

    r <- r + n
  }

  return(df)

}


#
# elevates hz_init to spc
#

spc_init <- function(pedons = c(1),
                     nhz = c(1),
                     tops = list(0),
                     bots = list(1))
  {

  df <- hz_init(pedons=pedons,
                nhz=nhz,
                tops=tops,
                bots=bots)

  depths(df) <- id ~ hz_top + hz_bot # promotes df to SPC
  return(df)

}
