# Process Childrens' anthropometric data for DHS
# Date: 2018_06_29
# Author: Tim Essam, GeoCenter -- adopted from Laura Hughes original code

# prep data ---------------------------------------------------------------

#kr_ten <- str_c(dhs2010, 'MWKR61DT/MWKR61FL.DTA')
kids_raw10 <- read_dta(here(dhs2010, 'MWKR61DT/MWKR61FL.DTA'))

desc_vars(kids_raw10, v025)

kids10 <- kids_raw10 %>%
  
  select(
    rural = v025,
    cluster = v001,
    hh_num = v002,
    svywt = v005,
    interview_month = v006,
    age_mom = v447a,
    # check not v012 age_mom_cat = v013,
    psu = v021,
    strata = v022,
    region = v024,
    dejure = v135,
    
    # ed
    educ_high = v106,
    cmc = v017,
    educ_years = v133,
    educ_attainment = v149,
    lit = v155,
    
    
    # WASH
    drinking_src = v113,
    time2drinking = v115,
    toilet_src = v116,
    shared_toilet = v160,
    
    # hh characteristics
    elec = v119,
    hhsize = v136,
    kids_under5 = v137,
    femhead = v151,
    agehead = v152,
    freq_radio = v158,
    freq_tv = v159,
    
    # wealth
    wealth = v191,
    wealth_cat = v190,
    
    #bed net use?
    mos_net = ml101,
    mps_net = v459,
    kid_slept_mosquito = v460,
    mom_slept_mosquito = v461,
    
    # children ever born, ratios v201-207, v218, v219, v220
    births_5y = v208,
    births_1y = v209,
    age_first_birth = v212,
    curr_preg = v213,
    mcu = v313,
    want_child = v367,
    fp_radio = v384a,
    fp_tv = v384b,
    fp_news = v384c,
    fp_worker = v393,
    visit_healthfac = v394,
    heard_ors = v416,
    mom_stunting = v440,
    mom_bmi = v445,
    mom_rohrer = v446,
    hemoglobin = v456,
    # adj for alt, smoking
    anemic = v457,
    poo_disposal = v465,
    
    # why hard to go to med fac v467b-f
    live_wpartner = v504,
    num_otherwives = v505,
    age_firstcohab = v511,
    unmet_need = v624,
    ideal_boys = v627,
    ideal_girls = v628,
    ideal_either = v629,
    
    bidx,
    birth_order = bord,
    birth_month = b1,
    birth_year = b2,
    sex = b4,
    prev_birthinterval = b11,
    subseq_birthinterval = b12,
    
    # prenatal
    m2a,
    m2b,
    m2g,
    m2h,
    m2k,
    #2057 NAs
    doc_assist = m3a,
    nurse_assist = m3b,
    trad_assist = m3g,
    friend_assist = m3h,
    chw_assist = m3i,
    neighbor_assist = m3j,
    other_assist = m3k,
    no_assist = m3n,
    first_antenatal = m13,
    num_antenatal = m14,
    place_delivery = m15,
    birth_size = m18,
    
    # breastfeeding
    breastfeed_dur = m4,
    breastfeeding_months = m5,
    
    # vac
    vac_tb = h2,
    vac_dpt1 = h3,
    vac_polio1 = h4,
    vac_dpt2 = h5,
    vac_polio2 = h6,
    vac_dpt3 = h7,
    vac_polio3 = h8,
    vac_measles = h9,
    vac_polio0 = h0,
    diarrhea = h11,
    fever = 22,
    cough = h31,
    vitA = h33,
    vitA2 = h34,
    Fe = h42,
    int_parasites = h43,
    
    age_months = hw1,
    weight = hw2,
    height = hw3,
    
    # malnutrition - wasting, underweight and bmi are all missing for children
    stunting = hw70,
    underweight = hw71,
    wasting = hw72,
    bmi = hw73,
    
    kid_hemoglobin = hw56,
    kid_anemia = hw57,
    mosqnet_type = ml0
  ) %>% 
  
  # filter non-dejure residents; have no hh level info about them
  filter(dejure == 1)

# Check the dataset and labels
#kids_meta <- data.frame(attr(kids10, 'label'))
(contents(kids10))


# Recode and cleanup ------------------------------------------------------
#1) id and convert any NA codes
#2) convert decimal values to proper values
#3) convert categorical vars to factors
#4) classify factors into groaups wehre neded

# ID Values that are missing

View(id_weirdos(kids10))

# common missing values that are untaggee
kids10 %>% count_value()

# Check the missingness patterns for chunks of vars
desc_vars(kids10, stunting, wasting, underweight, bmi)

kids10 = kids10 %>% 
  # Convert "other" and "not dejure resident" to NA
  replace_missing()


kids10 = kids10 %>%
  # Converting "other" and "not dejure resident" to NA
  replace_missing(missing_codes = c(96, 97, 99), drinking_src, toilet_src) %>%
  
  replace_missing(missing_codes = c(996, 997, 998), time2drinking) %>%
  
  replace_missing(missing_codes = c(98), agehead, num_otherwives) %>%
  
  replace_missing(
    missing_codes = 9996:9999,
    mom_stunting,
    mom_bmi,
    mom_rohrer,
    stunting,
    wasting,
    underweight,
    bmi
  ) %>%
  
  replace_missing(
    missing_codes = c(8, 9),
    poo_disposal,
    birth_size,
    vac_tb,
    vac_dpt1,
    vac_polio1,
    vac_dpt2,
    vac_polio2,
    vac_dpt3,
    vac_polio3,
    vac_measles,
    vac_polio0,
    diarrhea,
    cough,
    vitA,
    vitA2,
    Fe,
    int_parasites
  )

kids10 %>% count_value(7) # not dejure
kids10 %>% count_value(8)
kids10 %>% count_value(9)
kids10 %>% count_value(98)
kids10 %>% count_value(99)
kids10 %>% count_value(998)
kids10 %>% count_value(999)
kids10 %>% count_value(9998)
kids10 %>% count_value(9999)


# Convert to decimals -----------------------------------------------------
kids10 = kids10 %>%
  mutate(
    svywt = svywt / 1e6,
    wealth = wealth / 1e5,
    stunting = stunting / 1e2,
    wasting = wasting / 1e2,
    underweight = underweight / 1e2,
    mom_rohrer = mom_rohrer / 1e2,
    mom_bmi = mom_bmi / 1e2,
    stunted = as.numeric(stunting < -2),
    wasted = as.numeric(wasting < -2),
    year = 2010
  )

desc_vars(
  kids10,
  svywt,
  wealth,
  stunting,
  wasting,
  underweight,
  mom_rohrer,
  mom_bmi,
  stunted,
  wasted
)


# Factorize and clean up categories ---------------------------------------
# Factorize some variables for plotting and sorting, also categorize ages into DHS groups
kids10 = kids10 %>% 
  
  # Need to relevel wealth_cat as middle is 2nd not 3rd
  factorize('_lab', region, sex, femhead, educ_high, wealth_cat, rural) %>% 
  
  # categorize the age ranges with half-open ranges to match DHS
  mutate(
    age_cat = cut(
      age_months,
      breaks = c(0, 6, 9, 12, 18, 24, 36, 48, 59),
      labels = c(
        "<6 months",
        "6 - 8",
        "9 - 11",
        "12 - 17",
        "18 - 23",
        "24 - 35",
        "36 - 47",
        "48 - 59"
      ),
      right = FALSE
    ),
    elig_age = ifelse(is.na(age_months) != TRUE, 1, 0),
    elig_height = ifelse(height <= 2500, 1, 0),
    elig = ifelse((elig_age == 1 & elig_height == 1), 1, 0),
    
    # Need to relevel wealth_cat as middle is 2nd not 3rd, same w/ education
    wealth_cat_lab = fct_relevel(wealth_cat_lab, "middle", after = 2),
    educ_high_lab = fct_relevel(educ_high_lab, "primary", after = 1),
    educ_high_lab2 = fct_collapse(educ_high_lab, 
                                  "no education" = "no education",
                                  "primary" = "primary",
                                  "secondary or higher" = c("higher", "secondary"))
  )



# Check factors and plot stunting to see if it’s reasonable ---------------

kids10 %>% select(ends_with("_lab")) %>% 
  map(levels)


kids10 %>% filter(elig_age == 1) %>%
  ggplot(., aes(
    x = age_months,
    y = stunted,
    group = sex_lab,
    color = sex_lab
  )) + 
  stat_smooth() + 
  facet_grid( ~ educ_high_lab2) +
  scale_colour_brewer(palette = "Set2")

describe(kids10)
contents(kids10)



