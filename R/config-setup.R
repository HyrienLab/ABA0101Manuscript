
simplify_group_label = function(full_group_label){
  full_group_label |>
    str_remove("Group ") |>
    str_remove(" IV") |>
    str_remove(" SC")
}

route_label = tribble(
  ~route, ~route_pl, ~route_no,
  "IV", "Intravenous", 1,
  "SC", "Subcutaneous", 2
)

group_colors <- c(
  "Group A: 0.3 mg/kg IV" = "#1B9E77",
  "Group B: 1 mg/kg SC" = "#D95F02",
  "Group C: 1 mg/kg IV" = "#7570B3",
  "Group D: 3 mg/kg IV" = "#E7298A",
  "Group E: 10 mg/kg IV" = "#66A61E",
  "Group F: 30 mg/kg IV" = "#E6AB02",
  "Group J: 2.5 mg/kg SC" = "#A6761D",
  "Group K: 10 mg/kg SC" = "#666666",
  "Group H: 10 mg/kg IV" = "#0F6B99",
  "Group I: 30 mg/kg IV" = "#B22C2C"
)

groups_colors_altname = group_colors
names(groups_colors_altname) = simplify_group_label(names(groups_colors_altname))

pkpd_parameters_mm <- tribble(
  ~parameter, ~parameter_label, ~parameter_description,
  "ka_pop", "$k_a$", "Absorption rate (day$^{-1}$)",
  "F_pop", "$F$", "Bioavailability",
  "V_pop", "$V_c$", "Volume, central compartment (L)",
  "beta_V_logtWt", "$\\beta_{V,log(Wt)}$", "Weight-adjustment covariate",
  "Cl_pop", "$Cl$", "Clearance (L/day)",
  "beta_Cl_logcd4", "$\\beta_{Cl,log(CD4pct)}$", "CD4 pct.-adjustment covariate",
  "Q_pop", "$Q$", "Intercompartmental clearance (L/day)",
  "V2_pop", "$V_p$", "Volume, peripheral compartment (L)",
  "kel_pop", "$k_{el}$", "Linear clearance (day$^{-1}$)",
  "beta_kel_log_dose", "$\\beta_{kel,log(dose)}$", "dose-adjustment covariate",
  "Vstart_pop", "$Vm_0$", "Initial maximal velocity of the non-linear elimination (mcg/mL day$^{-1}$)",
  "vk_pop", "$v_k$", "Exponential rate of change between $Vm_0$ and $Vm_\\infty$ (day$^{-1}$)",
  #"Vend_pop", "$Vm_\\infty$", "Final maximal velocity of the non-linear elimination (mcg/mL day$^{-1}$)",
  "Vm_pop", "$Vm_\\infty$", "Final maximal velocity of the non-linear elimination (mcg/mL day$^{-1}$)",
  "Km_pop", "$K_m$", "Concentration of half-maximal velocity (mcg/mL)",
  "EC50_pop", "$EC_{50}$", "Concentration producing half occupation (mcg/mL)",
  "h_pop", "$h$", "Hill coefficient or slope",
  "omega_V", "$\\omega_{Vc}$", "SD, volume of central compartment",
  "omega_Cl", "$\\omega_{Cl}$", "SD, clearance",
  "omega_EC50", "$\\omega_{EC_{50}}$", "SD, $EC_{50}$",
  "b1", "$\\sigma_{concentration}$ (proportional)", "Model SE",
  "b2", "$\\sigma_{CD4RO}$ (proportional)", "Model SE"
)