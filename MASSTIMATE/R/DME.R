# based on Erickson & Tumanova (2000)
DME <- function(juv_proxy, adu_proxy, adu_mass, scale_fac = 3) {
  j.scale <- juv_proxy^scale_fac
  a.scale <- adu_proxy^scale_fac
  j.prop <- j.scale/a.scale
  j.mass <- adu_mass*j.prop
  return(j.mass)
}