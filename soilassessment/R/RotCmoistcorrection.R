RotCmoistcorrection=function(P, E, S.Thick, clay, pE, fk)
{
  MTSMD = -(20 + 1.3 * clay - 0.01 * (clay^2)) * (S.Thick/23) *(1/fk)
  Moistdeficit = P - E * pE
  ATSMD = 0
  for (i in 2:length(Moistdeficit)) {
    ATSMD[1] = ifelse(Moistdeficit[1] > 0, 0, Moistdeficit[1])
    if (ATSMD[i - 1] + Moistdeficit[i] < 0) {
      ATSMD[i] = ATSMD[i - 1] + Moistdeficit[i]
    }
    else (ATSMD[i] = 0)
    if (ATSMD[i] <= MTSMD) {
      ATSMD[i] = MTSMD
    }
  }
  b = ifelse(ATSMD > 0.444 * MTSMD, 1, (0.2 + (1-0.2) * ((MTSMD - ATSMD)/(MTSMD - 0.444 * MTSMD))))
  return(b)
}
