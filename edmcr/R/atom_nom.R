atom_nom <- function(residuetype, atom, mode){
  delta <- -1
  natom <- ""
  
  switch(mode,
         full = switch(residuetype,
                       ALA = switch(atom,
                                    HB1 = {natom = "QB"
                                           delta=1},
                                    HB2 = {natom = "QB"
                                           delta=1},
                                    HB3 = {natom = "QB"
                                           delta=1},
                                    QB = c()),
                       ARG = switch(atom,
                                    HB2 = c(),
                                    HB3 = c(),
                                    QB = c(),
                                    HG2 = c(),
                                    HG3 = c(),
                                    QG = c(),
                                    HD2 = c(),
                                    HD3 = c(),
                                    QD = c(),
                                    HH11 = c(),
                                    HH12 = c(),
                                    QH1 = c(),
                                    HH21 = c(),
                                    HH22 = c(),
                                    QH2 = c()),
                       ASN = switch(atom,
                                    HB2 = c(),
                                    HB3 = c(),
                                    QB = c(),
                                    HD21 = c(),
                                    HD22 = c(),
                                    QD2 = c()),
                       ASP = switch(atom,
                                    HB2 = c(),
                                    HB3 = c(),
                                    QB = c()),
                       CYS = switch(atom,
                                    HB2 = c(),
                                    HB3 = c(),
                                    HG = c(),
                                    QB = c()),
                       CYSS = switch(atom,
                                     HB2 = c(),
                                     HB3 = c(),
                                     QB = c()),
                       GLN = switch(atom,
                                    HB2 = c(),
                                    HB3 = c(),
                                    QB = c(),
                                    HG2 = c(),
                                    HG3 = c(),
                                    QG = c(),
                                    HE21 = c(),
                                    HE22 = c(),
                                    QE2 = c()),
                       GLU = switch(atom,
                                    HB2 = c(),
                                    HB3 = c(),
                                    QB = c(),
                                    HG2 = c(),
                                    HG3 = c(),
                                    QG = c()),
                       GLY = switch(atom,
                                    HA2 = c(),
                                    HA3 = c(),
                                    QA = c()),
                       HIS = switch(atom,
                                    HB2 = c(),
                                    HB3 = c(),
                                    QB = c(),
                                    HD1 = c(),
                                    HD2 = c(),
                                    HE1 = c()),
                       ILE = switch(atom,
                                    HB = c(),
                                    HG21 = {natom = "QG2"
                                            delta=1},
                                    HG22 = {natom = "QG2"
                                            delta=1},
                                    HG23 = {natom = "QG2"
                                            delta=1},
                                    QG2 = c(),
                                    HG12 = c(),
                                    HG13 = c(),
                                    QG1 = c(),
                                    HD11 = {natom = "QD1"
                                            delta=1},
                                    HD12 = {natom = "QD1"
                                            delta=1},
                                    HD13 = {natom = "QD1"
                                            delta=1},
                                    QD1 = c()),
                       LEU = switch(atom,
                                    HB2 = c(),
                                    HB3 = c(),
                                    QB = c(),
                                    HG = c(),
                                    HD11 = {natom = "QD1"
                                            delta=1},
                                    HD12 = {natom = "QD1"
                                            delta=1},
                                    HD13 = {natom = "QD1"
                                            delta=1},
                                    QD1 = c(),
                                    HD21 = {natom = "QD2"
                                            delta=1},
                                    HD22 = {natom = "QD2"
                                            delta=1},
                                    HD23 = {natom = "QD2"
                                            delta=1},
                                    QD2 = c(),
                                    QQD = c()),
                       LYS = switch(atom,
                                    HB2 = c(),
                                    HB3 = c(),
                                    QB = c(),
                                    HG2 = c(),
                                    HG3 = c(),
                                    QG = c(),
                                    HD2 = c(),
                                    HD3 = c(),
                                    QD = c(),
                                    HE2 = c(),
                                    HE3 = c(),
                                    QE = c(),
                                    HZ1 = {natom = "QZ"
                                           delta=1},
                                    HZ2 = {natom = "QZ"
                                           delta=1},
                                    HZ3 = {natom = "QZ"
                                           delta=1},
                                    QZ = c()),
                       MET = switch(atom,
                                    HB2 = c(),
                                    HB3 = c(),
                                    QB = c(),
                                    HG2 = c(),
                                    HG3 = c(),
                                    QG = c(),
                                    HE1 = {natom = "QE"
                                           delta=1},
                                    HE2 = {natom = "QE"
                                           delta=1},
                                    HE3 = {natom = "QE"
                                           delta=1},
                                    QE = c()),
                       PHE = switch(atom,
                                    HB2 = c(),
                                    HB3 = c(),
                                    QB = c(),
                                    HD1 = c(),
                                    HD2 = c(),
                                    QD = c(),
                                    HB2 = c(),
                                    HB3 = c(),
                                    QB = c(),
                                    HE1 = c(),
                                    HE2 = c(),
                                    QE = c(),
                                    QR = c(),
                                    HZ = c()),
                       PRO = switch(atom,
                                    HB2 = c(),
                                    HB3 = c(),
                                    QB = c(),
                                    HG2 = c(),
                                    HG3 = c(),
                                    QG = c(),
                                    HD2 = c(),
                                    HD3 = c(),
                                    QD = c()),
                       SER = switch(atom,
                                    HB2 = c(),
                                    HB3 = c(),
                                    QB = c(),
                                    HG = c()),
                       THR = switch(atom,
                                    HB = c(),
                                    HG21 = {natom = "QG2"
                                            delta=1},
                                    HG22 = {natom = "QG2"
                                            delta=1},
                                    HG23 = {natom = "QG2"
                                            delta=1},
                                    QG2 = c(),
                                    HG1 = c()),
                       TRP = switch(atom,
                                    HB2 = c(),
                                    HB3 = c(),
                                    QB = c(),
                                    HD1 = c(),
                                    HE1 = c(),
                                    HE3 = c(),
                                    HZ2 = c(),
                                    HZ3 = c(),
                                    HH2 = c()),
                       TYR = switch(atom,
                                    HB2 = c(),
                                    HB3 = c(),
                                    QB = c(),
                                    HD1 = c(),
                                    HD2 = c(),
                                    QD = c(),
                                    HE1 = c(),
                                    HE2 = c(),
                                    QE = c(),
                                    QR = c(),
                                    HH = c()),
                       VAL = switch(atom,
                                    HB = c(),
                                    HG11 = {natom = "QG1"
                                            delta=1},
                                    HG12 = {natom = "QG1"
                                            delta=1},
                                    HG13 = {natom = "QG1"
                                            delta=1},
                                    QG1 = c(),
                                    HG21 = {natom = "QG2"
                                            delta=1},
                                    HG22 = {natom = "QG2"
                                            delta=1},
                                    HG23 = {natom = "QG2"
                                            delta=1},
                                    QG2 = c(),
                                    QQG = c())),
         homitted = switch(residuetype,
                           ALA = switch(atom,
                                        HB1 = {natom = "CB"
                                               delta=1},
                                        HB2 = {natom = "CB"
                                               delta=1},
                                        HB3 = {natom = "CB"
                                               delta=1},
                                        QB = c()),
                           ARG = switch(atom,
                                        HB2 = {natom = "CB"
                                               delta=1},
                                        HB3 = {natom = "CB"
                                               delta=1},
                                        QB = {natom = "CB"
                                              delta=1},
                                        HG2 = {natom = "CG"
                                               delta=1},
                                        HG3 = {natom = "CG"
                                               delta=1},
                                        QG = {natom = "CG"
                                              delta=1},
                                        HD2 = {natom = "CD"
                                               delta=1},
                                        HD3 = {natom = "CD"
                                               delta=1},
                                        QD = {natom = "CD"
                                              delta=1},
                                        HH11 = c(),
                                        HH12 = c(),
                                        QH1 = c(),
                                        HH21 = c(),
                                        HH22 = c(),
                                        QH2 = c()),
                           ASN = switch(atom,
                                        HB2 = {natom = "CB"
                                               delta=1},
                                        HB3 = {natom = "CB"
                                               delta=1},
                                        QB = {natom = "CB"
                                              delta=1},
                                        HD21 = c(),
                                        HD22 = c(),
                                        QD2 = c()),
                           ASP = switch(atom,
                                        HB2 = {natom = "CB"
                                               delta=1},
                                        HB3 = {natom = "CB"
                                               delta=1},
                                        QB = {natom = "CB"
                                              delta=1}),
                           CYS = switch(atom,
                                        HB2 = {natom = "CB"
                                               delta=1},
                                        HB3 = {natom = "CB"
                                               delta=1},
                                        HG = {natom = "SG"
                                              delta=1},
                                        QB = {natom = "CB"
                                              delta=1}),
                           CYSS = switch(atom,
                                         HB2 = {natom = "CB"
                                                delta=1},
                                         HB3 = {natom = "CB"
                                                delta=1},
                                         QB = {natom = "CB"
                                               delta=1}),
                           GLN = switch(atom,
                                        HB2 = {natom = "CB"
                                               delta=1},
                                        HB3 = {natom = "CB"
                                               delta=1},
                                        QB = {natom = "CB"
                                              delta=1},
                                        HG2 = {natom = "CG"
                                               delta=1},
                                        HG3 = {natom = "CG"
                                               delta=1},
                                        QG = {natom = "CG"
                                              delta=1},
                                        HE21 = c(),
                                        HE22 = c(),
                                        QE2 = c()),
                           GLU = switch(atom,
                                        HB2 = {natom = "CB"
                                               delta=1},
                                        HB3 = {natom = "CB"
                                               delta=1},
                                        QB = {natom = "CB"
                                              delta=1},
                                        HG2 = {natom = "CG"
                                               delta=1},
                                        HG3 = {natom = "CG"
                                               delta=1},
                                        QG = {natom = "CG"
                                              delta=1}),
                           GLY = switch(atom,
                                        HA2 = c(),
                                        HA3 = c(),
                                        QA = c()),
                           HIS = switch(atom,
                                        HB2 = {natom = "CB"
                                               delta=1},
                                        HB3 = {natom = "CB"
                                               delta=1},
                                        QB = {natom = "CB"
                                              delta=1},
                                        HD1 = c(),
                                        HD2 = c(),
                                        HE1 = c()),
                           ILE = switch(atom,
                                        HB = c(),
                                        HG21 = {natom = "QG2"
                                                delta=1},
                                        HG22 = {natom = "QG2"
                                                delta=1},
                                        HG23 = {natom = "QG2"
                                                delta=1},
                                        QG2 = c(),
                                        HG12 = {natom = "CG1"
                                                delta=1},
                                        HG13 = {natom = "CG1"
                                                delta=1},
                                        QG1 = {natom = "CG1"
                                               delta=1},
                                        HD11 = {natom = "QD1"
                                                delta=1},
                                        HD12 = {natom = "QD1"
                                                delta=1},
                                        HD13 = {natom = "QD1"
                                                delta=1},
                                        QD1 = c()),
                           LEU = switch(atom,
                                        HB2 = {natom = "CB"
                                               delta=1},
                                        HB3 = {natom = "CB"
                                               delta=1},
                                        QB = {natom = "CB"
                                              delta=1},
                                        HG = c(),
                                        HD11 = {natom = "QD1"
                                                delta=1},
                                        HD12 = {natom = "QD1"
                                                delta=1},
                                        HD13 = {natom = "QD1"
                                                delta=1},
                                        QD1 = c(),
                                        HD21 = {natom = "QD2"
                                                delta=1},
                                        HD22 = {natom = "QD2"
                                                delta=1},
                                        HD23 = {natom = "QD2"
                                                delta=1},
                                        QD2 = c(),
                                        QQD = c()),
                           LYS = switch(atom,
                                        HB2 = {natom = "CB"
                                               delta=1},
                                        HB3 = {natom = "CB"
                                               delta=1},
                                        QB = {natom = "CB"
                                              delta=1},
                                        HG2 = {natom = "CG"
                                               delta=1},
                                        HG3 = {natom = "CG"
                                               delta=1},
                                        QG = {natom = "CG"
                                              delta=1},
                                        HD2 = {natom = "CD"
                                               delta=1},
                                        HD3 = {natom = "CD"
                                               delta=1},
                                        QD = {natom = "CD"
                                              delta=1},
                                        HE2 = {natom = "CE"
                                               delta=1},
                                        HE3 = {natom = "CE"
                                               delta=1},
                                        QE = {natom = "CE"
                                              delta=1},
                                        HZ1 = {natom = "QZ"
                                               delta=1},
                                        HZ2 = {natom = "QZ"
                                               delta=1},
                                        HZ3 = {natom = "QZ"
                                               delta=1},
                                        QZ = c()),
                           MET = switch(atom,
                                        HB2 = {natom = "CB"
                                               delta=1},
                                        HB3 = {natom = "CB"
                                               delta=1},
                                        QB = {natom = "CB"
                                              delta=1},
                                        HG2 = {natom = "CG"
                                               delta=1},
                                        HG3 = {natom = "CG"
                                               delta=1},
                                        QG = {natom = "CG"
                                              delta=1},
                                        HE1 = {natom = "QE"
                                               delta=1},
                                        HE2 = {natom = "QE"
                                               delta=1},
                                        HE3 = {natom = "QE"
                                               delta=1},
                                        QE = c()),
                           PHE = switch(atom,
                                        HB2 = {natom = "CB"
                                               delta=1},
                                        HB3 = {natom = "CB"
                                               delta=1},
                                        QB = {natom = "CB"
                                              delta=1},
                                        HD1 = c(),
                                        HD2 = c(),
                                        QD = c(),
                                        HB2 = c(),
                                        HB3 = c(),
                                        QB = c(),
                                        HE1 = c(),
                                        HE2 = c(),
                                        QE = c(),
                                        QR = c(),
                                        HZ = c()),
                           PRO = switch(atom,
                                        HB2 = c(),
                                        HB3 = c(),
                                        QB = c(),
                                        HG2 = c(),
                                        HG3 = c(),
                                        QG = c(),
                                        HD2 = c(),
                                        HD3 = c(),
                                        QD = c()),
                           SER = switch(atom,
                                        HB2 = {natom = "CB"
                                               delta=1},
                                        HB3 = {natom = "CB"
                                               delta=1},
                                        QB = {natom = "CB"
                                              delta=1},
                                        HG = {natom = "OG"
                                              delta=1}),
                           THR = switch(atom,
                                        HB = c(),
                                        HG21 = {natom = "QG2"
                                                delta=1},
                                        HG22 = {natom = "QG2"
                                                delta=1},
                                        HG23 = {natom = "QG2"
                                                delta=1},
                                        QG2 = c(),
                                        HG1 = {natom = "OG1"
                                               delta=1}),
                           TRP = switch(atom,
                                        HB2 = {natom = "CB"
                                               delta=1},
                                        HB3 = {natom = "CB"
                                               delta=1},
                                        QB = {natom = "CB"
                                              delta=1},
                                        HD1 = c(),
                                        HE1 = c(),
                                        HE3 = c(),
                                        HZ2 = c(),
                                        HZ3 = c(),
                                        HH2 = c()),
                           TYR = switch(atom,
                                        HB2 = {natom = "CB"
                                               delta=1},
                                        HB3 = {natom = "CB"
                                               delta=1},
                                        QB = {natom = "CB"
                                              delta=1},
                                        HD1 = c(),
                                        HD2 = c(),
                                        QD = c(),
                                        HE1 = c(),
                                        HE2 = c(),
                                        QE = c(),
                                        QR = c(),
                                        HH = {natom = "OH"
                                              delta=1}),
                           VAL = switch(atom,
                                        HB = c(),
                                        HG11 = {natom = "QG1"
                                                delta=1},
                                        HG12 = {natom = "QG1"
                                                delta=1},
                                        HG13 = {natom = "QG1"
                                                delta=1},
                                        QG1 = c(),
                                        HG21 = {natom = "QG2"
                                                delta=1},
                                        HG22 = {natom = "QG2"
                                                delta=1},
                                        HG23 = {natom = "QG2"
                                                delta=1},
                                        QG2 = c(),
                                        QQG = c())),
         xplor = switch(residuetype,
                       ALA = switch(atom,
                                    HB1 = c(),
                                    HB2 = c(),
                                    HB3 = c(),
                                    QB = c()),
                       ARG = switch(atom,
                                    HB2 = c(),
                                    HB1 = {natom="HB3"
                                           delta=-1},
                                    QB = c(),
                                    HG2 = c(),
                                    HG1 = {natom="HG3"
                                           delta=-1},
                                    QG = c(),
                                    HD2 = c(),
                                    HD1 = {natom="HD3"
                                           delta=-1},
                                    QD = c(),
                                    HH11 = c(),
                                    HH12 = c(),
                                    QH1 = c(),
                                    HH21 = c(),
                                    HH22 = c(),
                                    QH2 = c()),
                       ASN = switch(atom,
                                    HB2 = c(),
                                    HB1 = {natom="HB3"
                                           delta=-1},
                                    QB = c(),
                                    HD21 = c(),
                                    HD22 = c(),
                                    QD2 = c()),
                       ASP = switch(atom,
                                    HB2 = c(),
                                    HB1 = {natom="HB3"
                                           delta=-1},
                                    QB = c()),
                       CYS = switch(atom,
                                    HB2 = c(),
                                    HB1 = {natom="HB3"
                                           delta=-1},
                                    HG = c(),
                                    QB = c()),
                       CYSS = switch(atom,
                                     HB2 = c(),
                                     HB1 = {natom="HB3"
                                            delta=-1},
                                     QB = c()),
                       GLN = switch(atom,
                                    HB2 = c(),
                                    HB1 = {natom="HB3"
                                           delta=-1},
                                    QB = c(),
                                    HG2 = c(),
                                    HG1 = {natom="HG3"
                                           delta=-1},
                                    QG = c(),
                                    HE21 = c(),
                                    HE22 = c(),
                                    QE2 = c()),
                       GLU = switch(atom,
                                    HB2 = c(),
                                    HB1 = {natom="HB3"
                                           delta=-1},
                                    QB = c(),
                                    HG2 = c(),
                                    HG1 = {natom="HG3"
                                           delta=-1},
                                    QG = c()),
                       GLY = switch(atom,
                                    HA2 = c(),
                                    HA1 = {natom="HA3"
                                           delta=-1},
                                    QA = c()),
                       HIS = switch(atom,
                                    HB2 = c(),
                                    HB1 = {natom="HB3"
                                           delta=-1},
                                    QB = c(),
                                    HD1 = c(),
                                    HD2 = c(),
                                    HE1 = c()),
                       ILE = switch(atom,
                                    HB = c(),
                                    HG21 = c(),
                                    HG22 = c(),
                                    HG23 = c(),
                                    QG2 = c(),
                                    HG12 = c(),
                                    HG11 = {natom="HG13"
                                            delta=-1},
                                    QG1 = c(),
                                    HD11 = c(),
                                    HD12 = c(),
                                    HD13 = c(),
                                    QD1 = c()),
                       LEU = switch(atom,
                                    HB2 = c(),
                                    HB1 = {natom="HB3"
                                           delta=-1},
                                    QB = c(),
                                    HG = c(),
                                    HD11 = c(),
                                    HD12 = c(),
                                    HD13 = c(),
                                    QD1 = c(),
                                    HD21 = c(),
                                    HD22 = c(),
                                    HD23 = c(),
                                    QD2 = c(),
                                    QQD = c()),
                       LYS = switch(atom,
                                    HB2 = c(),
                                    HB1 = {natom="HB3"
                                           delta=-1},
                                    QB = c(),
                                    HG2 = c(),
                                    HG1 = {natom="HG3"
                                           delta=-1},
                                    QG = c(),
                                    HD2 = c(),
                                    HD1 = {natom="HD3"
                                           delta=-1},
                                    QD = c(),
                                    HE2 = c(),
                                    HE1 = {natom="HE3"
                                           delta=-1},
                                    QE = c(),
                                    HZ1 = c(),
                                    HZ2 = c(),
                                    HZ3 = c(),
                                    QZ = c()),
                       MET = switch(atom,
                                    HB2 = c(),
                                    HB1 = {natom="HB3"
                                           delta=-1},
                                    QB = c(),
                                    HG2 = c(),
                                    HG1 = {natom="HG3"
                                           delta=-1},
                                    QG = c(),
                                    HE1 = c(),
                                    HE2 = c(),
                                    HE3 = c(),
                                    QE = c()),
                       PHE = switch(atom,
                                    HB2 = c(),
                                    HB1 = {natom="HB3"
                                           delta=-1},
                                    QB = c(),
                                    HD1 = c(),
                                    HD2 = c(),
                                    QD = c(),
                                    HB2 = c(),
                                    HB3 = c(),
                                    QB = c(),
                                    HE1 = c(),
                                    HE2 = c(),
                                    QE = c(),
                                    QR = c(),
                                    HZ = c()),
                       PRO = switch(atom,
                                    HB2 = c(),
                                    HB1 = {natom="HB3"
                                           delta=-1},
                                    QB = c(),
                                    HG2 = c(),
                                    HG1 = {natom="HG3"
                                           delta=-1},
                                    QG = c(),
                                    HD2 = c(),
                                    HD1 = {natom="HD3"
                                           delta=-1},
                                    QD = c()),
                       SER = switch(atom,
                                    HB2 = c(),
                                    HB1 = {natom="HB3"
                                           delta=-1},
                                    QB = c(),
                                    HG = c()),
                       THR = switch(atom,
                                    HB = c(),
                                    HG21 = c(),
                                    HG22 = c(),
                                    HG23 = c(),
                                    QG2 = c(),
                                    HG1 = c()),
                       TRP = switch(atom,
                                    HB2 = c(),
                                    HB1 = {natom="HB3"
                                           delta=-1},
                                    QB = c(),
                                    HD1 = c(),
                                    HE1 = c(),
                                    HE3 = c(),
                                    HZ2 = c(),
                                    HZ3 = c(),
                                    HH2 = c()),
                       TYR = switch(atom,
                                    HB2 = c(),
                                    HB1 = {natom="HB3"
                                           delta=-1},
                                    QB = c(),
                                    HD1 = c(),
                                    HD2 = c(),
                                    QD = c(),
                                    HE1 = c(),
                                    HE2 = c(),
                                    QE = c(),
                                    QR = c(),
                                    HH = c()),
                       VAL = switch(atom,
                                    HB = c(),
                                    HG11 = c(),
                                    HG12 = c(),
                                    HG13 = c(),
                                    QG1 = c(),
                                    HG21 = c(),
                                    HG22 = c(),
                                    HG23 = c(),
                                    QG2 = c(),
                                    QQG = c())))
  
  return(list(natom=natom, delta=delta))
}

