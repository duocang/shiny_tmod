```bash
R version 3.3.3 (2017-03-06) -- "Another Canoe"
Copyright (C) 2017 The R Foundation for Statistical Computing
Platform: x86_64-apple-darwin13.4.0 (64-bit)

R ist freie Software und kommt OHNE JEGLICHE GARANTIE.
Sie sind eingeladen, es unter bestimmten Bedingungen weiter zu verbreiten.
Tippen Sie 'license()' or 'licence()' für Details dazu.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Tippen Sie 'demo()' für einige Demos, 'help()' für on-line Hilfe, oder
'help.start()' für eine HTML Browserschnittstelle zur Hilfe.
Tippen Sie 'q()', um R zu verlassen.

[Vorher gesicherter Workspace wiederhergestellt]

> t1 <- read.csv("
1.csv          readme.R       tt_2_S.csv     tt_4_TBaS.csv
2.csv          tt_1_TB.csv    tt_3_TBvS.csv
> t1 <- read.csv("tt_1_TB.csv")
> head(t1)
             X Row Col Start
1  A_24_P66027   1  12   978
2  A_32_P77178   1  13     0
3 A_23_P212522   1  14  6520
4 A_24_P934473   1  15  1287
5   A_24_P9671   1  16  1051
6  A_32_P29551   1  17     0
                                                      Sequence ProbeUID
1 GCTGCCCGCATCTATGATTACGACCCCCTATATAAGGAGGCGCTGCAAATGCTGCGGGAT        3
2 GGTCAATTTTTGGTCAAAAGTACAGAGAGCATAGAATAAAAGCAAAGATGTGAATGTCTC        5
3 ATTTTCTAACTGTCCTCTTTCTTGGGTCTAAAGCTCATAATACACAAAGGCTTCCAGACC        7
4 AAGCCAAGTACTTTAGAGAAGAAAAACGGTCTCAGCTGAACCTGTAGTGAGAGCATGCAG        9
5 ATCCAGGTCAGATTGTCAAGCATGGAGATATCAAGTGTGTACTAAATGAAGGCATGCCAA       11
6 CCTCTGTCTGGCTTTCTGGATCCTTAGATGAATTGCAGTTGGATTGGAATTTGGCACAAA       13
  ControlType    ProbeName     GeneName SystematicName
1           0  A_24_P66027     APOBEC3B      NM_004900
2           0  A_32_P77178  A_32_P77178    A_32_P77178
3           0 A_23_P212522       ATP11B      NM_014616
4           0 A_24_P934473 LOC100132006       BX537921
5           0   A_24_P9671       DNAJA1      NM_001539
6           0  A_32_P29551  A_32_P29551    A_32_P29551
                                                                                                        Description
1 ref|Homo sapiens apolipoprotein B mRNA editing enzyme, catalytic polypeptide-like 3B (APOBEC3B), mRNA [NM_004900]
2                                                                                                           Unknown
3                                            ref|Homo sapiens ATPase, class VI, type 11B (ATP11B), mRNA [NM_014616]
4                                  gb|Homo sapiens mRNA; cDNA DKFZp686E20147 (from clone DKFZp686E20147) [BX537921]
5                           ref|Homo sapiens DnaJ (Hsp40) homolog, subfamily A, member 1 (DNAJA1), mRNA [NM_001539]
6                                                                                                           Unknown
        logFC          t         msd         SE           d        ciL
1 -0.76959856 -3.8927979  0.37077998 0.19769805 -0.35806445 -1.1684171
2  0.04476325  0.2928927 -0.26354567 0.15283158  0.01610013 -0.2635457
3  1.06696108  6.1307780  0.71588118 0.17403355  0.43699467  0.7158812
4 -0.50767548 -8.8978381  0.39257565 0.05705605 -0.06816830 -0.6227753
5  1.46592639  9.7462445  1.16250385 0.15040936  0.51889752  1.1625038
6 -0.11563806 -1.4251830 -0.04804478 0.08113909 -0.02208134 -0.2793209
          ciR         qval
1 -0.37077998 1.030480e-03
2  0.35307217 8.392085e-01
3  1.41804099 1.390000e-06
4 -0.39257565 4.150000e-10
5  1.76934894 3.990000e-11
6  0.04804478 2.515534e-01
> t1 <- t1[, c("ProbeName", "GeneName", "logFC", "msd", "qval")]
> head(t1)
     ProbeName     GeneName       logFC         msd         qval
1  A_24_P66027     APOBEC3B -0.76959856  0.37077998 1.030480e-03
2  A_32_P77178  A_32_P77178  0.04476325 -0.26354567 8.392085e-01
3 A_23_P212522       ATP11B  1.06696108  0.71588118 1.390000e-06
4 A_24_P934473 LOC100132006 -0.50767548  0.39257565 4.150000e-10
5   A_24_P9671       DNAJA1  1.46592639  1.16250385 3.990000e-11
6  A_32_P29551  A_32_P29551 -0.11563806 -0.04804478 2.515534e-01
> t1 <- t1[ t1$ProbeName != t1$GeneName, ]
Fehler in Ops.factor(t1$ProbeName, t1$GeneName) :
  level sets of factors are different
> t1 <- read.csv("tt_1_TB.csv", stringsAsFactors=FALSE)
> t1 <- t1[, c("ProbeName", "GeneName", "logFC", "msd", "qval")]
> t1 <- t1[ t1$ProbeName != t1$GeneName, ]
> head(t1)
     ProbeName     GeneName      logFC       msd        qval
1  A_24_P66027     APOBEC3B -0.7695986 0.3707800 1.03048e-03
3 A_23_P212522       ATP11B  1.0669611 0.7158812 1.39000e-06
4 A_24_P934473 LOC100132006 -0.5076755 0.3925757 4.15000e-10
5   A_24_P9671       DNAJA1  1.4659264 1.1625038 3.99000e-11
7 A_24_P801451        EHMT2 -0.5620778 0.4406253 1.23000e-10
8  A_32_P30710        RPL23  1.5344275 1.0618922 3.91000e-07
> dim(t1)
[1] 33280     5
> library(tmod)
> data(tmod)
> head(tmod$
tmod$GENES          tmod$MODULES
tmod$GENES2MODULES  tmod$MODULES2GENES
> head(tmod$GENES)
       ID original.ID                        Description ENTREZ       REFSEQ
CCL2 CCL2        CCL2     chemokine (C-C motif) ligand 2   6347    NM_002982
DCN   DCN         DCN                            decorin   1634    NM_001920
LIF   LIF         LIF         leukemia inhibitory factor   3976 NM_001257135
PLAU PLAU        PLAU   plasminogen activator, urokinase   5328 NM_001145031
IL6   IL6         IL6 interleukin 6 (interferon, beta 2)   3569    NM_000600
MGP   MGP         MGP                 matrix Gla protein   4256    NM_000900
> t1 <- t1[ t1$GeneName %in% tmod$GENES$ID, ]
> dim(t1)
[1] 17001     5
> write.csv(t1, file="
1.csv          readme.R       tt_2_S.csv     tt_4_TBaS.csv
2.csv          tt_1_TB.csv    tt_3_TBvS.csv
> write.csv(t1, file="tt_1_TB_reduced.csv")
> is.null(list())
[1] FALSE
> t0 <- t1[ t1$ProbeName != t1$GeneName, ]
> head(t0)
      ProbeName GeneName      logFC       msd        qval
1   A_24_P66027 APOBEC3B -0.7695986 0.3707800 1.03048e-03
3  A_23_P212522   ATP11B  1.0669611 0.7158812 1.39000e-06
5    A_24_P9671   DNAJA1  1.4659264 1.1625038 3.99000e-11
7  A_24_P801451    EHMT2 -0.5620778 0.4406253 1.23000e-10
8   A_32_P30710    RPL23  1.5344275 1.0618922 3.91000e-07
11  A_32_P86028    RPS13  1.1083269 0.7598460 5.86000e-07
> t0 <- read.csv("tt_1_TB.csv")
head> head(t0)
             X Row Col Start
1  A_24_P66027   1  12   978
2  A_32_P77178   1  13     0
3 A_23_P212522   1  14  6520
4 A_24_P934473   1  15  1287
5   A_24_P9671   1  16  1051
6  A_32_P29551   1  17     0
                                                      Sequence ProbeUID
1 GCTGCCCGCATCTATGATTACGACCCCCTATATAAGGAGGCGCTGCAAATGCTGCGGGAT        3
2 GGTCAATTTTTGGTCAAAAGTACAGAGAGCATAGAATAAAAGCAAAGATGTGAATGTCTC        5
3 ATTTTCTAACTGTCCTCTTTCTTGGGTCTAAAGCTCATAATACACAAAGGCTTCCAGACC        7
4 AAGCCAAGTACTTTAGAGAAGAAAAACGGTCTCAGCTGAACCTGTAGTGAGAGCATGCAG        9
5 ATCCAGGTCAGATTGTCAAGCATGGAGATATCAAGTGTGTACTAAATGAAGGCATGCCAA       11
6 CCTCTGTCTGGCTTTCTGGATCCTTAGATGAATTGCAGTTGGATTGGAATTTGGCACAAA       13
  ControlType    ProbeName     GeneName SystematicName
1           0  A_24_P66027     APOBEC3B      NM_004900
2           0  A_32_P77178  A_32_P77178    A_32_P77178
3           0 A_23_P212522       ATP11B      NM_014616
4           0 A_24_P934473 LOC100132006       BX537921
5           0   A_24_P9671       DNAJA1      NM_001539
6           0  A_32_P29551  A_32_P29551    A_32_P29551
                                                                                                        Description
1 ref|Homo sapiens apolipoprotein B mRNA editing enzyme, catalytic polypeptide-like 3B (APOBEC3B), mRNA [NM_004900]
2                                                                                                           Unknown
3                                            ref|Homo sapiens ATPase, class VI, type 11B (ATP11B), mRNA [NM_014616]
4                                  gb|Homo sapiens mRNA; cDNA DKFZp686E20147 (from clone DKFZp686E20147) [BX537921]
5                           ref|Homo sapiens DnaJ (Hsp40) homolog, subfamily A, member 1 (DNAJA1), mRNA [NM_001539]
6                                                                                                           Unknown
        logFC          t         msd         SE           d        ciL
1 -0.76959856 -3.8927979  0.37077998 0.19769805 -0.35806445 -1.1684171
2  0.04476325  0.2928927 -0.26354567 0.15283158  0.01610013 -0.2635457
3  1.06696108  6.1307780  0.71588118 0.17403355  0.43699467  0.7158812
4 -0.50767548 -8.8978381  0.39257565 0.05705605 -0.06816830 -0.6227753
5  1.46592639  9.7462445  1.16250385 0.15040936  0.51889752  1.1625038
6 -0.11563806 -1.4251830 -0.04804478 0.08113909 -0.02208134 -0.2793209
          ciR         qval
1 -0.37077998 1.030480e-03
2  0.35307217 8.392085e-01
3  1.41804099 1.390000e-06
4 -0.39257565 4.150000e-10
5  1.76934894 3.990000e-11
6  0.04804478 2.515534e-01
> head(t1)
      ProbeName GeneName      logFC       msd        qval
1   A_24_P66027 APOBEC3B -0.7695986 0.3707800 1.03048e-03
3  A_23_P212522   ATP11B  1.0669611 0.7158812 1.39000e-06
5    A_24_P9671   DNAJA1  1.4659264 1.1625038 3.99000e-11
7  A_24_P801451    EHMT2 -0.5620778 0.4406253 1.23000e-10
8   A_32_P30710    RPL23  1.5344275 1.0618922 3.91000e-07
11  A_32_P86028    RPS13  1.1083269 0.7598460 5.86000e-07
> foo <- lapply(list(t0, t1), colnames)
> foo
[[1]]
 [1] "X"              "Row"            "Col"            "Start"
 [5] "Sequence"       "ProbeUID"       "ControlType"    "ProbeName"
 [9] "GeneName"       "SystematicName" "Description"    "logFC"
[13] "t"              "msd"            "SE"             "d"
[17] "ciL"            "ciR"            "qval"

[[2]]
[1] "ProbeName" "GeneName"  "logFC"     "msd"       "qval"

> intersect(1:10, 8:20)
[1]  8  9 10
> foo <- lapply(list(t0, t1), colnames)
> foo <- c(foo, c("logFC", "msd", "blah"))
> foo
[[1]]
 [1] "X"              "Row"            "Col"            "Start"
 [5] "Sequence"       "ProbeUID"       "ControlType"    "ProbeName"
 [9] "GeneName"       "SystematicName" "Description"    "logFC"
[13] "t"              "msd"            "SE"             "d"
[17] "ciL"            "ciR"            "qval"

[[2]]
[1] "ProbeName" "GeneName"  "logFC"     "msd"       "qval"

[[3]]
[1] "logFC"

[[4]]
[1] "msd"

[[5]]
[1] "blah"

> foo <- lapply(list(t0, t1), colnames)
> foo[[3]] <- foo[[1]]
> foo
[[1]]
 [1] "X"              "Row"            "Col"            "Start"
 [5] "Sequence"       "ProbeUID"       "ControlType"    "ProbeName"
 [9] "GeneName"       "SystematicName" "Description"    "logFC"
[13] "t"              "msd"            "SE"             "d"
[17] "ciL"            "ciR"            "qval"

[[2]]
[1] "ProbeName" "GeneName"  "logFC"     "msd"       "qval"

[[3]]
 [1] "X"              "Row"            "Col"            "Start"
 [5] "Sequence"       "ProbeUID"       "ControlType"    "ProbeName"
 [9] "GeneName"       "SystematicName" "Description"    "logFC"
[13] "t"              "msd"            "SE"             "d"
[17] "ciL"            "ciR"            "qval"

> write.csv(t1, file="tt_1_TB_reduced.csv", col)
col               colSums           colorConverter    colors
col2rgb           colnames          colorRamp         colorspaces
colMeans          colnames<-        colorRampPalette  colours
> write.csv(t1, file="tt_1_TB_reduced.csv", colnames=FALSE)
Fehler in write.table(t1, file = "tt_1_TB_reduced.csv", colnames = FALSE,  :
  unbenutztes Argument (colnames = FALSE)
> write.csv(t1, file="tt_1_TB_reduced.csv", col.names=FALSE)
Warnmeldung:
In write.csv(t1, file = "tt_1_TB_reduced.csv", col.names = FALSE) :
  Versuch ignoriert 'col.names' zu setzen
> write.csv(t1, file="tt_1_TB_reduced.csv", row=FALSE)
row                     row.names<-.data.frame  rownames<-
row.names               row.names<-.default     rowsum
row.names.data.frame    rowMeans                rowsum.data.frame
row.names.default       rowSums                 rowsum.default
row.names<-             rownames
> write.csv(t1, file="tt_1_TB_reduced.csv", rownames=FALSE)
Fehler in write.table(t1, file = "tt_1_TB_reduced.csv", rownames = FALSE,  :
  unbenutztes Argument (rownames = FALSE)
> write.csv(t1, file="tt_1_TB_reduced.csv", row.names=FALSE)
> cd /Users/song/code/shiny/example/tt_1_TB_reduced.csv
Fehler: Objekt 'cd' nicht gefunden
> tmodCERNOtest(t1$GeneName[ order(t1$msd, decreasing=T)])
               ID                                     Title     cerno  N1
LI.M32.1 LI.M32.1                  platelet activation (II) 140.25899  21
LI.M32.2 LI.M32.2                   CORO1A-DEF6 network (I) 114.37612  19
LI.M32.0 LI.M32.0                   platelet activation (I) 122.52223  22
LI.M100   LI.M100                       MAPK, RAS signaling  68.04119  10
LI.M63     LI.M63           regulation of localization (GO)  74.31434  12
LI.M136   LI.M136                                       TBA  86.38922  16
LI.M32.7 LI.M32.7                                       TBA  66.84306  11
LI.M32.8 LI.M32.8                   cytoskeletal remodeling  63.20795  10
LI.M32.4 LI.M32.4                  CORO1A-DEF6 network (II)  73.39938  14
LI.M32.3 LI.M32.3                     KLF12 targets network  75.74043  16
LI.M167   LI.M167                    enriched in cell cycle  71.79312  15
LI.M188   LI.M188                                       TBA  52.69617  10
LI.M32.6 LI.M32.6                                       TBA  60.06689  13
LI.M192   LI.M192                                       TBA  55.98512  12
LI.M248   LI.M248                                       TBA  46.48650   9
LI.M75     LI.M75                   antiviral IFN signature  83.72989  22
LI.M215   LI.M215 small GTPase mediated signal transduction  61.88156  15
LI.M55     LI.M55                                       TBA  51.60176  12
LI.M11.0 LI.M11.0                enriched in monocytes (II) 451.22768 181
LI.M32.5 LI.M32.5                                       TBA  58.81585  15
LI.M226   LI.M226                                proteasome  48.91332  12
LI.M120   LI.M120                                       TBA  44.40311  11
               AUC      cES      P.Value    adj.P.Val
LI.M32.1 0.8442387 3.339500 1.650351e-12 5.710214e-10
LI.M32.2 0.9045088 3.009898 1.406435e-09 2.433133e-07
LI.M32.0 0.8245468 2.784596 2.478871e-09 2.858964e-07
LI.M100  0.8405819 3.402059 3.791148e-07 3.279343e-05
LI.M63   0.8192424 3.096431 4.767788e-07 3.299309e-05
LI.M136  0.8039019 2.699663 6.817799e-07 3.931597e-05
LI.M32.7 0.8983713 3.038321 2.056213e-06 9.709830e-05
LI.M32.8 0.8238091 3.160398 2.245047e-06 9.709830e-05
LI.M32.4 0.8467068 2.621407 6.150221e-06 2.364418e-04
LI.M32.3 0.8107637 2.366888 2.085901e-05 7.217217e-04
LI.M167  0.8027651 2.393104 2.781633e-05 8.749501e-04
LI.M188  0.6613103 2.634808 9.006667e-05 2.596922e-03
LI.M32.6 0.7488184 2.310265 1.642600e-04 4.371844e-03
LI.M192  0.7358139 2.332713 2.302389e-04 5.690190e-03
LI.M248  0.7948174 2.582583 2.518472e-04 5.809274e-03
LI.M75   0.7615854 1.902952 2.842169e-04 6.146190e-03
LI.M215  0.6461251 2.062719 5.416506e-04 1.102418e-02
LI.M55   0.7879933 2.150074 8.815068e-04 1.694452e-02
LI.M11.0 0.5797928 1.246485 9.627760e-04 1.753266e-02
LI.M32.5 0.6866882 1.960528 1.277767e-03 2.210537e-02
LI.M226  0.8425741 2.038055 1.942269e-03 3.200119e-02
LI.M120  0.6786369 2.018323 3.156959e-03 4.965036e-02
> tmodCERNOtest(t0$GeneName[ order(t0$msd, decreasing=T)])
                 ID                                                  Title
LI.M37.0   LI.M37.0                    immune activation - generic cluster
LI.M11.0   LI.M11.0                             enriched in monocytes (II)
LI.M32.1   LI.M32.1                               platelet activation (II)
LI.M32.2   LI.M32.2                                CORO1A-DEF6 network (I)
LI.M32.0   LI.M32.0                                platelet activation (I)
LI.M4.0     LI.M4.0                           cell cycle and transcription
LI.S4         LI.S4                             Monocyte surface signature
LI.M136     LI.M136                                                    TBA
LI.M63       LI.M63                        regulation of localization (GO)
LI.M100     LI.M100                                    MAPK, RAS signaling
LI.M32.7   LI.M32.7                                                    TBA
LI.M32.4   LI.M32.4                               CORO1A-DEF6 network (II)
LI.M32.3   LI.M32.3                                  KLF12 targets network
LI.M32.8   LI.M32.8                                cytoskeletal remodeling
LI.M5.0     LI.M5.0 regulation of antigen presentation and immune response
LI.M167     LI.M167                                 enriched in cell cycle
LI.M75       LI.M75                                antiviral IFN signature
LI.M118.0 LI.M118.0                             enriched in monocytes (IV)
LI.M32.6   LI.M32.6                                                    TBA
LI.M16       LI.M16                         TLR and inflammatory signaling
LI.M188     LI.M188                                                    TBA
LI.M192     LI.M192                                                    TBA
LI.M248     LI.M248                                                    TBA
LI.M215     LI.M215              small GTPase mediated signal transduction
LI.M55       LI.M55                                                    TBA
LI.M32.5   LI.M32.5                                                    TBA
LI.M226     LI.M226                                             proteasome
LI.M216     LI.M216   respiratory electron transport chain (mitochondrion)
LI.M219     LI.M219   respiratory electron transport chain (mitochondrion)
LI.M120     LI.M120                                                    TBA
LI.M174     LI.M174                                                    TBA
LI.M238     LI.M238   respiratory electron transport chain (mitochondrion)
LI.M165     LI.M165             enriched in activated dendritic cells (II)
LI.M113     LI.M113                                     golgi membrane (I)
LI.M250     LI.M250                                            spliceosome
LI.M144     LI.M144                                cell cycle, ATP binding
LI.M127     LI.M127                             type I interferon response
LI.M40       LI.M40                  complement and other receptors in DCs
LI.M72.0   LI.M72.0                                                    TBA
LI.M4.3     LI.M4.3       myeloid cell enriched receptors and transporters
LI.M72.1   LI.M72.1                                                    TBA
LI.M48       LI.M48                                                    TBA
LI.M117     LI.M117                                     cell adhesion (GO)
LI.M118.1 LI.M118.1                        enriched in monocytes (surface)
LI.M156.0 LI.M156.0                plasma cells & B cells, immunoglobulins
LI.M128     LI.M128                                                    TBA
LI.M76       LI.M76                                             DNA repair
LI.M67       LI.M67                              activated dendritic cells
LI.S10       LI.S10               Resting dendritic cell surface signature
LI.M37.2   LI.M37.2                                  endoplasmic reticulum
LI.M23       LI.M23              RA, WNT, CSF receptors network (monocyte)
LI.M237     LI.M237                                    golgi membrane (II)
LI.M185     LI.M185                                                    TBA
LI.M72.2   LI.M72.2                                                    TBA
LI.M214     LI.M214                                                    TBA
LI.M153     LI.M153                                                    TBA
LI.M147     LI.M147                                intracellular transport
LI.M227     LI.M227                                 translation initiation
LI.M2.1     LI.M2.1                              extracellular matrix (II)
LI.M137     LI.M137                                                    TBA
               cerno  N1       AUC      cES      P.Value    adj.P.Val
LI.M37.0  1016.87461 320 0.6578104 1.588867 1.210212e-19 4.187334e-17
LI.M11.0   620.20165 181 0.6946317 1.713264 7.332673e-16 1.268553e-13
LI.M32.1   159.12933  21 0.8757946 3.788794 1.574626e-15 1.816069e-13
LI.M32.2   134.47308  19 0.9460899 3.538765 1.052846e-12 8.452263e-11
LI.M32.0   144.55455  22 0.8846976 3.285331 1.221425e-12 8.452263e-11
LI.M4.0    889.98815 314 0.6605347 1.417179 2.391333e-11 1.379002e-09
LI.S4      320.16832  90 0.7311278 1.778713 6.181018e-10 3.055189e-08
LI.M136    102.20766  16 0.8638720 3.193989 2.900440e-09 1.254440e-07
LI.M63      85.29818  12 0.8639458 3.554091 8.525998e-09 3.277773e-07
LI.M100     77.55127  10 0.9096588 3.877564 1.018281e-08 3.523254e-07
LI.M32.7    78.49664  11 0.9429988 3.568029 2.858850e-08 8.992384e-07
LI.M32.4    88.35605  14 0.9098567 3.155573 3.574115e-08 1.030537e-06
LI.M32.3    93.04473  16 0.8906742 2.907648 7.195772e-08 1.915182e-06
LI.M32.8    72.24407  10 0.8586572 3.612204 7.801863e-08 1.928175e-06
LI.M5.0    263.71138  77 0.6746456 1.712412 8.937589e-08 2.061604e-06
LI.M167     87.11743  15 0.8690588 2.903914 1.784029e-07 3.857962e-06
LI.M75     108.01484  22 0.8625793 2.454883 2.656426e-07 5.406609e-06
LI.M118.0  197.68206  54 0.7358494 1.830389 3.150329e-07 6.055633e-06
LI.M32.6    72.92687  13 0.8246115 2.804879 2.464149e-06 4.487345e-05
LI.M16     148.87724  40 0.7291598 1.860965 4.705222e-06 8.140034e-05
LI.M188     60.92412  10 0.7554527 3.046206 5.118628e-06 8.433548e-05
LI.M192     67.31722  12 0.8102942 2.804884 5.530820e-06 8.698472e-05
LI.M248     56.03937   9 0.8849678 3.113298 8.785773e-06 1.321686e-04
LI.M215     75.22306  15 0.7427631 2.507435 9.379902e-06 1.352269e-04
LI.M55      65.06166  12 0.8817575 2.710903 1.192130e-05 1.649909e-04
LI.M32.5    73.75499  15 0.7901957 2.458500 1.498763e-05 1.994508e-04
LI.M226     62.61131  12 0.9121995 2.608805 2.708687e-05 3.471132e-04
LI.M216     54.62265  11 0.7996015 2.482848 1.343462e-04 1.660135e-03
LI.M219     72.30725  17 0.7557584 2.126684 1.405243e-04 1.676600e-03
LI.M120     53.96129  11 0.7479604 2.452786 1.665651e-04 1.921051e-03
LI.M174     85.42474  22 0.8373489 1.941471 1.823903e-04 2.035711e-03
LI.M238     70.33922  17 0.8477148 2.068801 2.464610e-04 2.664859e-03
LI.M165    117.42349  35 0.7003983 1.677478 3.340839e-04 3.502819e-03
LI.M113     45.48516   9 0.8934340 2.526953 3.525273e-04 3.587484e-03
LI.M250     50.37950  11 0.8374469 2.289977 5.210093e-04 5.138392e-03
LI.M144     64.75627  16 0.8485002 2.023634 5.346304e-04 5.138392e-03
LI.M127     53.12598  12 0.7679849 2.213582 5.567437e-04 5.206306e-03
LI.M40      55.86287  13 0.7491445 2.148572 5.878147e-04 5.352208e-03
LI.M72.0    75.36764  20 0.8292391 1.884191 6.041812e-04 5.360172e-03
LI.M4.3    100.80596  30 0.7091128 1.680099 7.662417e-04 6.627990e-03
LI.M72.1    62.43762  16 0.8445263 1.951175 1.013592e-03 8.553725e-03
LI.M48      50.58541  12 0.7247854 2.107725 1.192080e-03 9.820467e-03
LI.M117     74.73199  21 0.6268934 1.779333 1.396804e-03 1.123940e-02
LI.M118.1   63.60590  17 0.7776763 1.870762 1.543181e-03 1.213502e-02
LI.M156.0   84.55856  25 0.6207887 1.691171 1.628724e-03 1.252308e-02
LI.M128     46.37490  11 0.8400708 2.107950 1.769180e-03 1.308462e-02
LI.M76      76.37141  22 0.7091464 1.735714 1.777390e-03 1.308462e-02
LI.M67      46.03598  11 0.8009179 2.092545 1.956551e-03 1.410347e-02
LI.S10     197.13896  72 0.6100882 1.369021 2.182257e-03 1.523144e-02
LI.M37.2    64.91703  18 0.7251377 1.803251 2.201076e-03 1.523144e-02
LI.M23      48.36713  12 0.6741209 2.015297 2.272688e-03 1.541863e-02
LI.M237     60.77912  17 0.7906792 1.787621 3.187242e-03 2.120742e-02
LI.M185     48.99906  13 0.8319546 1.884579 4.131125e-03 2.696923e-02
LI.M72.2    46.17067  12 0.8418563 1.923778 4.222585e-03 2.705582e-02
LI.M214     45.86037  12 0.8338718 1.910849 4.601180e-03 2.894560e-02
LI.M153     53.84678  15 0.8039269 1.794893 4.781522e-03 2.954298e-02
LI.M147     58.92335  17 0.7908340 1.733040 5.049847e-03 3.065345e-02
LI.M227     39.84624  10 0.8392455 1.992312 5.223777e-03 3.116253e-02
LI.M2.1    126.51897  45 0.5493387 1.405766 6.749744e-03 3.958325e-02
LI.M137     51.57642  15 0.7949171 1.719214 8.457567e-03 4.877197e-02
>
```