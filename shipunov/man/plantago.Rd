\name{plantago}
\alias{plantago}

\title{plantago}

\description{

Plantago (ribworts, plantains) species from European Russia:
morphological table (Shipunov, 1998).

All not applicable, unknown and "both" values are labeled as "NA".

}

\usage{
plantago
}

\format{

This data frame has species names as row names, and contains the
following columns (all variables are binary):

\describe{
\item{\code{V01}}{0 annuals or biennials, 1 perennials}
\item{\code{V02}}{0 not taller than 20 cm, 1 taller than 20 cm}
\item{\code{V03}}{0 aboveground stems herbaceous, 1 aboveground stems woody}
\item{\code{V04}}{0 vegetative nodes shortened, 1 vegetative nodes elongated}
\item{\code{V05}}{0 vegetative shoots do not branch, 1 vegetative shoots branch}
\item{\code{V06}}{0 phyllotaxis opposite, 1 phyllotaxis alternate}
\item{\code{V07}}{0 well developed green leaves <= 5, 1 more}
\item{\code{V08}}{0 the base of main shoot covered with remains of withered leaves, 1 the base is not covered with remains of withered leaves}
\item{\code{V09}}{0 rhizome > 1 cm diam, 1 rhizome thinner}
\item{\code{V10}}{0 slanted or horizontal rhizome, 1 vertical rhizome}
\item{\code{V11}}{0 main root fast degrading, 1 main root presents on adult plants}
\item{\code{V12}}{0 adventitious and lateral roots >= 1 mm diam, 1 less than 1 mm diam}
\item{\code{V13}}{0 heterophylly present, 1 leaves similar}
\item{\code{V14}}{0 leaves thin, transparent, 1 leaves not transparent}
\item{\code{V15}}{0 leaves darken when dry, 1 leaves do not darken, sometimes became yellow or brown}
\item{\code{V16}}{0 leaves (almost) naked, 1 leaves pubescent}
\item{\code{V17}}{0 leaves flat, 1 leaves section rounded or leaves with furrow}
\item{\code{V18}}{0 leaves with large teeth or even lobes, 1 leaves margin whole or with small teeth}
\item{\code{V19}}{0 leaves linear, 1 leaves more broad}
\item{\code{V20}}{0 leaves lanceolate, 1 leaves more broad}
\item{\code{V21}}{0 leaves obovate, 1 leaves elliptic or ovate}
\item{\code{V22}}{0 leaf tip blunt, 1 leaf tip sharp}
\item{\code{V23}}{0 leaf base broad, suddenly narrowing into petiole, 1 leaf base narrow, smoothly become a petiole}
\item{\code{V24}}{0 leaf margin with teeth, 1 leaf margin whole}
\item{\code{V25}}{0 leaf veins >=7, 1 < 7}
\item{\code{V26}}{0 petioles present, 1 petioles absent}
\item{\code{V27}}{0 petioles almost equal or a bit shorter than leaf blades, 1 petioles much shorter than blades}
\item{\code{V28}}{0 petioles without wings at the lowest 1/3 of length, 1 petioles with wings at the lowest 1/3 of length}
\item{\code{V29}}{0 stalks horizontal or arcuate, 1 stalks straight or curved}
\item{\code{V30}}{0 stalks naked, 1 stalks pubescent}
\item{\code{V31}}{0 stalks with ribs, 1 stalks without ribs}
\item{\code{V32}}{0 spikes longer, equal or slightly shorter than stalks, 1 spikes much shorter than stalks}
\item{\code{V33}}{0 spikes long cylindrical or tail-like, 1 spikes rounded or short cylindrical}
\item{\code{V34}}{0 lower bracts are much much broader than others, 1 all bracts similar}
\item{\code{V35}}{0 middle and upper bracts not longer than sepals, 1 longer than sepals}
\item{\code{V36}}{0 bracts with sharp tip, 1 bracts with blunt tip}
\item{\code{V37}}{0 bract width >= length, 1 bract length > width}
\item{\code{V38}}{0 bracts pubescent, 1 bracts naked}
\item{\code{V39}}{0 bracts awned, 1 bracts not awned}
\item{\code{V40}}{0 flowers slanted, spike lax, 1 flowers appressed, spike dense}
\item{\code{V41}}{0 outer and inner sepals significantly different, 1 all sepals more or less similar}
\item{\code{V42}}{0 sepals narrow, 1 sepals broad}
\item{\code{V43}}{0 sepals with sharp tip, 1 sepals with blunt tip}
\item{\code{V44}}{0 sepals pubescent, 1 sepals naked}
\item{\code{V45}}{0 outer sepals fused, 1 outer sepals separate}
\item{\code{V46}}{0 corolla tube pubescent, 1 corolla tube naked}
\item{\code{V47}}{0 corolla lobes broad, elliptic or rounded, 1 corolla lobes narrow, oblanceolate or lanceolate}
\item{\code{V48}}{0 corolla lobes with sharp tip, 1 corolla lobes with blunt tip}
\item{\code{V49}}{0 corolla lobes white or silver, 1 corolla lobes yellowish or brownish}
\item{\code{V50}}{0 stamens not exserted, 1 stamens exserted}
\item{\code{V51}}{0 filaments yellowish or brownish, 1 filaments white, pinkish or purplish}
\item{\code{V52}}{0 pollen grains with thickened pore margin, 1 pollen grains without thickened pore margin}
\item{\code{V53}}{0 pollen grains with >= 9 pores, 1 pollen grains with < 9 pores}
\item{\code{V54}}{0 pyxidium ovate or broadly conical, 1 pyxidium narrowly conical or elongated}
\item{\code{V55}}{0 one or two seeds are much smaller than others, 1 all seeds similar}
\item{\code{V56}}{0 seeds =< 2, 1 seeds > 2}
\item{\code{V57}}{0 seeds 3--5, 1 seeds >= 6}
\item{\code{V58}}{0 seeds flattened, 1 seeds rounded or angled}
\item{\code{V59}}{0 plane which passes through embryo cotyledons is perpendicular to placenta, 1 plane which passes through embryo cotyledons is parallel to placenta}
\item{\code{V60}}{0 polyploids, 1 diploids}
\item{\code{V61}}{0 x=5, 1 x=6}
}}

\source{

Shipunov A. 1998. Plantains (genera Plantago L. and Psyllium Mill.,
Plantaginaceae) of European Russia and adjacent territories. Ph. D.
Thesis. Moscow State University.

}

\examples{
plot(hclust(dist(plantago, method="binary")))
}

\keyword{datasets}
