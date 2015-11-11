\name{strata.cnt}
\alias{strata.cnt}
\title{
Count trios for each of the 15/16 strata with mother-father-child genotypes MFC
}
\description{
Count trios with mother-father-child genotypes MFC (additive model) as
mating type 1: 222;
mating type 2: 212,122,211,121;
mating type 3: 201,021;
mating type 4: 112,(111M,111F) or 111,110;
mating type 5: 101,100,011,010;
mating type 6: 000;
where 111M and 111F are for categories with heterozygous children inherited the disease allele from the mother and father, respectively. Additive model means the genotype is the number of copies of minor allele that the individual carries assuming a bi-allelic locus. This genotype will be 0 for homozygous wild-type, 1 for heterozygous, and 2 for homozygous mutant. Note that trios with missing data are discarded.
}
\usage{
strata.cnt(sample)
}
\arguments{
  \item{sample}{
case- and/or control-trios dataset with first column as disease status (1=case, 0=control), the second, third and forth columns as maternal, paternal and child genotypes. The fifth column, if entered as part of the input data, is the parent-of-origin indicator of disease allele in a heterozygous child (a value of 1 indicates the mother transmitted the disease allele and 0 indicates the father transmitted). The parent-of-origin indicator is only meaningful for a heterozygous child.
}
}
\value{
\item{cnt.MFC}{count of stratum with genotypes MFC (additive model)}
}
\author{
Lam Opal Huang
}
\seealso{
\code{\link{rtrios}}
\code{\link{ll}}
}
\examples{
trios=rtrios(100000,500,0.1,0.2,0.3,1,0.5,0.5,0.1)
strata.cnt(trios$case)
strata.cnt(trios$ctrl)
}
