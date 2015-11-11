\name{gcount}
\alias{gcount}
\title{
Calculate genotype counts (additive model) for selected population.
}
\description{
Calculate genotype counts (additive model) of case-trios, control-trios and the whole sample, separately for children, mothers and fathers. Additive model means the genotype is the number of copies of minor allele that the individual carries assuming a bi-allelic locus. This genotype will be 0 for homozygous wild-type, 1 for heterozygous, and 2 for homozygous mutant.  Minor allele normally refers to the disease allele, which has lower frequency than the alternative allele(s). Trios with partially missing data are included.
}
\usage{
gcount(sample)
}
\arguments{
  \item{sample}{
case- and/or control-trios dataset with first column as disease status (1=case, 0=control), the second, third and forth columns as maternal, paternal and child genotypes. The fifth column, if entered as part of the input data, is the parent-of-origin indicator of disease allele in a heterozygous child (a value of 1 indicates the mother transmitted the disease allele and 0 indicates the father transmitted). The parent-of-origin indicator is only meaningful for a heterozygous child.
}
}
\value{
\item{cases}{genotype counts of children, mothers and fathers, in case-trios}
\item{ctrls}{genotype counts of children, mothers and fathers, in control-trios}
\item{sample}{genotype counts of children, mothers and fathers, in the whole sample}
}
\author{
Lam Opal Huang
}
\seealso{
\code{\link{rtrios}}
}
\examples{
trios=rtrios(100000,500,0.1,0.2,0.3,1,0.5,0.5,0.1)
gcount(trios$case)
gcount(trios$ctrl)
gcount(rbind(trios$case,trios$ctrl))
}
