\name{find.maf}
\alias{find.maf}
\title{
Find minor allele frequency (MAF) in case-trios, control-trios, or the whole sample.
}
\description{
Compute MAF in child, parents and triad of case-trios, control-trios, or the whole sample. Minor allele normally refers to the disease allele, which has lower frequency than the alternative allele(s). Trios with partially missing data are included.
}
\usage{
find.maf(sample)
}
\arguments{
  \item{sample}{
case- and/or control-trios dataset with first column as disease status (1=case, 0=control), the second, third and forth columns as maternal, paternal and child genotypes. The fifth column, if entered as part of the input data, is the parent-of-origin indicator of disease allele in a heterozygous child (a value of 1 indicates the mother transmitted the disease allele and 0 indicates the father transmitted). The parent-of-origin indicator is only meaningful for a heterozygous child.
}
}
\value{
\item{cases}{MAF in affected children, parents, or triads of case-trios}
\item{ctrls}{MAF in unaffected children, parents, or triads of control-trios}
\item{sample}{MAF in children, parents, or triads of the whole sample}
}
\author{
Lam Opal Huang
}
\seealso{
\code{\link{rtrios}}
}
\examples{
trios=rtrios(100000,500,0.1,0.2,0.3,1,0.5,0.5,0.1)
find.maf(trios$case)
find.maf(trios$ctrl)
find.maf(rbind(trios$case,trios$ctrl))
}
