#### Conversion to full genome

Take into account that if the input curve is the one measured by FISH, it will be converted to its full genome equivalent. This is automatic if you load the the curve from an `.RDS` file; if you input the curve manually you will need to provide the genomic conversion factor.

This is done because in order to estimate doses, full genomic fitting curves are required.
