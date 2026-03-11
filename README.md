# Index Based Drought Monitoring

Explorations in using the full spectrum of MODIS bands and contemporary vegetation indices to better quantify their sensitivity to (soil) droughts.

As highlighted by in Stocker (2019) GPP sensitivity to drought as derived from remote sensing data through a simple light use efficiency approach is poor. Therefore a need exists to:

1. quantify soil droughts from remote sensing data 
2. correct existing operational GPP models to address these inconsistencies.

This project addresses the first (1) component of this issue by using the whole (MODIS) spectral domain to model sensitivity to soil droughts from MODIS data alone.

The analysis was limited to locations where soil droughts could be quantified based upon ecosystem fluxes as described in Stocker et al. (2018, 2019). Sites are limited to those as listed in this publication, further limited to those with a relatively homogeneous vegetation.

## Notes

Model fitting was done with `analysis/03beni_regression_training_LSO_caret.R`

## GEE install

https://developers.google.com/earth-engine/guides/python_install-conda

## References

Stocker, Benjamin D., Jakob Zscheischler, Trevor F. Keenan, I. Colin Prentice, Sonia I. Seneviratne, and Josep Peñuelas. “Drought Impacts on Terrestrial Primary Production Underestimated by Satellite Monitoring.” Nature Geoscience 12, no. 4 (April 2019): 264–70. https://doi.org/10.1038/s41561-019-0318-6.

Stocker, Benjamin D., Jakob Zscheischler, Trevor F. Keenan, I. Colin Prentice, Josep Peñuelas, and Sonia I. Seneviratne. “Quantifying Soil Moisture Impacts on Light Use Efficiency across Biomes.” New Phytologist 218, no. 4 (June 2018): 1430–49. https://doi.org/10.1111/nph.15123.
