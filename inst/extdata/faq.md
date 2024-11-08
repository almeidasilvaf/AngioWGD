


> **1) How are WGD events positioned in nodes of the tree?**
>
> To place a WGD on the tree, we use the following steps:
>
> 1. Extract the names of all species that share that WGD event;
> 2. Identify the ID of the node with the most recent common ancestor (MRCA)
> of all species extracted in step 1 - this node ID is used to obtain the y-axis
> coordinate of the rectangle indicating a WGD;
> 3. Extract mean and Highest Convergence Region (HCR) of the posterior
> distribution of WGD age to represent the age and uncertainties of a WGD
> event - these are used to obtain start and end coordinates of the
> rectangle indicating a WGD.
>
>
> For 13 WGD events (IDs: *POAC*, *POAL*, *DIOS*, *PASE b*, *CALY*, *LARD*, 
> *TROC a*, *NEOL*, *SALV*, *PASS*, *MANG*, *POZO*, *AESC*), simply using
> the mean and 90% HCR to obtain x-axis coordinates of the WGD rectangles leads to
> an apparent placement of WGD rectangles 'outside' the node where it must be.
> This is likely due to shifts in substitution rates, as reported in 
> [Vanneste et al. (2014)](https://doi.org/10.1101/gr.168997.113).
> However, even when WGD rectangles seem out of the node, the uncertainties
> around WGD dates still overlap with uncertainties around divergence
> times estimates. For example, the figure below shows the *POAC* WGD (shared
> by all Poaceae species) on the tree.
>
> <div style="text-align: center;">
> <img src="https://github.com/almeidasilvaf/AngioWGD/blob/main/inst/extdata/tree_Poaceae.png?raw=true" width="50%">
> </div>
> 
> Note that, although the rectangle seems out of the node where it should be
> (indicated with an arrow), uncertainties overlap, supporting their correct
> placement.
>

----

> **2) How do I report an issue or suggest a feature?**
>
> You can open an issue in the [GitHub repository](https://github.com/almeidasilvaf/AngioWGD) 
> where the source code for this app is.

----

> **3) How do I save figures/tables to files?**
>
> Boxes in all tabs with figures and tables have a download 
> button (![](https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/solid/download.svg){width="1%"})
> at the top right corner. By clicking it, you can specify the output file 
> format (.pdf/.svg for figures, .tsv/.tsv.gz for tables), and download the file
> to your local machine.

----

> **4) In the posterior distributions of WGD ages, how do you obtain the 'consensus' distribution?**
>
> The 'consensus' distribution is obtained from data points from all species combined.
> For example, signatures of the *JUGL* WGD event (shared by all Juglandaceae 
> species) were identified in four species. When visualizing age distributions
> for this WGD, you will be able to see four different distributions (each with 20,000 
> data points) on the left-hand side. On the right-hand side, you will see
> a single distribution with all data points combined, regardless of the species
> from which they were obtained (in this case, 4 x 20k = 80,000 data points).



