---
title: "How to use the multEvol package"
author: "Dipendra Nath Basu and Vaishali Bhaumik"
date: '2022-08'
output: 
  html_document: 
    toc: yes
---

# I. Introduction

The `multEvol` package performs data extraction and phylogenetic analyses of shape and colour pattern data from 2D images. This document provides some example code to demonstrate how the package works.

## (a) Study system

This example uses a subset of the images that were analysed in our [biorXiv manuscript](https://www.biorxiv.org/content/10.1101/2022.06.19.496720v1). In total, 29 images of individual forewings (5 species/sex combinations; 4--5 images per species/sex combination) are being used to evaluate mimetic convergence in forewing colour patterns between species. The wings were photographed under constant light conditions and the images were cleaned in Adobe Photoshop to remove the background and to repair any damage due to scale loss or tearing. Cleaned forewing images (.jpg) of the following species were included:

1.  **Model species**: *Tirumala septentrionis* (Tiru_sept) and *Parantica aglea* (Para_agle).

2.  **Mimic species**: *Pareronia hippia*, including mimetic females (Pare_hipf) and non-mimetic males (Pare_hipm).

3.  **Sister species of mimics**: *Colotis aurora* (Colo_auro) and *Ixias pyrene* (Ixia_pyre).

The images were named as follows:

<center>

abbreviated species name (9 characters) + sex (2 characters) + sequence number (1 character) + file extension (3 characters)

</center>

For example, the second forewing image for female *Pareronia hippia* was named "Pare_hipfFe2.jpg". The number of characters in the species name (9 characters in this case) <b>should remain consistent across all image files</b>, as this will be important for species-level grouping during analysis.

## (b) Data organisation -- folders

Since extracting and analysing these data is a multistep process with several intermediate outputs, create some folders in your working directory to organise the input and output files. The names and contents of these folders are listed below. All folders apart from **img_folder** should be empty at the beginning of the analysis.

1.  **img_folder**: Cleaned images to be used for data extraction.
2.  **binary_images**: Binarised (black & white) images to be used in shape analysis.
3.  **transformed_folder**: Affine-transformed target images against which other images can be aligned during data extraction for colour patterns.
4.  **output_folder**: All output files, including intermediate data files, statistical results, and stacked images.

## (c) Data organisation -- files

These input files are also required:

1.  **col_index**: A dataframe containing the names of species/groups to be included in analysis (column 1) and the sequence number of the target image for each group (column 2).
2.  **wg_tree**: A phylogenetic tree containing all species/groups to be analyzed.

# II. Shape data

Shape data are extracted through elliptical Fourier analysis to quantify the 2D closed contours. These data are extracted with the `binImg()` and `cleqEFA()` functions.

Load the package:
```
    library(multEvol)
```
## (a) Transforming into binary images
The `binImg()` function creates black & white versions of each image:
```
im_path<-system.file("/extdata/img_folder/", package="multEvol")
binImg(img_path = im_path, thresh = 0.97, dimn = 100, out_path = "binary_images/")
```
<center>

![Original](https://raw.githubusercontent.com/DipendraBasu/multEvol_Images/main/Pare_hipfFe2_col.jpg?token=GHSAT0AAAAAABX6R33ONQHUOGM5ZDEQLGOGYYFBHQQ){width="100px;" height="100px"}

![Black & white](https://raw.githubusercontent.com/DipendraBasu/multEvol_Images/main/Pare_hipfFe2_bin.jpg?token=GHSAT0AAAAAABX6R33OW2MGX7FROMW2E4ROYYFBHRA){width="100px;" height="100px"}

</center>

## (b) Extracting Fourier descriptors
Next, the `cleqEFA()` function uses the outputs of `binImg()` to extract shape data:
```
    cleqEFA(img_path = "binary_images/", out_path = "output_folder/")
```
This function is interactive and requires several user inputs. The first prompt requests the minimum number of characters that defines species/groups. For the image files being used here, this number is 9 (for an explanation, see section I(a) Study system):
```
    Number of characters in group name: 9
```	
The second prompt requests the alignment procedure based on which the outlines will be aligned (see documentation for the `Momocs` package). Options are provided for Eigen value-based alignment along:

1. The longer axis (`coo_align()`).
2. The longest axis of shapes along x-axis (`coo_alignxax()`).
3. The calliper length (`coo_aligncalliper()`).
4. The minor axis (`alignminradius()`):
```
	Alignment options (1=align/2=alignxax/3=aligncalliper/4=alignminradius): 1
```
The third prompt requests the direction (up/down/right/left) in which the start point for the alignment will be reset:
```
	Start point reset direction (1=down/2=left/3=up/4=right): 1
```
The function plots an image showing all the outlines stacked and aligned against each other:

<center>

![Stacked outlines](https://raw.githubusercontent.com/DipendraBasu/multEvol_Images/main/Contour_stack.jpg?token=GHSAT0AAAAAABX6R33OQ7RVID43YPBW57K4YYFBA2Q){width="400px;" height="400px"}

</center>

The fourth prompt requests permission to continue on to the next step. If the stacked outlines seem appropriately aligned, enter "y". If not, enter "n" and the entire function will loop, requesting different alignment options.
```
	Continue (y/n): y
```
The fifth prompt requests the name of a target image (e.g., "Tiru_septFe1") that will be used to visualize the sine and cosine components of the shape and the fitted harmonics:
```
	Target image name (without extension): Tiru_septFe1
```
<center>

![Elliptical analysis](https://raw.githubusercontent.com/DipendraBasu/multEvol_Images/main/Elliptical_analysis.jpg?token=GHSAT0AAAAAABX6R33PSTNQTV3J4DMOGMECYYFBD6Q){width="400px;" height="400px"}

![Ellipse fit](https://raw.githubusercontent.com/DipendraBasu/multEvol_Images/main/Ellipse_fit.jpg?token=GHSAT0AAAAAABX6R33ODMKFQE3AGQJS54ZWYYFBEWA){width="400px;" height="400px"}

</center>

The sixth prompt requests the number of harmonics to be used for calibration when estimating the appropriate minimum number of harmonics that can explain all the shapes. This number should be relatively high (15--20) for explaining simple shapes:
```
	Number of harmonics for calibration: 15
```

Based on the input, the function calibrates the harmonic power and saves a boxplot visualizing the percentage of the shape that is accurately described by a given number of harmonics:

<center>

![Harmonic power](https://raw.githubusercontent.com/DipendraBasu/multEvol_Images/main/Harmonic_power.png?token=GHSAT0AAAAAABX6R33OVLMDJX7ME3OBOE6UYYFBD7A){width="400px;" height="400px"}

</center>

The seventh prompt requests the number of harmonics that the `efourier()` function should use. This number can be decided upon based on the previous boxplot, which is saved as a .pdf file ("Harmonic_power.pdf") in the output folder. In case all other harmonics need to be scaled against the first harmonic, choose one extra harmonic. In this case, seven harmonics would explain 99% of the variance in shape. To account for scaling against the first harmonic, the input should be "8":
```
	Number of harmonics for Fourier analysis: 8
```
The `cleqEFA()` function provides the following outputs:

1. **Stacked images** showing the stacked outlines of images in each group.
2. **An outline plot** showing the locations of equidistant points on the outline of the target wing.
3. **A line graph** showing the relationship between the sine and cosine components of the shape.
4. **An outline plot** approximating the wing shape, where each higher order ellipse follows the periphery of the preceding lower order ellipse.
5. **A box plot** explaining what percentage of the shape is accurately described by a given number of harmonics.
6. **A dataframe** containing the Fourier descriptors of the meanshape in columns (four coefficients per harmonic) for each group/species. This can be used as multivariate shape data.
7. **A list** of double class objects containing the x and y coordinates of meanshapes of each species. This can be used for the comparison of shapes between species.

## (c) Comparison of meanshapes between groups
The `shpComp()` function visualizes inter-group differences in the form of deformation heatmaps arranged in a matrix. The groups can comprise the same or different species/group names:

```
# gr1 <- col_index[, 1][3:4]
# gr2 <- col_index[, 1][5:6]
gr1 <- gr2 <- col_index[, 1][3:6]
shpComp(gr1, gr2, mnshps)
```
<center>

![Wing shape deformations between four butterfly species](https://raw.githubusercontent.com/DipendraBasu/multEvol_Images/main/Shape_comparison.png?token=GHSAT0AAAAAABX6R33OQRI5VETC3AQ53SHWYYFA6HQ){width="400px;" height="400px"}

</center>

# III. Colour data

Colour data extraction uses utilities in the `RNiftyReg` and `patternize` packages to quantify the pixel-level presence/absence of multiple colours.

## (a) Transforming target images

Before sampling colour data, image registration is performed to ensure that all images within and across groups can be stacked against each other, so that the data extracted from these images are comparable. Input a dataframe that lists the target image for each group:
```
    data(col_index)
```
Then, select a group (e.g., "Tiru_sept") whose target image ("Tiru_septFe1.jpg") functions as the target for the target images of *all other* groups. The `affTr()` function performs image registration by affine-transforming one target image from each group against the universal target for *all* groups:
```
    affTr(img_path = "img_folder/", img_ID = col_index, target = "Tiru_sept", out_path = "transformed_folder/")
```
<center>

![Original (Colo_auroMa5.jpg)](https://raw.githubusercontent.com/DipendraBasu/multEvol_Images/main/Colo_auroMa5_orig.jpg?token=GHSAT0AAAAAABX6R33OY6Z26CQ5FXKXPO7CYYFBFIQ){width="100px;" height="100px"}

![Target (Tiru_septFe1.jpg)](https://raw.githubusercontent.com/DipendraBasu/multEvol_Images/main/Tiru_septFe1.jpg?token=GHSAT0AAAAAABX6R33P6TRCPPDJLJV75L4UYYFBFJA){width="100px;" height="100px"}

![Transformed (Colo_auroMa5.jpg)](https://raw.githubusercontent.com/DipendraBasu/multEvol_Images/main/Colo_auroMa5_tra.jpg?token=GHSAT0AAAAAABX6R33PSVZHIGV7YY3LHMC4YYFBFKA){width="100px;" height="100px"}

</center>

This image registration produces borders around the image, which have to be removed. This can be done in any image editing software.

[**IMPORTANT**]{.underline}: Before moving on, create a backup of all original images. Then, replace the original target image in each group with the affine-transformed image. Alternatively, create a new folder in which the target images in each group have been replaced by affine-transformed target images. Once these images have been transformed, they can no longer be used for shape analysis.

## (b) Colour sampling

Three colours can be sampled in these images: melanistic (MEL), red/orange (RO), and blue (BLU). These are not present uniformly across species; for example, the wings of "Colo_auro" contain black and red/orange pigments, but not blue pigments. Therefore, "Colo_auro" is only included in "MEL" and "RO":
```
    sp_index <- list(MEL = c("Colo_auro", "Ixia_pyre", "Para_agle", "Pare_hipf", "Pare_hipm", "Tiru_sept"),
                     RO = c("Colo_auro", "Ixia_pyre"),
                     BLU = c("Para_nilg", "Pare_hipm", "Tiru_limn", "Tiru_sept"))
```
The `colData()` function extracts pixel-level colour data as a linearized matrix:
```
    colData(sp_index=sp_index, img_folder = "img_folder", ext = ".jpg", target = col_index, out_folder = "output_folder/", resampleFactor = 3)
```
This is also an interactive function that requires several user inputs. The first prompt asks for the colour to be sampled. E.g., red/orange (2 = RO) colours in "Colo_auro" and "Ixia_pyre":

```
Select colour patch (1=MEL; 2=RO; 3=BLU; ): 2
```

The second prompt asks for the minimum number of characters that defines species/groups (9):

```
Number of characters in group name: 9
```
Once all relevant images have been added to the list, the next prompt asks for the colour offset. For the first iteration, enter "2". This corresponds to an offset of 0.1, which is the default in the `patRegRGB()` function in `patternize`:

```
Choose colour offset (1=0.05; 2=0.1; 3=0.15; 4=0.2; 5=0.25; 6= 0.3; 7=0.35; 8= 0.4): 2
```
The plot window displays the target image for the first species in the list (Colo_auro).

<center>

![Transformed Colo_auroMa5.jpg](https://raw.githubusercontent.com/DipendraBasu/multEvol_Images/main/Colo_auroMa5_tra.jpg?token=GHSAT0AAAAAABX6R33PSVZHIGV7YY3LHMC4YYFBFKA){width="100px;" height="100"}

</center>

Clicking on any part of the image inputs an RGB value to which the offset is applied. Pixels with the resulting range of RGB values are highlighted in each image in the group, and these plots are stacked on top of each other. For red/orange colours, click on the part of the wing that contains the orange pigment. Given an offset of 0.2 and an RGB input of [192 87 4], the resulting stacked image for "Colo_auro" might look something like this (note that the image is flipped on the vertical axis):

<center>

![Orange colours in Colo_auro (5 images)](https://raw.githubusercontent.com/DipendraBasu/multEvol_Images/main/RED_Colo_auro.png?token=GHSAT0AAAAAABX6R33PCEJGR66XRASQFOYQYYFBHYQ){width="100px;" height="100px"}

</center>

The next prompt asks whether the function should continue to the next species/group. If the stacked image looks okay, enter "y". If not, enter "n" (or really anything except "y"). The function loops back and requests the colour offset again, and this goes on until you are satisfied with the colour sampling output.
```
Continue (press 'y' for yes): y
```
The `colData()` function provides the following outputs:

1. **Stacked images** showing the presence/absence of a given colour across all images in a group.
2. **A dataframe** indicating the presence/absence of a given colour in a given pixel averaged across all images in a group.  For example, if there are 5 images of Colo_auro and a given pixel is orange in 4 of those images, the corresponding value in the data matrix will be 4/5 = 0.8. These values always range from 0 to 1 (NAs are replaced with 0). The data matrix for each species/group is linearised into a vector, and the vectors of each species are combined into a dataframe.  For example, the dataframe for RO has two rows ("Colo_auro" and "Ixia_pyre") and several columns. The number of columns depends on the dimensions of the resampled image; in this case, the dimensions are 100 px * 100 px = 10000 columns.
3. **A list** containing the sequence number of the target image in each group, the RGB value sampled in each species, and the corresponding offset value. This file is for posterity and will not be used in further analysis.
4. **The RasterStacks** of each species.

This function can be used to extract colour data for multiple colours (in this case, MEL, RO, and BLU).

## (c) Combining colour data for multiple colours

The `concat()` function concatenates the data matrices of multiple colours into a single dataframe for subsequent analysis. This step is unnecessary when only one colour is being extracted for analysis:

```
col_concat <- concat(file_names = list(MEL_mean, BLU_mean, RED_mean))
```

## (d) Comparison of colour patterns between and within groups
The `colComp()` function visualizes intra- and inter- group variations in colour pattern by two heatmap scheme in a form of cartoon matrix implementing  various utilities of `patternize`:

```
gr1<-gr2<-col_index[,1][3:6]
colComp(gr1, gr2, rstrstcks = MEL_RasterStacks, ID = MEL_IDList, outline_path = "outline/", bcg_col = "dimgray", font_col = "white")
```
<center>

![Colour pattern variations within and between four butterfly species](https://raw.githubusercontent.com/DipendraBasu/multEvol_Images/main/Colour_comparison.png?token=GHSAT0AAAAAABX6R33OII5HGHFLCGVKBMXMYYFBGVQ){width="400px;" height="400px"}

</center>

# IV. Phylogenetic analysis

## (a) Phylogenetic PCA
The `multEvol()` function standardizes multivariate data and performs a phylogenetic principal component analysis using utilities in the packages `ape` and `phytools`. This requires a phylogenetic tree and a dataframe containing multivariate data. If the data need to be phylogenetically size-corrected against an independent variable, the column containing the independent variable needs to be specified.
```
data(wg_tree)
data(col_concat)
PCA_col <- dataPrep(phy=wg_tree, dat=col_concat, colnum=NULL, scale=F, method="BM", mode="cov")
```
The output is a dataframe containing the principal components.

## (b) Fitting phylogenetic models to multivariate data

The `pcModels()` function uses utilities in the `geiger` package to fit six macroevolutionary models to a phylogenetic tree: Brownian motion (BM), early burst (EB), Ornstein–Uhlenbeck (OU), lambda, kappa, and delta.
```
evo_model <- pcModels(phy = wg_tree, dat= PCA_col)
```
The output is a list containing the estimates of statistical parameters for each model fitted to each PC.

## (c) Estimating convergence
Convergence can be estimated between various groups of models and mimics. The names of these species are entered in a list called 'regimes':

```
regimes <- list(mod_mim1 = c("Para_agle", "Pare_hipf"), mod_mim2 = c("Tiru_sept", "Pare_hipf"))
```

The `cIndex()` function estimates convergence indices (C1--C4) using the `convratsig()` function in the `convevol` package. The inputs are a vector or data matrix containing principal components (PCA_col), a phylogenetic tree (wg_tree), and a list of species combinations (regimes). The number of principal components to be used for analysis and the number of simulations can be specified:

```
cIndex(x = PCA_col, y = wg_tree, z = regimes, dimn = 4, nsim = 1000)
```
The output includes the following:

1. **C indices** (C1--C4) indicating the extent of convergence among various groups of species.

2. **Simulated null distributions** of C indices, estimated using a Brownian motion model.

3. **The *p*-values** of actual C indices, estimated against the respective null distributions of C indices.

## (d) Estimating rate of evolution
The `rateEvol()` function estimates the rate of trait evolution for each branch of a phylogenetic tree using the ridge regression method implemented in the `RRphylo` package. The function simultaneously estimates the evolutionary rates of multiple traits, which should be entered in the form of a list:

```
traitPCA = list(shape = data(shp_pcscores), colour= data(col_pcscores))
```

Rates of evolution can also be estimated for incompletely resolved (polytomic) phylogenies or phylogenies with artificially introduced branches (e.g., individual branches for conspecific forms or sexes). In such cases, the `rateEvol()` function considers the introduced branches one at a time (while omitting its sister branches) and calculates the rate of evolution from the node that represents species divergence. This process is repeated as many times as the number of introduced branches. For rate estimation with introduced branches, the function requires a user input in the form of a list  with two nested list objects. The first list object contains the tip labels of introduced branches (for which the rate of evolution will be estimated in each iteration), and the second contains the tip labels of introduced sister branches (that need to be omitted in each iteration):

```
tiplist <- list(measure = c("Pare_hipf", "Pare_hipm"), omit = c("Pare_hipm", "Pare_hipf"))
```

The `rateEvol()` function also requires user inputs indicating the number of dimensions (PCs) of multivariate trait data and the total number of tips for which the rate of evolution is to be estimated, and a phylogenetic tree of class phylo:

```
rate_trt <- rateEvol(traitPCA, dimn = 4, tree = wg_tree, ntip = 6, intr = T, tiplist)
```

The output is **a dataframe** containing the coefficients of ridge regression for each trait (column) and each branch in the phylogeny, including each group/species (rows).
