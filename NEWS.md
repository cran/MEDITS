MEDITS v0.1.7
==============
- the print() function is used (F.I. in the case of LFD.R) to return the ggplot2 plots and not to return messages to the console. In cases in which messages() and warning() were used, they can be silenced with the paremeter verbose = TRUE.
- "T" and "F" substituted in all the scripts with "TRUE" and "FALSE" 
- The use of the *immediate* call of on.exit() was corrected in: sex.ratio.ts.R, index.rs.R, index.ts.R, sp.index.R, bubbleplot.R
- The acronyms (such as TA) were always explained in .Rd files
- In bubbleplot() the parameter "inches" was introduced to define the maximum radius (in inches) of the circles.


MEDITS v0.1.6
==============
-Saving option of plot eliminated from bubble.rs function
-Used "donttest" instead of "dontrun" in the following .Rd files: bubble.rs.Rd, TATB.grid, TATC.grid,haul.plot, index.rs, LFD, m.TATC, quant, sp.index.
-\value added to following .Rd files: bubble.rs.Rd, m.TATB.Rd, dd.distance.Rd, dd.to.MEDITS.Rd, MEDITS.distance.Rd, MEDITS.to.dd.Rd
-substitution of cat or print with message function in the following function: m.TATB, m.TATC, check.format, dd.distance, MEDITS.distance, land.points, index.ts
-verbose parameter added to the following functions: m.TATB, m.TATC, index.ts, dd.distance, MEDITS.distance
-added the on.exit() function to retain the user default values of par()  in the following functions: bubbleplot, index.rs, index.ts, sexratio.ts, sp.index
-in haul.plot function was introduced the possibility to select the format of the coordinate to be plotted: "MEDITS" or "degrees".
-estimation of abundance indices time series by sex in index.ts() function.
-q95() function substituted by quant() function that allows the estimation of the length at a user-defined percentile value of a LFD. The argument "quantile" is now required by the function.
-the outputs of LFD() function was modified. The LFD is returned as a data frame and not as the element of a list, as well as for the LFD plot.
-update of README.md file

MEDITS v0.1.5
==============
-The Title field formatted in title case

MEDITS v0.1.4
==============
- crop of wmap, countries, depth_1, depth_2, depth_3 shapefiles to the Mediterranean and Black Sea area to reduce package size

MEDITS v0.1.3
==============
- Title changed with "Analysis of MEDITS-like Survey Data" in the Description file 
- Link to MEDITS URL added in the the Description file

MEDITS v0.1.2
==============
- the function strata.area was suppressed because not stable 
