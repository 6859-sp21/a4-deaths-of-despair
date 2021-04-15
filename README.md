# **Deaths of Despair**

## **Context**

The goal of our project was to shed light on a ground-breaking economics paper by Anne Case and Angus Deaton. Case and Deaton first showed in 2015 that an increase in “deaths of despair” - defined as deaths related to drug and alcohol poisoning, suicide, and alcoholism-related liver disease - had challenged decades of decreases in overall US mortality rates. At the time of their study (1999-2013), these increases were specific to non-Hispanic White Americans and they were not observed in other rich countries. 

![](/images/dod.png)

*Fig. 1 Age 45–54 mortality rates for US white non-Hispanics (USW, in red), US Hispanics (USH, in blue), and 6 comparison countries. From Case and Deaton (2015)*

In 2017, Case and Deaton built on their initial findings with a new paper that sought to explore the causes of these increases. However, they retained their focus on non-Hispanic White Americans. With six more years of mortality data available to us, we wanted to explore whether the tragic phenomenon they had exposed was still specific to this population, or whether any policy decisions that had followed from their first paper should be expanded to consider other affected communities.

## **Rationale for design decisions**

**a) Encodings**

Given the broad scope of the original paper, there were several demographic and mortality indices worth exploring for our visualization. For example, Case and Deaton had shown that the toll of deaths-of-despair varied by educational attainment, gender, and region. We decided to explore how much rates of deaths of despair were varying across race/ethnicity groups, because our findings there contrasted most with the original paper (which had found no or little increase for non-White groups). To account for the differences in each demographic group’s size, we normalized the data by each group’s total population per year then scaled it by 100,000. Thus, all ‘rates’ on our graphs represent deaths per 100,000 per group (stated in the description in bold). 

To organize and communicate comparisons of deaths-of-despair between age and race/ethnicity groups, we utilized position, numeric, color, and scale encoding channels via a linked small multiples visual. Small multiples enabled us to segment our ‘story’ into simpler, digestible charts for each group. All at once, the small multiples tell the complete, and disheartening, story that deaths-of-despair have increased across all groups in the last five years. 
As Groeger (2014) points out, small multiples ‘harnesses pre-attentive processing’ because the user can quickly see changes in size between different charts at the same time. Therefore, the user does not need to do all the comparative hard yards serially and reflectively. Indeed, a crucial benefit of small multiples is that critical information (such as the lines on the charts) is on the screen and is not changing. Although the cursor does add annotated values to each chart, the actual lines stay constant (unless a button is clicked). As a result, the user has to remember less information in order to make comparisons than they would when viewing an animation or scroll story, for example. It also allows the user to refer back to previous data points easily and iteratively update their inferences.

We opted for an area-line chart for each small multiple to visualize mortality. The small-multiples charts are organized in grid format with identical x- and y- axes to streamline intra race- or age-group comparisons. The grid is set-up such that the race (or ethnicity) varies by row so that age groups can vary by column (along the horizontal axis). This ensures all chronological elements (i.e. year and age) are increasing along the x-axis, in ascending order (left to right). This position encoding felt most intuitive for time, and thus guaranteed users did not struggle to interpret the charts and their interconnectedness. Additionally, we recognized that the power of our visual would be elevated by the user’s ability to quickly summarize the overall story, so we decided to further encode the scale of the crisis numerically, with the overall percent change in mortality per race/ethnicity across all ages written to the right of each row of plots. We also used this number to organize the race/ethnicity groups along the y-axis, so that the group most affected by a type of death was always shown at the top.

Finally, this is a sensitive topic, and therefore we used muted colours to encode the race/ethnicity groups, and a muted palette overall. Secondly, we have included information on two different suicide prevention lifelines, so that people who are affected by these issues can seek support.

**b) Interaction**

As discussed, we wanted the user to be able to compare trends across a number of different groups without getting overwhelmed. To that end, we chose a linked small multiples setup because it reinforces the user’s scanning and reading patterns to allow complex comparative inference (Hullman & Diakopoulos, 2011). Specifically, hovering the cursor above a plot retrieves the mortality rate for the relevant year on the target plot, as well as the rate for the same year on all the other plots. This allows users to easily ‘read’ more deeply into the differences between charts by anchoring the overall trends they see with specific year-over-year values . This is complementary to the perceptual support provided by small multiples, which is discussed above.

We also included a number of filter buttons so that the user could compare the toll of deaths of despairs to that of all other types of deaths, and contextualize the scale of their growth relative to overall mortality rates. Indeed, toggling back and forth between mortality trends across the same time period exposes the disproportionate impact of deaths of despair on pushing up overall mortality rates. This addition was driven by MVP feedback, as a number of our peers wanted to contextualize the larger implications of the trends they were seeing. Finally, although we define “deaths of despair”, the addition of a drop-down button that uncovers precisely what constitutes these deaths reminds the user of the concrete tragedies behind the term. 

Based on our prior class readings, we knew that it should not be necessary for the user to interact with the visualization to uncover key insights. In fact, our static graph alone should be sufficient to understand that the toll of deaths of despair is no longer particular to whites, and instead now affects other communities more acutely. Discovering that hovering above the graphs triggers an annotation of the value for that year on all charts merely helps the user drill down on the specifics. Similarly, clicking on the filter buttons is not instrumental to grasping the differential impacts of deaths of despair.

**c) Alternatives and process (incl. post-MVP revisions)**

At the start of this project, we were largely interested in exploring the relationship between educational attainment and rates of deaths of despair. Reading the Case and Deaton paper had led us to think that the impact of this tragic phenomenon was largely confined to white Americans, and that educational attainment was an important predictor of vulnerability to these deaths within that group. However, a number of data limitations (detailed in the process section) pushed us to explore our demographic variables more carefully and led us to the disheartening insight that deaths of despair were now increasing across all race/ethnicity groups. Therefore we chose to focus our visualization on characterizing the impact of these deaths across different people of ace/ethnicities.

Feedback from our MVP suggested that the chart was a little overwhelming. This makes sense because research suggests that movement can have very powerful preattentive influence (6.859 Lecture, Week 7), even though the movement is uniform on all the small multiples. Therefore we decided to reduce the number of plots on the visualization, based on this feedback. We also added a column of summary statistics that displays the overall change in mortality rates for the relevant types of deaths and groups to help the user grasp the magnitude of the trends captured by the small multiples. Finally, we decided to order the race/ethnicity groups by this change metric instead of by population size, so that the user could easily perceive which groups were most affected by deaths of despair. 

In order to improve this visualization further in the future it may be worth trialling how users perceive certain aspects of the visualization. Particularly, it might be worth understanding whether the gestalt principle of ‘common fate’ is influencing the user’s inferences from the visualization. This principle suggests that objects which move together are perceptually grouped by the user. Indeed, it would be useful to know whether this actually inhibits the user’s ability to distinguish trends by race/ethnicity and age, or supports more complex inferences. We could also have made more efforts to “anthropomorphize” our data (in Michael Correll’s words), as our current graphic may seem very dry and abstract in comparison to the devastating events it describes. Lastly, in future iterations it might also be useful to interrogate our own position as authors of a data visualization and how our own positions of relative privilege impact how we approach this topic.


## Developmental Process

**Data Retrieval and Analysis (~20 hours)**
As is always the case, the data analysis portion of this project took longer than expected. We had initially planned to use the CDC’s WONDER database to obtain mortality data (as this database computes mortality rates and aggregates them by demographics for the user), but the interface turned out to have a very frustrating cap on how many records could be accessed at once and did not contain any educational attainment data, which we had wanted to explore. Instead, Axelle pulled individual mortality data over 2009-2019 from the CDC’s National Vital Statistics. These files were much more “raw”, and figuring out how to convert them to a usable format was challenging. Thankfully, she eventually found a public script on Github that she was able to adapt for our purposes to read in the data. 

The Vital Statistics data was at the individual level, so Lama had to pull population totals for each of the demographic groups to convert mortality counts into rates. For this, she used estimates from the US Census Bureau, which she found and converted into a usable format. Unfortunately, this source was missing information for 2009, so we made the decision to impute 2010 population totals for 2009. Furthermore, these estimates only focused on race/ethnicity and age, and we were interested in the intersection of these attributes with education. Axelle hypothesized that one way to estimate these totals would be to use data from the Current Population Survey, which guarantees a representative sample. However, the rates she derived from this process were nonsensical and very inconsistent, so we had to abandon the education analysis.

The Vital Statistics data provides cause of death using the WHO’s ICD-10 convention, which has over 70,000 individual codes. In a discovery that boggled our minds, the WHO does not provide a data dictionary for these codes in text format, only a PDF containing a lengthy description and discussion of the codes. Thankfully, Case and Deaton had published the exact list of codes they had associated with each death of despair. Still, we wanted to make sure nothing was missing, as the ICD-10 is regularly updated. Axe;;e was able to find a mostly complete list of definitions for the codes from a third party website and added a few codes to the chronic liver diseases and cirrhosis umbrella. 
The aggregation by race/ethnicity, age and type of death were done by Axelle in R. We had expected to focus mostly on the educational differences, which were much highlighted in the original paper. However, we were confronted by new and tragic findings: that the dramatic increases in rates of deaths of despair were no longer specific to non-Hispanic white Americans and were rather most pronounced in non-Hispanic black Americans. We decided to make this the focus of our visualization. 

**Visualization Creation (~50 hours)**

All three of us were brand new to web development and D3.js (although we had extensive coding experience), so we knew that this assignment would be a challenge! We wanted to make A4 manageable and as much of a learning experience as possible, so we decided to build from an existing linked small multiples prototype. Many of the online tutorials were aimed at rather advanced coders, but we eventually found a pretty bare-bones version that would function well as a skeleton for our MVP. The source is cited on the visualization. A lot of time was also spent following HTML/CSS/JavaScript/D3.js online tutorials and reading Scott Murray’s book.

Prior to creating the visual online, Lama sketched several detailed prototypes of how to organize the small multiples for the group to vote on. Axelle then adapted the source code found to work with our data and Lama’s sketch. Axelle amended all color encodings and text to reflect our groupings and the information we wanted to convey. Lama and Phil expanded the source code to incorporate functionalities for a sorting button. Axelle added in a summary growth number for each race/ethnicity group that matched the color encoding and ordering of the relevant group, and ensured that the orderings of both the charts and this number matched the highest growth in death rates. Each of us spent time adding comments to and formatting the source code. 

Personalizing the small multiples away from the source code’s original implementation was very important to us. Most of our changes were across visual aesthetics, user interactivity, and grid formatting. Lama wanted to make each individual line/area chart as readable as possible without interaction and therefore added horizontal major grid lines. She also experimented with different color values, and ultimately decided to choose muted-earth tones to show which emphasized the ‘humanity’ behind the data. We want to ground our users with the implicit notion that the numbers visualized are not just statistics, they announce a crisis important to all the racial/ethnicity groups we belong to. For similar reasons she also opted for the less-rectangular and more “natural” sans-serif font family. Lama also spent some time contemplating the size of the charts, choosing them such that the charts all together make up approximately three-fifths of the users’ screen. She also made sure they did not re-scale or change ordering per somebody’s screen size. 

**Adding an additional layer of interactivity (~15 hours)**

Lama originally considered introducing a sorting function so that the user could choose to sort the plots from highest to lowest on different metrics (rate of increase in deaths, overall death counts, alphabetically etc.) Lama tried to implement this using the js library, isotope. However, it was decided that this would be one too many features, considering the feedback from our MVP suggested that our visualization already had too much going on and was distracting. Therefore, we decided to choose to implement a set of filtering buttons, instead of implementing sorting buttons. Phil was able to create the button by switching between various data sources. The button worked by deleting and reading all the graphs each time the selection is made. Lama was able to expand the button functionalities to include a drop down menu that tells the user which specific filter they're on, and how each filter is related. 

**Write-up (~ 10 Hours)**

All three of us utilised the readings and lectures from the course to explain the theoretical underpinnings of the visualization in the write-up. In order to do this we went through the material from classes again and analysed how we could integrate these insights into our visualization. We also all did extra research on Case & Deaton’s insights (re-reading the paper, listening to their full book as an audiobook, listening to lectures etc.) in order to do justice to their insights and include these in the write-up. Phil wrote the initial write-up which we then all reviewed and built on. 


## **References**
Chapter 1: Information Visualization. Stuart Card, Jock Mackinlay, and Ben Shneiderman. Readings in Information Visualization. 1999.

Visualization Rhetoric: Framing Effects in Narrative Visualization. Jessica Hullman and Nick Diakopoulos. IEEE InfoVis. 2011.

‘A Big Article About Wee things’, Pro Publica, Lena V Groeger, 2014

Case, Anne, and Angus Deaton. “Rising Morbidity and Mortality in Midlife among White Non-Hispanic Americans in the 21st Century.” Proceedings of the National Academy of Sciences 112, no. 49 (December 8, 2015): 15078–83. https://doi.org/10.1073/pnas.1518393112.

Case, Anne, and Angus Deaton. “Mortality and Morbidity in the 21st Century.” Brookings Papers on Economic Activity 2017 (2017): 397–476.

https://flowingdata.com/2014/10/15/linked-small-multiples/
