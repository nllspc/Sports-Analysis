Notes
================

-   [Eligibility for the JAWS calculation](#eligibility-for-the-jaws-calculation)
-   [Choosing Primary Positions](#choosing-primary-positions)
-   [Hall of Fame Score](#hall-of-fame-score)

Eligibility for the JAWS calculation
------------------------------------

The four-season-tenured constraint was decided upon by examining the distribution of the members of the Reds Hall of Fame. My initial group was only 81 players so I didn't want trim too much but I also wanted a final group that would provide a good measuring stick for future nominees. Sticking with seven years would be great. It would allow me to stay consistent with the original JAWS but it seemed unlikely. My initial thinking was that five years would satisfy my conditions.

I began by looking at common outlier measurements regarding the tenures of inductees and focused on values on the low end. There isn't a concern with players playing *too* long for JAWS calculation. Hopefully this would give me baseline from where I could start a decision process.

![\\text{threshold} = \\text{median} - 2\*\\text{MAD} = 3.0696](https://latex.codecogs.com/png.latex?%5Ctext%7Bthreshold%7D%20%3D%20%5Ctext%7Bmedian%7D%20-%202%2A%5Ctext%7BMAD%7D%20%3D%203.0696 "\text{threshold} = \text{median} - 2*\text{MAD} = 3.0696") ![\\text{threshold} = \\text{1st quartile} - 1.5\*\\text{IQR} = 1](https://latex.codecogs.com/png.latex?%5Ctext%7Bthreshold%7D%20%3D%20%5Ctext%7B1st%20quartile%7D%20-%201.5%2A%5Ctext%7BIQR%7D%20%3D%201 "\text{threshold} = \text{1st quartile} - 1.5*\text{IQR} = 1")

These values weren't very helpful to be honest. A one year floor wasn't worth considering. Three years seemed low, too. So these values didn't give me a desirable statistical floor.

Next I looked at the histogram and table of the different tenure values to see what percentage of players would remain at each cutoff. In my mind, I didn't want to go below 80%, but given that I'm starting with an intial group of 81 players, that was probably too low.

| cutoff | % remaining |
|:------:|:-----------:|
|  7 yrs |     67%     |
|  6 yrs |     78%     |
|  5 yrs |     86%     |
|  4 yrs |     92%     |

From here, the choice narrowed to four or five years. Now I wanted to see who would be left out at each cutoff. While I am a Reds fan, I am not well-versed in their history. So it's quite possible that there are some historically famous players on these two lists that I'm not aware of, but I did recognize one name, Dave "The Cobra" Parker. Mr. Parker only had four seasons with the Reds. If you look at his WAR, he had one good season and the others were poor, but if you look closer he got dinged heavily for his defense and was pretty good offensively, especially in 1985. WAR is what matters here though and Dave is lacking. It also shouldn't matter that he's a hometown kid, his nickname is **The Cobra**, and used to swing a sledge hammer in the batter's circle. I decided to stick a pin in it.

Lastly, I looked at the nominees and it turns out Scott Rolen only played four seasons. The goal of this project is to evaluate nominees so four years would be the necessary cutoff in order for Rolen to be included. Yeah, I should've checked that out to begin with. I don't consider this subject closed though and plan on revisiting it in the future. Your opinions are welcome. So the inductees that didn't make it were the following: Billy Werber, Bill McKechnie, and Wayne Granger. The Wright boys, George and Harry, also aren't in there. They played with the Reds before 1871 and their WAR wasn't available.

Choosing Primary Positions
--------------------------

Hall of Fame Score
------------------

This score is just a standardization of the player stats. There are many standardization methods and I didn't want to wade to deep into the area. I looked at two: one uses mean and standard deviation and the other uses median and MAD.

![\\text{score} = \\frac{\\text{value} - \\text{median}}{\\text{MAD}}](https://latex.codecogs.com/png.latex?%5Ctext%7Bscore%7D%20%3D%20%5Cfrac%7B%5Ctext%7Bvalue%7D%20-%20%5Ctext%7Bmedian%7D%7D%7B%5Ctext%7BMAD%7D%7D "\text{score} = \frac{\text{value} - \text{median}}{\text{MAD}}")

![\\text{score} = \\frac{\\text{value} - \\text{mean}}{\\text{sd}}](https://latex.codecogs.com/png.latex?%5Ctext%7Bscore%7D%20%3D%20%5Cfrac%7B%5Ctext%7Bvalue%7D%20-%20%5Ctext%7Bmean%7D%7D%7B%5Ctext%7Bsd%7D%7D "\text{score} = \frac{\text{value} - \text{mean}}{\text{sd}}")