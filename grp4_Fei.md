### Group4 

####Project Title: an RShiny app development project (NY School Hunter)

Project summary: Our project takes all available data on colleges and universities in New York State and attempts to create a useful shiny app that allows users to **explore and compare schools based on user-specific filtering criteria**. The purpose of our design is to provide users with a bird’s eye view of New York colleges and universities; allow them to **filter, search, and group** schools by their preferred criteria; and further **compare two schools on a more micro level**. A distinguishing feature of our app is the **map search function - users can see all the specified schools on the map** (normal map view or satellite map view), focus in on a specific area of New York State, and choose to group schools on an overlayed hovering map (i.e. a map overlayed on top the main map) by clusters based on their choosing (e.g. part-time or full-time programs). We made our map so detailed that users can zoom in on a single school, say on the satellite map view, and see layout of the campus along with names of buildings on the campus. As for our side-by-side comparison feature, currently it compares only two schools side-by-side, but it can easily be changed to a multi-select option for comparing more than two schools (we leave it be for this prototype of our app). The side-by-side school comparison feature allows users to see a detailed breakdown of meaningful data and statistics from our available data on each school. We’ve picked out and highlighted certain details that we think would be helpful to users searching for schools - e.g. tuition fees, locale, demographics, etc. Though our data on schools is limited, we extracted as much meaningful data as we could and analyzed and displayed it in an appealing and useful way, with visual aids.


### Pros:
+ There is a fancy background and neat main page, which give audience a comfortable sense.
+ The floating classification map is an important highlight. And its dynamic property gives more information
+ Side-by-side comparison gives direct information about the school and we can compare two school that we are interested in very conveniently.
+ It's good to use `plotly` to plot interactive figures in comparison. 

### Cons
+ Filter options is kind of confused. For example, if I choose `Tuition`, I still need to choose a specific option in the second filter option. So what the big meaning of the first filtering bar? I do think if I want to focus on `Scores`, the `Major` and `Tuition` should be hidden.
+ For each sub-choices(like `Degree`, `Length`), it's better to add some descriptions. Otherwise we don't know what that means because it seems no response no matter whether I pick or not.
+ Again, no statements about the Classification Map. Such a confusion
+ Side-by-side function might be too heavy that the resposne is a little slow especially the rolling bar. And also, if I want to find a school, I'd like search it or at least can seach by the fist letter.
+ The information for comparison is a mess and there are so many NAs. Maybe we can also set filter bar here to focus on what we are interested in(like `scores` or `tuition`)
+ The icon for each schools, I think, is not that attractive. It might slow the searching speed and I need to scroll down to find the critical infromation.