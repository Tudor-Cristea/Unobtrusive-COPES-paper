# Unobtrusive measurement of self-regulated learning: R scripts

R scripts for the paper *Unobtrusive measurement of self-regulated learning: A clickstream-based multi-dimensional scale* by T. Cristea, C. Snijders, U. Matzat and A. Kleingeld, published in *Education and Information Technologies* (2024): <https://doi.org/10.1007/s10639-023-12372-6>. This is Chapter 4 of my PhD dissertation (Eindhoven University of Technology, 2025).

- Archived version of this code: <https://doi.org/10.5281/zenodo.7855167>
- For the data cleaning and pre-processing steps that come before these scripts, see my tutorial (Chapter 3): <https://github.com/Tudor-Cristea/tutorial-for-LA-in-R>

## Data

The scripts work with Canvas data. The student data used in the paper are not included in this repository, because they are personal data that cannot be shared publicly. All IDs in the scripts were changed for anonymisation purposes.

## Requirements

R, with these packages:

```r
install.packages(c("dplyr", "stringr", "lubridate", "psych", "lavaan", "car", "corrplot", "REdaS", "VIM"))
```

## How to use the scripts

The scripts are numbered in the order you should use them, and some have prerequisites, which are described below.

Start by loading the "course_dim" table, which contains the "course_id" of your target course. You can use it to filter the requests in the "requests" table. You should also add the "enrollment_dim" table to filter by role (e.g., students) using the "type" column.

Next, load the smaller tables, which contain extra information on the requests and are needed for certain indicators. In our study, we used:

- "discussion_topic_dim" for discussion and announcement indicators (the "type" column distinguishes the two)
- "assignment_dim" for clicks on assignment pages
- "submission_dim" for students' submissions (merge it with "assignment_dim" to know where each submission belongs)
- "submission_comment_dim" for comments students left with or after a submission
- "quiz_main" and "quiz_fact" for all clicks within quizzes (merge them to get all the necessary information)
- "files_dim" for clicks on learning materials within modules (such as articles and videos)
- "conferences_dim" for clicks on video conferences (in our case, only BigBlueButton was saved)

Ideally, merge the requests with only one or two tables at a time, and use the resulting table for the indicators that need it (e.g., build the indicators about discussion clicks from the requests + discussion table). This keeps everything manageable and faster. The parts we share are in scripts (1) and (2). Do not run these two scripts in full; run only the parts that apply to your course. We needed this because we used courses from different years and quartiles.

Once this is done, use script (3) to compute the indicators for phases one (Task Definition), two (Goal Setting) and four (Adaptation). At the end of the script, a function runs the right script for the third phase (Enactment) based on the course ID. Each course needs its own Enactment script; script (4) is an example. The main problem is the way Canvas represents files: materials within modules can only be distinguished by extension or name. Because we wanted to separate mandatory from optional materials, we could not build indicators from file extensions alone (e.g., a .doc file can contain mandatory or optional exercises). We therefore went through each course as an observer and assigned the files to categories manually. Some files also had slightly different names for the same material (e.g., "Lecture 1a" and "Lecture 1A"), which required further manual checks. With only four courses, this was the easiest approach, and it kept the differences between courses easy to express. You can copy the scripts and adapt them; with more courses, I would recommend a more scalable solution.

Once you have the indicators, clean them in script (5): remove indicators with very little variance or zero clicks, replace NAs with 0 where logical, and transform them per course. Script (6) forms the scales using Cronbach's alpha and principal component analysis. Before running the correlations, combine the final scales with the grades and survey responses. If the data are pseudonymised, you can use the "pseudonym_dim" table to link them. In our case, the surveys contained the SRL scale of the MSLQ. To check academic performance (grades), you can use the same table to link everything to the OSIRIS tables (TU/e's student information system).

The final correlations and regressions (the validity check) are in script (7).

## Problems encountered and tips

- The IDs (for students, courses, discussions, etc.) were very long, which caused problems when they were read as numeric. We recommend reading everything as character and converting later.
- Tables may use IDs of different lengths for the same thing. For example, a discussion might have the ID "12340000000019761" in the requests table but "19761" in the discussion table. Check the ID lengths before merging tables; if they differ, remove the first part with `str_remove()`. This worked everywhere for us.
- Some IDs (e.g., for files) are not available as a column, so you may need to extract them from the URL in the requests table. Our scripts show examples, but you may need to adapt them.
- Some ID columns have different names in different tables. For example, discussions use "discussion_id" in the requests table but simply "id" in "discussion_topic_dim", and some tables use "canvas_id". The Canvas Data documentation is very helpful: <https://portal.inshosteddata.com/docs>
- Before merging tables, make sure the ID columns have the same class. If you read everything as character, this should not be a problem.

## Licence and citation

The code is available under the MIT licence (see LICENSE). If you use it, please cite the paper above; the "Cite this repository" button in the sidebar gives the reference.

## Acknowledgements

I thank Dr. Rianne Conijn (<https://github.com/RConijn>) for helping with parts of the code, and Sonja Kleter for helping with the slow manual checks that such large amounts of data require. This would have taken even longer without them.
