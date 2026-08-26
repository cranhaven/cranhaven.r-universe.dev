# MIT License
#
# Copyright (c) 2023 Ulster University (https://www.ulster.ac.uk)
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the 'Software'), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.


# create GAD-7 instrument
instrument_gad <- create_instrument_from_list(
    instrument_name = "GAD-7",
    question_texts = list(
        "Feeling nervous, anxious, or on edge",
        "Not being able to stop or control worrying",
        "Worrying too much about different things",
        "Trouble relaxing",
        "Being so restless that it is hard to sit still",
        "Becoming easily annoyed or irritable",
        "Feeling afraid, as if something awful might happen"
    ),
    question_numbers = list("GAD-7_1", "GAD-7_2", "GAD-7_3", "GAD-7_4", "GAD-7_5", "GAD-7_6", "GAD-7_7")
)

# create CES-D instrument
instrument_cesd <- create_instrument_from_list(
    instrument_name = "CES-D",
    question_texts = list(
        "I was bothered by things that usually don't bother me.",
        "I did not feel like eating; my appetite was poor.",
        "I felt that I could not shake off the blues even with help from my family or friends.",
        "I felt that I was just as good as other people.",
        "I had trouble keeping my mind on what I was doing.",
        "I felt depressed.",
        "I felt that everything I did was an effort.",
        "I felt hopeful about the future.",
        "I thought my life had been a failure.",
        "I felt fearful.",
        "My sleep was restless.",
        "I was happy.",
        "I talked less than usual.",
        "I felt lonely.",
        "People were unfriendly.",
        "I enjoyed life.",
        "I had crying spells.",
        "I felt sad.",
        "I felt that people disliked me.",
        "I could not get “going.”"
    ),
    question_numbers = list(
        "CES-D_1", "CES-D_2", "CES-D_3", "CES-D_4", "CES-D_5",
        "CES-D_6", "CES-D_7", "CES-D_8", "CES-D_9", "CES-D_10",
        "CES-D_11", "CES-D_12", "CES-D_13", "CES-D_14", "CES-D_15",
        "CES-D_16", "CES-D_17", "CES-D_18", "CES-D_19", "CES-D_20"
    )
)


# test the function for one instrument
match_gad <- match_instruments(list(instrument_gad))
crosswalk_table_gad <- generate_crosswalk_table(match_gad$instruments, match_gad$matches, 0.7, TRUE, FALSE)

# round match scores for testing
crosswalk_table_gad$match_score <- round(crosswalk_table_gad$match_score, 2)

# crosswalk table from web version
expected_crosswalk_table_gad <- data.frame(
    pair_name = c("GAD-7_GAD-7_1_GAD-7_GAD-7_7", "GAD-7_GAD-7_2_GAD-7_GAD-7_3"),
    question1_id = c("GAD-7_GAD-7_1", "GAD-7_GAD-7_2"),
    question1_no = c("GAD-7_1", "GAD-7_2"),
    question1_text = c("Feeling nervous, anxious, or on edge", "Not being able to stop or control worrying"),
    question2_id = c("GAD-7_GAD-7_7", "GAD-7_GAD-7_3"),
    question2_no = c("GAD-7_7", "GAD-7_3"),
    question2_text = c(
        "Feeling afraid, as if something awful might happen",
        "Worrying too much about different things"
    ),
    match_score = c(0.71, 0.76)
)

# test if the output is the same as the web version
test_that("Generate crosswalk works for one instrument", {
    expect_equal(crosswalk_table_gad, expected_crosswalk_table_gad)
})


# now test the within_instrument_matches toggle
match_both <- match_instruments(list(instrument_gad, instrument_cesd))
crosswalk_table_both <- generate_crosswalk_table(
    match_both$instruments,
    match_both$matches,
    0.7,
    is_allow_within_instrument_matches = FALSE
)
crosswalk_table_both$match_score <- round(crosswalk_table_both$match_score, 2)

# crosswalk table from web version
expected_crosswalk_table_both <- data.frame(
    pair_name = "GAD-7_GAD-7_7_CES-D_CES-D_10",
    question1_id = "GAD-7_GAD-7_7",
    question1_no = "GAD-7_7",
    question1_text = "Feeling afraid, as if something awful might happen",
    question2_id = "CES-D_CES-D_10",
    question2_no = "CES-D_10",
    question2_text = "I felt fearful.",
    match_score = 0.78
)

test_that("Generate crosswalk works for two instruments", {
    expect_equal(crosswalk_table_both, expected_crosswalk_table_both)
})