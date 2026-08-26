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


questions_en <- list("I feel nervous", "I don't feel nervous")
instrument_en <- create_instrument_from_list(question_texts = questions_en)

# test single instrument with negation on as default
match <- match_instruments(instrument_en)

test_that("Single instrument with negation on as default", {
    expect_equal(2, length(match$instruments[[1]]$questions))
    expect_equal(2, length(match$matches))
    expect_lt(0.99, match$matches[[1]][[1]])
    expect_gt(0, match$matches[[1]][[2]])
    expect_lt(0.99, match$matches[[2]][[2]])
    expect_gt(0, match$matches[[2]][[1]])
})

# test single instrument without negation
match <- match_instruments(instrument_en, is_negate = FALSE)

test_that("Single instrument without negation", {
    expect_equal(2, length(match$instruments[[1]]$questions))
    expect_equal(2, length(match$matches))
    expect_lt(0.99, match$matches[[1]][[1]])
    expect_lt(0, match$matches[[1]][[2]])
    expect_lt(0.99, match$matches[[2]][[2]])
    expect_lt(0, match$matches[[2]][[1]])
})