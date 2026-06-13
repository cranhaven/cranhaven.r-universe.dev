#' voiceR test Audio Data
#'
#'
#' A test audio features data.frame, obtained by using autoExtract() on
#' the extended version of testAudioList, found
#' <a href="https://osf.io/zt5h2/?view_only=348d1d172435449391e8d64547716477">here</a>.
#'
#'
#' @name testAudioData
#' @docType data
#' @format `testAudioData`
#' A data.frame containing 90 observations and 11 variables, which is the result of
#' applying the autoExtract() function to the extended version of the data found
#' on testAudioList. This data.frame contains several voice features for 90 audio
#' files, which correspond to 15 English-speaking participants.
#' Participants first completed a Baseline Voice Task in which they were
#' instructed to read two predefined phrases ((1) Bar: "I go to the bar",
#' (2) Beer: "I drink a beer") aloud in their normal (neutral) voice.
#' Participants were then instructed to read the predefined phrases in either
#' a happy or a sad voice. The experimenter requested
#' each emotion one at a time and in a random sequence to counter-order effects.
#' Participants were then asked to describe their experience of mimicking the
#' stated phrases for each of the specified emotional states.
#' Thus the data contains 6 observations per participant: two observations for
#' the neutral state, two for the happy simulated state, and two for the sad
#' simulated state.
#' Below we also provide information about the columns this data.frame contains:
#' \describe{
#'   \item{ID}{Participant identifier}
#'   \item{Condition}{refers to the intention or emotional aspect that
#'   the speaker is conveying: Happy, or Sad. This component makes
#'   reference to the main point that we want to compare in our data; in the
#'   voiceR package the main comparison component is called Condition, because
#'   it usually refers to experimental conditions.}
#'   \item{Dimensions}{Phrase participants read: (1) Bar: "I go to the bar";
#'   (2): Beer: "I drink a beer"}
#'   \item{duration}{Total duration in seconds.}
#'   \item{voice_breaks_percent}{Proportion of unvoiced frames.}
#'   \item{RMS_env}{Root mean square of the amplitude envelope.}
#'   \item{mean_loudness}{Average subjective loudness in sone.}
#'   \item{mean_F0}{Average fundamental frequency in Hertz.}
#'   \item{sd_F0}{Standard deviation of the fundamental frequency in Hertz.}
#'   \item{mean_entropy}{Average Wiener entropy. A value of 0 indicates a pure tone, while a value of 1 indicates white noise.}
#'   \item{mean_HNR}{Average Harmonics-to-Noise Ratio.}
#' }
#' @examples
#' data(testAudioData)
NULL
