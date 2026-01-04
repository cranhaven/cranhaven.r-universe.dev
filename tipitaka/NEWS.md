# tipitaka 0.1.2

* Added 'LazyDataCompression: xz' to DESCRIPTION as requested to save space

# tipitaka 0.1.1

* `pali_sort` is now written in C++ and is about 400-500x faster than the  previous R version.

# tipitaka 0.1.0

* Added a `NEWS.md` file to track changes to the package.

## Known issues

### Volume numbering
Numbering of Tipitaka volumes is a bit of a mess. This is due to CST4 and PTS using somewhat different systems. Here is where things stand: 

* Volume numbering within the *Vinaya Pitaka* has been adjusted to match the PTS order. 
* Volume numbering within the *Abhidhamma Pitaka* is consistent between the two editions and is unchanged.
* Volume numbering within the *Dīgha Nikāya* is also consistent between the two.
* Volumes of the *Khuddaka Nikāya* are listed under their separate titles rather than by number, as is the norm for these works.
* Volume division and numbering within the *Majjhima Nikāya*, *Samyutta Nikāya*, and *Anguttara Nikāya* is **inconsistent** and has been left according to the CST4. 

The last of these is an annoyance and should be fixed in a future release. It requires some delicate editing of the underlying Pali raw files so as not to introduce new errors and I have not undertaken this yet.

### Stemming
It would be *hugely* valuable to provide a stemming function for Pali. Right now, every sequence of letters surrounded by spaces or punctuation is treated as a distinct word. That is not precisely correct for Pali, where where words can be joined together for phonetic reasons (a process called sandhi). There are also numerous declensions of most words (like dhamma, dhammo, etc.). On top of these two factors, there are also numerous compound words.

All fo this makes a good stemming algorithm both more valuable an dmore difficult. I have not attempted one yet, but I hope to get to it eventually.
