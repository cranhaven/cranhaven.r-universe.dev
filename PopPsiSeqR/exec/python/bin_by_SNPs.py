#!/usr/bin/env python3
import argparse
from collections import defaultdict
from numpy import mean, median, sum

parser = argparse.ArgumentParser()
parser.add_argument("-i","--file_in", help="input file")
parser.add_argument("-o","--file_out", help="output file")
parser.add_argument("-b", "--bin_size", help='snps per bin', action = "store", default = 100)
parser.add_argument("-s","--slide_rate", help='bin slide rate', action = "store", default = 50)
parser.add_argument("-m","--map", help='list of mappings to apply per window', action = "store", default = "count,")
parser.add_argument("-c","--columns", help='list of columns to apply mappings to', action = "store", default = "2,")
parser.add_argument("-H","--header_skip", help='number of header lines to skip', action = "store", default = 0)

args = parser.parse_args()

phial_in = args.file_in
phial_out = args.file_out
bin_size = int(args.bin_size)
slide_rate = int(args.slide_rate)
mapping = args.map
columns = args.columns
header_skip = int(args.header_skip)

print(mapping)
print(columns)

mapping  = mapping.split(",")
columns  = columns.split(",")
while "" in mapping:
	mapping.remove("")
while "" in columns:
	columns.remove("")
columns = [int(c) for c in columns]
while 1 in columns:
	ndx = columns.index(1)
	print("operations on the contig/chromosome are trivial when binning by genomic window; ignoring " + mapping[i] + " on column 1 ..." )
	columns.pop(ndx)
	mapping.pop(ndx)

allowed = ["mean", "count", "min", "max", "sum" , "median", "distinct"]
disallowed = []
for i in range(len(mapping)):
	if mapping[i] not in allowed:
		disallowed.append(i)
disallowed.reverse()
for i in disallowed:
	print("operation " + mapping[i] + " on column "+ str(columns[i]) + " is not allowed, disregarding...." )
	mapping.pop(i)
	columns.pop(i)

if len(mapping) == 0:
	raise Exception("there are no operations to apply or columns to apply them to!")
elif len(mapping) != len(columns):
	raise Exception("there must be the same number of mappings as columns to apply them to!")




def count(w):
	return(len(w))

def distinct(w):
	return(len(set(w)))









def parse_Snps(phial_in):
	comparison_dict = defaultdict(list)
	parser = open(phial_in, 'r')

	for line in parser.readlines()[header_skip:]:
		split_line = line.split('\t')
		split_line = [s.strip() for s in split_line]
		split_line[1] = int(split_line[1])
		split_line[2] = int(split_line[2])
		comparison_dict[split_line[0]].append(split_line[1:])
	parser.close()
	return comparison_dict


#modular window reporting function
def window_aggregator(window_in, mapp, col):
	funk = eval(mapp)
	if mapp in ["count", "distinct"]:
		return funk([w[col] for w in window_in])
	else:
		return funk([float(w[col]) for w in window_in])

def window_slider(comparison_dict):
	windows_list = []
	contig_list = comparison_dict.keys()
	for contig in contig_list:
		snps = sorted(comparison_dict[contig])
		for i in range(0,len(snps)-bin_size+slide_rate,slide_rate):
			window = snps[i:i+bin_size]
			start = window[0][0]
			end = window[-1][1]
			nu_win = [contig, start, end]

			for todo in zip(mapping, columns):
				nu_win.append(window_aggregator(window, todo[0], todo[1]-2) ) # remember, we've cut off the first column, AND we're going from 0 based to 1 based coords

			windows_list.append(nu_win)
	return( windows_list)


def file_writer(win, phial):
	output = open(phial, 'w')
	for lyne in win:
		new_line = '%s\t'*len(lyne) % tuple(lyne)
		output.write(new_line.strip()+"\n")
	output.close()

comparison_dict = parse_Snps(phial_in)
windows = window_slider(comparison_dict)
file_writer(windows, phial_out)

