---
title: "Building Concorde for osX"
author: "Philip Bulsink"
date: '2017-03-26'
output: html_document
status: publish
published: true
layout: post
excerpt_separator: <!--more-->
maps: true
tags: R TSP Concorde osX 
---
 

 
I've been playing around with [Travelling Salesperson Problems (TSP)](https://en.wikipedia.org/wiki/Travelling_salesman_problem) recently. A package for R, `TSP`, contains most basic solvers, but it doesn't contain one of the best, Concorde.
 
Concorde is available as prebuilt binaries for many platforms, but not for osX. For Macs, it has to be downloaded and built from the source. It can be tough to build it on OSx, but after much digging I've found some instructions. It required the Way Back Machine to dig it out of some archives, so I'm reposting it here for posterity. 
 
It comes from [http://davidsjohnson.net/TSPcourse/mac-install-concorde.txt](http://davidsjohnson.net/TSPcourse/mac-install-concorde.txt), with only minor modifications to update the tsp example url.
 
This works on osX Sierra (10.12.3) with XCode 8.3, gcc 4.8.0, and clang-800.0.42.1. 
```
Introduction: No tutorial online gives up-to-date instructions about how to install concorde on a Mac. This
document intends to fill that gap.
 
Step 0. Make sure your mac has a working c compiler. Xcode uses clang, but tutorials online say you need gcc.
Clang is fine. Type "cc" in the terminal to make sure it is installed. If not, the computer will prompt you
to install Xcode terminal tools. Go ahead and install it.
 
Step 1. Download concorde from here: http://www.math.uwaterloo.ca/tsp/concorde/downloads/codes/src/co031219.tgz
Untar it to the desired location. The directory will be called "concorde"
 
Setp 2. Donwload the Linear Programming solver here: http://www2.isye.gatech.edu/~wcook/qsopt/beta/index.html
you want the qsopt.a and qsopt.h files. Make sure you use the 64 bit mac verions of the files.
Store those in a directory somewhere. For the purposes of this note, we will call the directory "QSOPT"
 
Step 3. cd to the concorde directory
 
Step 4. In the terminal type: 
 
export QSOPTDIR=path/to/QSOPT
export CFLAGS="-g -O3 -arch x86_64"
./configure --with-qsopt=$QSOPTDIR --host=darwin
make
 
Step 6. To test it, download a practice tsp instance. A common one (d493) is available from here: https://raw.githubusercontent.com/mhahsler/TSP/master/inst/examples/d493.tsp
 
Step 7. cd to "concorde/TSP/" then execute "./concorde path/to/d493.tsp" 
 
My macbook retina (the one that came out
before the recent haswell processor version) uses OSX version 10.9.1 and has a dual core i7 with 8GB ram.
It solved d493.tsp in 48.62 seconds.
 
credits:
http://davidsjohnson.net/TSPcourse/mac-install-concorde.txt
http://wiki.evilmadscientist.com/Obtaining_a_TSP_solver
http://stackoverflow.com/questions/18500923/issue-with-archive-which-is-not-the-architecture-being-linked-x86-64-in-c
https://raw.githubusercontent.com/mhahsler/TSP/master/inst/examples/d493.tsp
 
```
 
Again, this is not my work, but just a repost for posterity. Newer Macbooks can do better, I'm sure, but my old 2012 MacBookPro with osX Sierra (10.12.3) finished in just over 30 seconds. 
