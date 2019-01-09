#
# Makefile for the Free Software Foundations g++ compiler
#
CC=g++
CFLAGS=-g -Wall
all: value ref big

big: big.cpp
	$(CC) -Wall -g -c big.cpp

ref: ref.cpp
	$(CC) -Wall -g -o ref ref.cpp

clean:
	rm big ref

