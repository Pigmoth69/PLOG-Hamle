:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(aggregate)).
:- use_module(library(random)).
:- use_module(library(system)).

:- include('menus.pl').
:- include('utilities.pl').
:- include('board.pl').
:- include('game.pl').


hamle:-
	startSeed,
	mainMenu.