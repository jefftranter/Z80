5 '-------------------------------------------------------------
10 '		DTC.BAS		@
	Enchanted House : Deposit the Chair
11 '@
	by Furman Smith @
	the University of Houston Victoria Campus
23 '@
	This DTC.ASC program was modified from NIT.ASC which
24 '	appears in the July 1980 issue of RECREATIONAL COMPUTING
25 '	This listing (of DTC.ASC) is not very easy to follow but @
	the NIT.ASC listing given in RECREATIONAL COMPUTING is.
26 '@
	Documentation is in the file DTC.DOC of this disk.
27 '	One of the 1981 issues of RECREATIONAL COMPUTING should have
28 '	an article discussing the flow of this program and what is
29 '	needed to modify the program to build your own Enchanted House.
100 '@
	*** initializing
105 CLEAR250:GOSUB705:GOSUB1010:GOSUB745
110 PRINT"Are you the only person playing (YES or NO) ";:INPUTA$
115 P=1:A$=LEFT$(A$,1):IFA$="Y"THENP9=1:GOTO140
120 IFA$<>"y"ANDA$<>"n"THEN130
125 PRINT"Please use capitol letters.":GOTO110
130 PRINT"How many of you will be playing this time ";:INPUTP9
135 IFP9<1THENPRINT"Oh, come on.":GOTO130
140 IFP9>8THENA$="initials "ELSEA$="name "
145 DIMP$(P9):I=1:PRINT"Please tell me your ";A$:INPUTP$(P)
150 IFI=P9THEN165ELSEI=I+1
155 PRINT"Please give me the ";A$;" of the next player."
160 INPUTP$(I):PRINT"Thank you.":GOTO150
165 IFP9=1THENT2=T9ELSET2=4
200 '@
	*** describing locations & manifestations
205 PRINT CL$
210 IF W5(R)<2 THEN W5(R)=W5(R)+1
211 PRINT STRING$(64,46) :PRINT :PRINT"You are in ";
213 PRINT"the ";W4$(R);" Room."   :PRINT :IF W5(R)=2 THEN 235
215 I=1+INT((R-1)/8) :J=R-8*(I-1) :ON I GOSUB 220,225, :GOTO 235
220 ON J GOTO 2011,2021,2031,2041,2051,2061,2071,2081
225 ON J GOTO 2091,2101,2111,2121
235 FORM=1TOM9
240 IF L(M)=R AND W6(M)=1 THEN W6(M)=2 :GOSUB 265
245 NEXT M :PRINT
247 I=0 :PRINT "The "; :FOR M=1 TO M9
248 IF L(M)=R AND W6(M)>0 AND N1(M)>0 THEN I=I+1 ELSE 251
249 IF I>1 THEN PRINT ", ";
250 PRINT N$(N1(M));
251 NEXT M
252 IF I=0 THEN PRINT"room now has nothing for you to TAKE.":GOTO254
253 IF I=1 THENPRINT" might be important." ELSEPRINT" are here."
254 GOSUB 4705 :IF T<3 OR RND(1)>.05 THEN 260
255 PRINT"A voice says, <<Type HELP if you need help>>."
260 GOTO305
265 PRINT:I=1+INT((M-1)/8):J=M-8*(I-1)
270 ONIGOTO275,280
275 ON J GOTO 2411,2421,2431,2441,2451,2461,2471,2481
280 ON J GOTO 2491,2501,2511
300 '@
	*** determining player's actions
305 PRINT:PRINT"*-* *-* ";Q$(1+Q9*RND(1));
310 I=FRE(A$):IFRND(1)>.5THENPRINT", ";P$(P);
315 PRINT"?   *-* *-* *-*":PRINTTAB(8);:INPUTA$:I9=LEN(A$):PRINT
317 IF LEFT$(A$,1)=" " THEN A$=RIGHT$( A$, LEN(A$)-1 ) :GOTO 317
318 IF RIGHT$(A$,1)=" " THEN A$=LEFT$( A$, LEN(A$)-1 ) :GOTO 318
320 I9=LEN(A$) :IFI9=0THEN305
325 FORI=1TOI9:IFMID$(A$,I,1)<>" "THENNEXTI
330 V$=LEFT$(A$,I-1):IFI=I9+1THENN$=V$:GOTO345
335 FORJ=I9TOISTEP-1:IFMID$(A$,J,1)<>" "THENNEXTJ
340 N$=RIGHT$(A$,I9-J)
345 V=0:N=0
350 FORI=1TOV9:IFV$(I)=V$THENV=I:GOTO355ELSENEXTI
355 FORI=1TON9:IFN$(I)=N$THENN=I:GOTO357ELSENEXTI
357 IF0<NANDN<13THEN405
360 IFV*N>0OR(V+N>0ANDV$=N$)THEN380ELSE3505
365 PRINT"I don't understand ";A$;"."
370 PRINT"Type LIST WORDS to see a list of"
375 PRINT"some of the words that I understand.":GOTO305
380 IFN<29THEN405ELSEIFN=29THENN=11ELSEN=N-7
400 '@
	*** ordinary moving, getting, & dropping
405 IFN>4ANDN<9THENN=N-4
410 IFN>4ORN=0THEN435ELSEONNGOTO412,416,420,424
412 W1(R)=ABS(W1(R)):IFW1(R)=1THEN430
414 IFW1(R)=2THENR=R-R8:GOTO605ELSE505
416 W1(R+R8)=ABS(W1(R+R8)):IFW1(R+R8)=1THEN430
418 IFW1(R+R8)=2THENR=R+R8:GOTO605ELSE505
420 W2(R+1)=ABS(W2(R+1)):IFW2(R+1)=1THEN430
422 IFW2(R+1)=2THENR=R+1:GOTO605ELSE505
424 W2(R)=ABS(W2(R)):IFW2(R)=1THEN430
426 IFW2(R)=2THENR=R-1:GOTO605ELSE505
430 PRINT"You can't walk through walls.":GOSUB4705:GOTO305
435 IF 9 <= N AND N <= 12 THEN 4390 ELSE GOSUB 4015
445 IF M=0 AND 0<V AND V<=8 THEN 450 ELSE 455
450 PRINT"That's not possible now, ";P$(P);".":GOTO305
455 IFM=0ORV=12ORN=17THEN505
460 IF4<VANDV<9THEN475
465 IFL(M)>-1ANDL(M)<>RTHEN470ELSE475
470 PRINT"It seems that the ";N$;" isn't here.":GOTO305
475 IFV>8THEN505ELSEIFV<5THENV=1ELSEV=5
480 IFN=28THENPRINT"The WATCH stays on your wrist.":GOTO305
485 IF M<= 4 THEN PRINT "You cannot." :GOTO 5980
490 ON(V+3)/4GOSUB4045,4100
495 GOTO235
500 '@
	*** otherwise responding
505 IF 0 < N AND N <= 12 THEN 5405
510 IF N=13 OR N=14 THEN ON N-12 GOSUB 5010, 5055 :GOTO 305
515 ON V-8 GOTO 5505,5505,5105,5180,5230,5980,5305,5605,5655
600 '@
	*** updating
605 T=T+1:T1=T1+1
610 IFT>T9THENGOSUB5155ELSE625
615 PRINT:PRINT"Time is up -- the ";T9;" scoots have passed."
620 GOSUB805:PRINT:PRINT"Push RETURN to play again.":INPUTA$:RUN
625 IFW1(R)=-1THENW1(R)=1
630 IFW1(R+R8)=-1THENW1(R+R8)=1
635 IFW2(R)=-1THENW2(R)=1
640 IFW2(R+1)=-1THENW2(R+1)=1
645 IFT1<=T2THEN205ELSET1=0:IFP<P9THENP=P+1ELSEP=1
650 PRINT STRING$(20,35);" IT's ";P$(P);"'s turn now.":GOTO210
700 '@
	*** instructing player
705 FORJ=1TO24:PRINT:NEXTJ
710 PRINT"Let's pretend that you are in an enchanted house."
715 PRINT
720 PRINT"You should use one or two words to tell me what you"
725 PRINT"want to do.  I'll suggest messages at first."
730 PRINT:PRINT"If you have a question while you're inside ";
735 PRINT"the house,":PRINT"type HELP.":RETURN
745 PRINT:PRINT"When you have read all this, please"
750 PRINT"push the button that says RETURN (or ENTER).";TAB(59);
755 INPUTA$:PRINT:PRINT"You have to push that button after"
760 PRINT"typing any message to me." :RETURN
800 '@
	*** talking to programmers
805 PRINT:PRINT"I hope that you enjoyed your visit to this"
810 PRINT"Enchanted House which was built by Furman Smith."
815 PRINT:PRINT"If you know how to read computer programs that"
820 PRINT"are written in BASIC and you would like to build your"
825 PRINT"own Enchanted House, you should consult Furman's pair"
830 PRINT"of articles starting in the July-Aug 1980 issue of"
835 PRINT "RECREATIONAL COMPUTING.  Any version of Enchanted"
840 PRINT"House that you write is yours to sell, swap, or give"
845 PRINT"away (it would be polite to credit the articles"
850 PRINT"appearing in RECREATIONAL COMPUTING)."
855 PRINT"Push return for more information"; :INPUT A$
860 PRINT:PRINT"RECREATIONAL COMPUTING is published bimonthly"
862 PRINT"by a nonprofit organization.	Subscritions are $12/yr"
864 PRINT"as I type this (Nov. 1980).  Their address is:"
866 PRINT"1263 El Camino Real, Box E, Menlo Park, CA 94025."
895 RETURN
1000 '@
	*** details of initializing
1010 DEFINTA-V
1015 R6=3 :R7=2 :R8=2
1016 R9=R6*R7*R8 :R0=R9-R8+1 :R=R0
1020 V9=17:N9=25:M9=11
1025 DIMV$(V9),N$(N9),L(M9),N1(M9),W(M9),W4$(R9),W5(R9),W6(M9)
1030 DIMW1((R6*R7+1)*R8),W2(R9+1),W1$(3),W2$(3),W3$(9)
1035 T=0:T1=0:T9=72:C=0:C9=4
1040 I=RND(-PEEK(8219))
1045 Q9=4:DIMQ$(Q9)
1055 ESC$=CHR$(27) :YG$=ESC$+"F" :NG$=ESC$+"G" :CL$=ESC$+"E"
1110 FORI=1TOV9:READV$(I):NEXTI
1115 DATATAKE,GET,GRAB,PICK,DROP,GIVE,LEAVE,DEPOSIT,OPEN
1127 DATA UNLOCK, LIST, WORTH, HELP, GO, RELOOK, CONSULT, SWEEP
1210 FORI=1TON9:READN$(I):NEXTI
1215 DATA N,S,E,W,NORTH,SOUTH,EAST,WEST,U,D,UP,DOWN
1232 DATA INVENTORY,SCORE,WORDS,WATCH,DOOR,COBWEB,COUPON
1234 DATA KEY, SUPERBROOM, MAP, COIN, JEWEL, CHAIR
1335 FOR J =1 TO 9 :READ A$ :W3$(J)=YG$+A$+NG$ :NEXT J
1340 DATA e, u, d, v, b, t, f, s, c
1472 W1$(0)=YG$+"aaa* aaa"+NG$ : W2$(0)=YG$+"*"+NG$
1473 W1$(1)=YG$+"aaaaaaaa"+NG$ : W2$(1)=YG$+"`"+NG$
1474 W1$(2)=YG$+"aaa  aaa"+NG$ : W2$(2)=YG$+" "+NG$
1475 W1$(3)=YG$+"aaawwaaa"+NG$ : W2$(3)=YG$+"w"+NG$
1510 FORI=0TOR9:READW4$(I):W5(I)=0:NEXTI
1515 DATA	"   ?    "
1516 '
1532 DATA	"  Red3  ",	" Closet "
1534 DATA	"  Sun   ",     " Stairs3"
1535 '
1536 DATA	"  Red2  ",	" Green2 "
1538 DATA	"  Dark  ",	" Stairs2"
1539 '
1540 DATA	"  Store ",	" Green1 "
1542 DATA	"  Bank  ",	" Stairs1"
1543 '
1550 '	When in room R, we check W2(R) to see if we can go west
1551 '	and we check W2(R+1) to see if we can go east.
1552 '	Similary W1(R) tells us if we can go north
1554 '	and W1(R+R8) tells us if we can go south.
1560 '	Thus						@
		N					@
	      W   E	movement governed by		@
		S					@
				W1(R)			@
			W2(R)	room R	W2(R+1) 	@
				W1(R+R8)		@
			in general			@
	and in particular (this version) by		@

1590 '@
		W1(1)		W1(2)			@
	W2(1)	R=1	W2(2)	R=2	W2(3)		@
		W1(3)		W1(4)			@
	W2(3)	R=3	W2(4)	R=4	W2(5)		@
		W1(5)		W1(6)			@
	W2(5)	R=5	W2(6)	R=6	W2(7)		@
		W1(7)		W1(8)			@
	W2(7)	R=7	W2(8)	R=8	W2(9)		@
		W1(9)		W1(10)			@
	W2(9)	R=9	W2(10)	R=10	W2(11)
1592 '		W1(11)		W1(12)			@
	W2(11)	R=11	W2(12)	R=12	W2(13)		@
		W1(13)		W1(14)
1610 FORI=1TO(R6*R7+1)*R8:READW1(I):NEXTI
1632 @
     DATA 1,1, -2,-2, 1,1, -2,-1, 1,1, -2,-2.5, 1,1
1642 '					    ^		@
		(small doorway blocks CHAIR; W1(12) describes)
1710 FORI=1TOR9+1:READW2(I):NEXTI
1732 @
     DATA 1,-1, 1,-2, 1,-3.5, 1,-1, 1,-3.5, 1,-2, 1
1742 '			  ^		^		@
		( W2(6) for COBWEB ) ( W2(10) for locked DOOR )
1810 FORM=1TOM9 :READ L(M),N1(M) :W(M)=0 :W6(M)=1 :NEXTM
1817 DATA 9,0,	0,0,  5,18,  6,18,  6,19,  0,20,  2,21,  11,22
1819 DATA 1,23,  2,24,	7,25
1821 W6(3)=0 :W6(4)=0 :FOR I=9 TO 10
1823 L(I) = INT(1+R9*RND(1)) :IF L(I)=R0 THEN 1823 ELSE NEXT I
1830 W(9)=10 :W(10)=60 :W(11)=30
1910 FORI=1TOQ9:READQ$(I):NEXTI
1915 DATA"What are you going to do now","What now"
1920 DATA"What do you choose to do","What is your decision"
1995 RETURN
2000 '@
	*** details of describing rooms 		@

2010 '@
	R=1	Red3
2011 PRINT"You are at the top of a firefighter's bright red "
2012 PRINT"pole.  This room is called the Red3 Room since it is"
2013 PRINT"on the third floor and has the red pole."
2014 PRINT"The pole goes through a hole in the floor so "
2015 PRINT"you can slide down it." :RETURN
2020 '@
	R=2	Closet
2021 PRINT"This closet is so big that you could play ping pong"
2022 PRINT"in it." :RETURN
2030 '@
	R=3	Sun
2031 PRINT"Beautiful warm sunshine is flowing into this room"
2032 PRINT"through thin yellow curtains." :RETURN
2040 '@
	R=4	Stairs3
2041 PRINT"You are at the top of the spiraling stairs.	There"
2042 PRINT"are doorways to the north and west." :RETURN
2050 '@
	R=5	Red2
2051 PRINT"You are at the base of a firefighter's bright red"
2052 PRINT"pole.  The pole goes through the ceiling." :RETURN
2060 '@
	R=6	Green2
2061 PRINT"You are in a cozy room with a thick green carpet on"
2062 PRINT"the floor and a bright green pole comming out of a"
2063 PRINT"hole in the floor." :RETURN
2070 '@
	R=7	Dark
2071 PRINT"The only light in this room comes through the north"
2072 PRINT"doorway -- its so dark that you can barely see."
2073 RETURN
2080 '@
	R=8	Stairs2
2081 PRINT"This room is filled with the spiraling stairs."
2082 PRINT"UP and DOWN are the only ways you can go here."
2083 RETURN
2090 '@
	R=9	Store
2091 PRINT"This room is called the Store but there is no"
2092 PRINT"salesperson here. There is a DOOR to the east and a"
2093 PRINT"doorway to the south.  You could exit this room by"
2094 PRINT"typing GO SOUTH or SOUTH or just S." :RETURN
2100 '@
	R=10	Green1
2101 PRINT"You are at the base of a firefighter's pole.  The"
2102 PRINT"pole is colored bright green and runs UP through"
2103 PRINT"the ceiling.  There is a small doorway to the"
2104 PRINT"south and a DOOR to the west." :RETURN
2110 '@
	R=11	Bank
2111 PRINT"A carpet on the floor says << Welcome ";
2112 PRINT P$(P); " >>." :PRINT "A clock on the wall indicates"
2113 PRINT"that "; T; " scoots have passed of your visit."
2114 PRINT"You are invited for "; T9; " scoots."
2115 PRINT"A sign says that anything that you DEPOSIT here will"
2116 PRINT"be safe.  You earn points by depositing valuable";
2117 PRINT" things here."
2118 PRINT"Type NORTH or EAST to exit this room." :RETURN
2120 '@
	R=12	Stairs1
2121 PRINT"A spiraling staircase leads up from this room."
2122 PRINT"To the west is the Bank Room and there is a small"
2123 PRINT"doorway to the north. Type UP or CLIMB UP to go up."
2124 RETURN
2400 '@
	*** details of describing manifestations
2410 '@
	M=1	quiet machine
2411 PRINT"A machine here claims that a useful KEY can be yours"
2412 PRINT"if you DEPOSIT an appropriate COUPON here." :RETURN
2420 '@
	M=2	machine giving KEY
2421 PRINT"The machine makes a soft whirring sound and suddenly"
2422 PRINT"sends a machine arm out and LEAVEs a KEY. Then it"
2423 PRINT"GRABs the COUPON and toots its horn";
2424 FOR I=1 TO 3: FOR J=1 TO 100:NEXT J :PRINT CHR$(7); :NEXT I
2425 PRINT" 3 times." :L(2)=0 :L(1)=9 :RETURN
2430 '@
	M=3	Red2 Room side of COBWEB
2431 IF W2(6)<0 THEN RETURN
2432 PRINT"The east doorway is blocked by a fantastic COBWEB."
2433 RETURN
2440 '@
	M=4	Green2 Room side of COBWEB
2441 IF W2(6)<0 THEN RETURN
2442 PRINT"The west doorway is blocked by a fantastic COBWEB--"
2443 PRINT"I think that it will take a SUPERBROOM to remove it."
2444 RETURN
2450 '@
	M=5	COUPON
2451 PRINT"There is an old dirty COUPON here." :RETURN
2460 '@
	M=6	KEY
2461 PRINT"There is an old brass KEY here." :RETURN
2470 '@
	M=7	SUPERBROOM
2471 PRINT"There is a SUPERBROOM here." :RETURN
2480 '@
	M=8	magic MAP
2481 PRINT"The magic MAP is here.  It draws itself as you"
2482 PRINT"explore.  To see all the floors at once, you need to"
2483 PRINT"DROP MAP and then CONSULT MAP." :RETURN
2490 '@
	M=9	COIN
2491 PRINT"A heavy COIN is here." :RETURN
2500 '@
	M=10	JEWEL
2501 PRINT"Well, well, ";P$(P);", there's a JEWEL here." :RETURN
2510 '@
	M=11	CHAIR
2511 PRINT"A valuable-looking green CHAIR is here." :RETURN
3500 '@
	*** responding to unlisted words
3505 IFA$="EXTEND TIME"THENT9=T9+26:N=16:GOTO5605
3995 GOTO365
4000 '@
	*** details of getting and dropping
4015 IFV=0ORN=0THENM=0:RETURN
4020 IF N=18 AND (R=5 OR R=6) THEN M=R-2 :RETURN
4025 FORI=1TOM9:IFN1(I)=NTHENM=I:RETURNELSENEXTI
4040 M=0 :RETURN
4045 IFL(M)=RANDC<C9THEN4075ELSEIFC>=C9THEN4055
4050 PRINT"You've still got the ";N$:GOTO4140
4055 PRINT"You can only carry ";C9;" objects (not counting"
4060 PRINT"the WATCH) and you are already carrying that much."
4065 PRINT"If you really want to GET something else, then you"
4070 PRINT"will have to DROP something first.":GOTO4140
4075 IFW(M)=0THEN4090ELSEPRINT"Congratulations, ";P$(P)
4080 PRINT"You'll earn points when the ";N$;" is DEPOSITed (or"
4085 PRINT"DROPped) in the Bank.":C=C+1:L(M)=-1:RETURN
4090 PRINT"Ok; you are now carrying the ";N$;"."
4095 L(M)=-1:C=C+1:RETURN
4100 IFL(M)=-1THENPRINT"Ok.":L(M)=R:C=C-1ELSE4135
4105 IFR=R0THENGOSUB5070
4115 IF R=9 AND L(5)=9 THEN L(5)=0 :L(6)=9 :L(1)=0 :L(2)=9
4130 RETURN
4135 PRINT"You don't have the ";N$;"."
4140 PRINT"To see an inventory of what you're carrying, type"
4145 PRINT"INVENTORY.":RETURN
4300 '@
	*** details of up & down moving
4310 ' The NELLAN program (July-Aug RECREATIONAL COMPUTING) had
4315 ' H for [ # of floor that we're on ] but I now prefer the
4320 ' the following H-less method.
4390 I=1+INT((R-1)/8) :J = R - 8*(I-1)
4395 IF N=10 OR N=12 THEN 4455
4415 ON I GOTO 4416, 4417	' UP part
4416 ON J GOTO 4445,4445,4445,4445,4430,4445,4445,4440
4417 ON J GOTO 4445,4430,4445,4440
4430 IF L(11) <> -1 THEN 4440	' see before say (ok UP or not)
4435 PRINT"The CHAIR is too heavy to carry UP." :GOTO 305
4440 R = R-R7*R8 :GOTO 605				'ok ^
4445 PRINT"There's no way to GO UP here." :GOTO 305	'no way
4455 ON I GOTO 4456, 4457	' DOWN part
4456 ON J GOTO 4490,4495,4495,4490,4495,4490,4495,4490
4457 ON J GOTO 4495,4495,4495,4495
4490 R = R + R7*R8 :GOTO 605				'ok
4495 PRINT"There's no way to GO DOWN here." :GOTO 305	'no way
4500 '@
	*** drawing map
4505 I1=60:PRINT:PRINT"You are in the ";W4$(R);" Room now.";
4510 PRINTTAB(I1);"N":PRINTTAB(I1-2);"W + E"
4515 PRINTTAB(I1);"S"
4520 I2=R7*R8:FORI1=1TOR6
4523 IF L(8)=-1 THEN 4525 ELSE GOSUB 4530 :NEXT I1 :RETURN
4525 IF(I1-1)*I2<RANDR<=I1*I2THEN4530ELSENEXTI1
4530 K0=(I1-1)*I2:K9=I1*I2:K=K0:J=1
4531 PRINT ' Remove this line if the map is too tall for the crt
4535 IFK=K0ANDJ=1THENPRINTW3$(7);
4540 IFK=K0ANDJ>1ANDJ<R8+1THENPRINTW3$(8);
4545 IFK=K0ANDJ=R8+1THENPRINTW3$(9)
4550 IFK>K0ANDK<K9ANDJ=1THENPRINTW3$(4);
4555 IFK>K0ANDK<K9ANDJ>1ANDJ<R8+1THENPRINTW3$(5);
4560 IFK>K0ANDK<K9ANDJ=R8+1THENPRINTW3$(6)
4565 IFK=K9ANDJ=1THENPRINTW3$(1);
4570 IFK=K9ANDJ>1ANDJ<R8+1THENPRINTW3$(2);
4575 IFK=K9ANDJ=R8+1THENPRINTW3$(3)
4580 IFJ>R8THEN4595
4585 IFW1(K+J)>0THENPRINTW1$(W1(K+J));ELSEPRINTW1$(0);
4590 J=J+1:GOTO4535
4595 IFK=K9THENRETURNELSEJ=1
4600 IFW2(K+J)>0THENPRINTW2$(W2(K+J));ELSEPRINTW2$(0);
4605 IFJ>R8THENPRINT:GOTO4620
4610 IF W5(K+J) >0THENPRINTW4$(K+J);ELSEPRINTW4$(0);
4615 J=J+1:GOTO4600
4620 K=K+R8:J=1:GOTO4535
4700 '@
	*** listing walls
4705 A$ = "" :PRINT
4710 IF W1(R)=1 THEN A$=A$+"N, "
4715 IF W1(R+R8)=1 THEN A$=A$+"S, "
4720 IF W2(R)=1 THEN A$=A$+"W, "
4725 IF W2(R+1)=1 THEN A$=A$+"E, "
4730 IF A$="" THEN PRINT"Possible passages abound." :RETURN
4735 A$=LEFT$(A$,LEN(A$)-2): PRINT "There ";
4740 IF LEN(A$) > 1 THEN PRINT"are walls";ELSE PRINT"is a wall";
4745 PRINT" to the "; A$; "." :RETURN
5000 '@
	*** details of otherwise responding
5005 '@
				for INVENTORY
5010 PRINT"Your WATCH stays on your wrist for the whole visit."
5015 IFC=0THENPRINT"You aren't carrying anything else.":RETURN
5020 PRINT"Here is an inventory of what you're carrying:"
5025 FORJ=1TOM9
5030 IFL(J)=-1THENPRINTTAB(20);N$(N1(J)):NEXTJELSENEXTJ
5035 RETURN
5050 '@
				for SCORE
5055 GOSUB5070:PRINT"You have ";I;" points out of 100."
5060 PRINT"You earn points by DEPOSITing valuable things in ";
5065 PRINT"the Bank.":RETURN
5070 I=0 :FOR J=1 TO M9 :IF L(J)=R0 THEN I=I+W(J)
5075 NEXT J :IF I<100 THEN RETURN
5080 IF P9=1 THEN A$="" ELSE A$="all"
5082 PRINT :PRINT STRING$(64,42) :PRINT
5085 PRINT"A voice booms << You "; A$; " won! >>." :GOTO 620
5100 '@
				for LIST
5105 IF N=13 THEN GOSUB 5010 :GOTO 305
5107 IF N<>15 THEN 5980 ELSE PRINT "I know these verbs:  ";
5110 FORI=1TOV9:A$=V$(I):GOSUB5155:NEXTI:PRINT
5115 PRINT"I know these nouns:	";
5120 FORI=1TON9:A$=N$(I):GOSUB5155:NEXTI:PRINT
5125 PRINT"I know some one word messages (for example: HELP)"
5130 PRINT"and I know some two word messages (for examples:"
5135 PRINT"GRAB KEY, DROP COIN).  Whatever you say, I'll look"
5140 PRINT"at the first and last words;  you could say"
5145 PRINT"TAKE THE OLD BRASS KEY. If the last word is a"
5147 PRINT"direction, I'll respond to just it; I understand"
5149 PRINT"TIPTOE NORTH and SCOOT NORTH and N just as well as"
5151 PRINT"GO NORTH and you're welcome to say SLIDE DOWN, CLIMB"
5153 PRINT"UP, CRAWL QUICKLY TO THE EAST, E, etc." :GOTO 305
5155 PRINTA$;
5160 IFA$=V$(V9)ORA$=N$(N9)THENPRINT"."ELSEPRINT", ";
5165 IFPOS(1)<55THENRETURNELSEPRINT:RETURN
5175 '@
				for WORTH
5180 IFM=0THEN5980ELSEPRINT"The ";N$;" is worth ";W(M);
5185 PRINT" points.":IFW(M)=0THENPRINT"The ";N$;ELSE305
5190 PRINT" might be useful as a tool.":PRINT :GOTO 305
5225 '@
				for HELP
5230 PRINT:PRINT"Lost? --- CONSULT MAP"
5232 PRINT"To see all the floors, DROP MAP and"
5233 PRINT"		  then CONSULT MAP." :PRINT
5235 PRINT"Not sure what you're carrying? --- INVENTORY"
5240 PRINT"How many scoots are left? --- CONSULT WATCH"
5245 PRINT"Want the room redescribed? --- RELOOK"
5247 PRINT"How many points do you have? --- SCORE"
5250 PRINT"What's a certain thing worth? --- WORTH that thing"
5255 PRINT"Examples: WORTH COIN, WORTH CHAIR.":PRINT
5270 PRINT"Not sure how to say something? --- LIST WORDS":PRINT
5276 GOTO 305
5300 '@
		for RELOOK
5305 FOR M=1 TO M9 :IF L(M)=R AND W6(M)=2 THEN W6(M)=1
5310 NEXT M :W5(R)=1 :GOTO 211
5400 '@
				for N, S, E, W, U, D
5404 '		COBWEB barrier twix R=5 & R=6
5405 IF R=5 OR R=6 THEN IF L(3)=5 THEN 5410 ELSE 5420 ELSE 5430
5410 PRINT"Oh, ";P$(P);", the doorway is blocked by a COBWEB."
5415 W6(3)=1 :W6(4)=1 :W2(6)=3.5 :GOTO 305
5420 W2(6)=2.5 :R=11-R :GOTO 605
5429 '		small doorway twix R=10 & R=12
5430 IF N=1 OR N=2 THEN IF L(11)=-1 THEN 5435 ELSE 5445 ELSE 5455
5435 PRINT "That big CHAIR cannot fit through this small "
5440 PRINT "doorway!" :W1(12)=3.5 :GOTO 305
5445 W1(12)=2.5 :R=22-R :GOTO 605
5454 '		locked DOOR twix R=9 & R=10
5455 IF L(6)=-1 THEN 5475 ELSE PRINT "You don't have the KEY ";
5460 PRINT"and the DOOR is locked." :W2(10)=3.5 :GOTO 305
5475 R=19-R :W2(10)=2.5 :GOTO 605
5500 '@
				for OPEN and UNLOCK
5505 IF R=9 OR R=10 THEN IF N=17 THEN 5455 ELSE 5980
5510 PRINT"There's no DOOR in this room." :GOTO 305
5600 '@
				for CONSULT
5605 IF M=8 THEN GOSUB 4505 :GOTO 305 'opportunity by line 465
5610 IF N=16 THEN PRINT T;" scoots have passed and" ELSE 5980
5615 PRINT T9-T; "scoots remain." :GOTO 305
5650 '@
				for SWEEP
5655 IF NOT (N=18 OR N=21) THEN 5980
5660 IF NOT ( (R=5 OR R=6) AND L(7)=-1 AND L(3)=5 ) THEN 5675
5665 PRINT"You've swept the COBWEB away with your SUPERBROOM!"
5670 L(3)=0 :L(4)=0 :GOTO 305
5675 IF L(7)>-1 THEN PRINT"You need a SUPERBROOM." :GOTO 305
5680 IF L(3)=0 OR NOT(R=5 OR R=6) THEN PRINT"There's no ";
5685 PRINT "COBWEB here." :GOTO 305
5975 '@
				for defaulting
5980 PRINT"The ";N$;" is hard to ";V$;".":GOTO305
