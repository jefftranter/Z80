10 '		NIT.BAS
40 '------------ Nellan is Thirsty ---
41 ' by Furman Smith 
42 '    University of Houston Victoria Campus
43 '    Victoria, Tx 77901
44 '
45 ' nice listing & documentation in the
46 ' Jul/Aug 1980 RECREATIONAL COMPUTING
99 '
105 CLEAR250:GOSUB705:GOSUB1010:GOSUB745
110 INPUT"Are you the only person playing (YES or NO) ";A$
115 P=1:A$=LEFT$(A$,1):IFA$="Y"THENP9=1:GOTO140
120 IFA$<>"y"ANDA$<>"n"THEN130
125 PRINT"Please use capitol letters.":GOTO110
130 INPUT"How many of you will be playing this time ";P9
135 IFP9<1THENPRINT"Oh, come on.":GOTO130
140 IFP9>8THENA$="initials "ELSEA$="name "
145 DIMP$(P9):I=1:PRINT"Please tell me your ";A$:INPUTP$(P)
150 IFI=P9THEN165ELSEI=I+1
155 PRINT"Please give me the ";A$;" of the next player."
160 INPUTP$(I):PRINT"Thank you.":GOTO150
165 IFP9=1THENT2=T9ELSET2=4
205 FOR J=1 TO 3 :PRINT :NEXT J '***for hardcopy
210 W4(R)=1:I=1+INT((R-1)/R8):J=R-R8*(I-1)
215 ONIGOSUB220,225,230:GOTO235
220 ONJGOTO2010,2020,2030
225 ONJGOTO2040,2050,2060
230 ONJGOTO2070,2080,2090
235 FORM=1TOM9
240 IFL(M)=RTHENGOSUB265
245 NEXTM
250 IFRND(1)>T*(2-W4(1)-W4(3))/(7*T9)THEN260
255 PRINT:PRINT"A voice says, <<Type HELP if you need help>>."
260 GOTO305
265 PRINT:I=1+INT((M-1)/8):J=M-8*(I-1)
270 ONIGOTO275,280,285,290
275 ONJGOTO2410,2420,2430,2440,2450,2460,2470,2480
280 ONJGOTO2490,2500,2510,2520,2530,2540,2550,2560
285 ONJGOTO2570,2580,2590,2600,2610,2620,2630,2640
290 ONJGOTO2650
305 PRINT:PRINT"*-* *-* ";Q$(1+Q9*RND(1));
310 I=FRE(A$):IFRND(1)>.5THENPRINT", ";P$(P);
315 PRINT"?":PRINTTAB(8);:INPUTA$:I9=LEN(A$):PRINT
320 IFI9=0THEN305
325 FORI=1TOI9:IFMID$(A$,I,1)<>" "THENNEXTI
330 V$=LEFT$(A$,I-1):IFI=I9+1THENN$=V$:GOTO345
335 FORJ=I9TOISTEP-1:IFMID$(A$,J,1)<>" "THENNEXTJ
340 N$=RIGHT$(A$,I9-J)
345 V=0:N=0
350 FORI=1TOV9:IFV$(I)=V$THENV=I:GOTO355ELSENEXTI
355 FORI=1TON9:IFN$(I)=N$THENN=I:GOTO357ELSENEXTI
357 IF0<NANDN<9THEN405
360 IFV*N>0OR(V+N>0ANDV$=N$)THEN380ELSE3505
365 PRINT"I don't understand ";A$;"."
370 PRINT"Type LIST WORDS to see a list of"
375 PRINT"some of the words that I understand.":GOTO305
380 IFN<29THEN405ELSEIFN=29THENN=11ELSEN=N-7
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
430 PRINT"You can't walk through walls.":GOTO305
435 IFN<>12ANDN<>22THENGOSUB4015ELSEGOSUB4030
440 IF(V<=4ORV=13)ANDN=22THEN5410
445 IFM<>0ORV=0ORV>8THEN455
450 PRINT"That's not possible now, ";P$(P);".":GOTO305
455 IFM=0ORV=15ORN=9THEN505
460 IF4<VANDV<9THEN475
465 IFL(M)>-1ANDL(M)<>RTHEN470ELSE475
470 PRINT"It seems that the ";N$;" isn't here.":GOTO305
475 IFV>8THEN505ELSEIFV<5THENV=1ELSEV=5
480 IFN=28THENPRINT"The WATCH stays on your wrist.":GOTO305
485 IFN=10ORN=23ORN=24THEN5980
490 ON(V+3)/4GOSUB4045,4100
495 GOTO235
505 IFN<=8ANDN>0THEN5015
510 IFN=26ORN=27THENON(N-25)GOSUB5105,5155:GOTO305
512 IFV=0THEN365
515 IFV<=14THENJ=V-8:GOTO520ELSEJ=V-14:GOTO525
520 ONJGOTO5205,5255,5355,5355,5405,5605
525 ONJGOTO5705,5755,5780,5805,5980,210
605 T=T+1:T1=T1+1
610 IFT>T9THENGOSUB5155ELSE625
615 PRINT:PRINT"Time is up -- the ";T9;" scoots have passed."
620 PRINT:INPUT"If you want to play again, push return.";A$:RUN
625 IFW1(R)=-1THENW1(R)=1
630 IFW1(R+R8)=-1THENW1(R+R8)=1
635 IFW2(R)=-1THENW2(R)=1
640 IFW2(R+1)=-1THENW2(R+1)=1
645 IFT1<=T2THEN205ELSET1=0:IFP<P9THENP=P+1ELSEP=1
650 PRINT"   #    ";"It's ";P$(P);"'s turn now.":PRINT:GOTO210
705 PRINTCHR$(27);"E"
710 PRINT"Let's pretend that you are in an enchanted house."
715 PRINT
720 PRINT"You should use one or two words to tell me what you"
725 PRINT"want to do.  I'll suggest messages at first."
730 PRINT:PRINT"If you have a question while you're inside ";
735 PRINT"the house,":PRINT"type HELP.":RETURN
745 PRINT:PRINT"When you have read all this, please"
750 PRINT"push the button that says RETURN (or ENTER).";TAB(59);
755 INPUTA$:PRINT:RETURN
1010 DEFINTA-V
1015 R6=1:R7=3:R8=3:R9=R6*R7*R8:R0=R9-R8+1:R=R0
1020 V9=20:N9=31:M9=25
1025 DIMV$(V9),N$(N9),L(M9),N1(M9),W(M9),W4(R9),W4$(R9)
1030 DIMW1((R6*R7+1)*R8),W2(R9+1),W1$(3),W2$(3),W3$(9)
1035 T=0:T1=0:T9=72:C=0:C9=4
1040 I=RND(-PEEK(8219))
1045 Q9=4:B9=4:DIMQ$(Q9),B$(B9)
1050 ' the following is for Heath graphics (H17 & H89)
1051 ' YG$ for Yes Graphics & NG$ for No Graphics
1052 ESC$ = CHR$(27)  :YG$=ESC$+"F"   :NG$=ESC$+"G"
1110 FORI=1TOV9:READV$(I):NEXTI
1115 DATATAKE,GET,GRAB,PICK,DROP,GIVE,OFFER,DEPOSIT
1120 DATAKICK,CONSULT,OPEN,UNLOCK,FILL,LIST,WORTH,SIT
1125 DATADRINK,HELP,GO,RELOOK
1210 FORI=1TON9:READN$(I):NEXTI
1215 DATAN,S,E,W,NORTH,SOUTH,EAST,WEST,DOOR,CHAIR
1220 DATARABBIT,BOWL,COUPON,WRAPPER,MAP,KEY,EGG,COIN
1225 DATAJEWEL,CATALOG,VASE,MILK,BANKER,CAT,WORDS
1230 DATAINVENTORY,SCORE,WATCH,CHULA,KLINKOYN,NELLAN
1310 ' See lines 48-50 for why these comments.
1335 FOR J=1 TO 9 :READ A$ :W3$(J)=YG$+A$+NG$ :NEXT J
1340 DATA " ea", "aua", "ad ", " va", "aba", "at ", " fa", "asa", "ac "
1472 W1$(0) = YG$+"aaa* aaa"+NG$ : W2$(0) = YG$+" * "+NG$
1473 W1$(1) = YG$+"aaaaaaaa"+NG$ : W2$(1) = YG$+" ` "+NG$
1474 W1$(2) = YG$+"aaa  aaa"+NG$ : W2$(2) = YG$+"   "+NG$
1475 W1$(3) = YG$+"aaawaaaa"+NG$ : W2$(3) = YG$+" w "+NG$
1510 FORI=0TO9:READW4$(I):W4(I)=0:NEXTI
1515 DATA"   ?    "
1520 DATA" Office ","  Milk  ","  Store "
1525 DATA" White  ","  Gold  ","  Hot   "
1530 DATA" Bank   ","  Green ","  Cat   "
1610 FORI=1TO(R6*R7+1)*R8:READW1(I):NEXTI
1615 DATA1,1,1
1620 DATA-2.5,-2,-1
1625 DATA-2,-1,-2
1630 DATA1,1,1
1710 FORI=1TOR9+1:READW2(I):NEXTI
1715 DATA1,-1,-2.5
1720 DATA1,-2,-2
1725 DATA1,1,-2,1
1810 FORM=1TOM9:READL(M),N1(M):W(M)=0:NEXTM
1815 DATA1,20,6,13,0,12,0,12,0,12,2,9,3,0,0,0,7,23
1820 DATA1,16,9,24,0,24,0,24,0,24,0,11,0,11,0,11,4,11
1825 DATA7,15,2,14,1,0,8,18,0,17,3,19,0,21
1830 FORI=22TO25:W(I)=25:NEXTI
1910 FORI=1TOQ9:READQ$(I):NEXTI
1915 DATA"What are you going to do now","What now"
1920 DATA"What do you choose to do","What is your decision"
1925 FORI=1TOB9:READB$(I):NEXTI
1930 DATAhappy,"glad to see you",busy,sleepy
1995 RETURN
2010 PRINT"Well, ";P$(P);", you are in the Office."
2011 PRINT"You are standing next to a CHAIR which is next to a"
2012 PRINT"desk in a room with yellow walls and brown carpet."
2013 PRINT"This is a room of study and joy."
2014 PRINT"There is a door in the south wall.":RETURN
2020 PRINT"This is the Milk Room.  If you have a BOWL, you can"
2022 PRINT"say FILL BOWL or GET MILK and have your BOWL"
2023 PRINT"filled with nice cold MILK. There are doorways to"
2024 IFL(15)=-1ANDL(6)=2THENL(18)=0:L(17)=2
2025 PRINT"the south and east.":RETURN
2030 PRINT"You are in the Store.":RETURN
2040 PRINT"Now you are in the White Room--the walls are white."
2041 PRINT"There is a wall to the west.  You know that the Bank"
2042 PRINT"is through the south doorway.  There's a door to"
2043 PRINT"the north and a doorway to the east.":RETURN
2050 PRINT"All the walls in this room are painted gold."
2052 PRINT"There are exits to the north, east, and west."
2053 IFL(15)=-1ANDL(23)=0THEN2054ELSERETURN
2054 L(15)=0:L(16)=5:L(23)=5:RETURN
2060 PRINT"Oh it's hot in here.  You are in the Hot Room and if"
2061 PRINT"cold MILK were in this room, it would be warmed."
2062 PRINT"The MAP shows the doorways."
2063 IFL(4)=-1THENL(4)=0:L(5)=-1:RETURNELSERETURN
2070 PRINT"You are in a room called the Bank.  A giant carpet"
2071 PRINT"on the floor says  WELCOME ";P$(P);"."
2075 PRINT"A screen says that you are invited to visit for ";T9
2076 PRINT"scoots.  ";T;" scoots have passed.  To leave this"
2078 PRINT"room, type NORTH or N or GO NORTH.":RETURN
2080 PRINT"You are standing in a room with green walls."
2082 PRINT"A green CHAIR is in the corner."
2083 IFW1(4)<0THENRETURNELSEPRINT"A sign here says:"
2084 PRINTTAB(20);STRING$(25,95)
2085 PRINTTAB(20);"|";TAB(26);"Magic works";TAB(44);"|"
2086 PRINTTAB(20);"|";TAB(29);"in the";TAB(44);"|"
2087 PRINTTAB(20);"|";TAB(24);"Enchanted House.";TAB(44);"|"
2088 PRINTTAB(20);"|";STRING$(23,95);"!":RETURN
2090 PRINT"You are in the room where ";N$(31);" lives; this"
2091 PRINT"room called the Cat room.":RETURN
2410 PRINT"There is a college CATALOG from the University of"
2411 PRINT"Houston Victoria Campus here.":RETURN
2420 PRINT"On the floor is a COUPON.  According to what is"
2421 PRINT"written on the COUPON, if it is DROPped in the Store"
2422 PRINT"while the machine is working, it will be good for one"
2423 PRINT"blue BOWL.":RETURN
2430 PRINT"There is an clean empty blue BOWL here.":RETURN
2440 PRINT"The BOWL of cold MILK is here.":RETURN
2450 PRINT"There is a blue BOWL with warm MILK.":RETURN
2460 RETURN
2470 PRINT"In a corner of the room is a machine which announces"
2472 PRINT"that if you DEPOSIT (or DROP) a COUPON and the"
2473 PRINT"machine is working, you will receive whatever the"
2474 PRINT"coupon is good for.  If the machine doesn't work,"
2475 PRINT"you are invited to call the repairperson.":RETURN
2480 PRINT"The machine in the corner has a flashing display"
2481 PRINT"announcing <<out of BOWLs>>.":RETURN
2490 PRINT"The BANKER, Mr ";N$(30);", looks ";
2492 PRINTB$(1+B9*RND(1));"."
2493 IFL(23)=R0THENPRINT"Type SCORE to check your score."
2494 RETURN
2500 PRINT"There is an old brass KEY here.":RETURN
2510 PRINTN$(31);" the CAT is here.  She pretty-much fills up"
2511 PRINT"the room; she's as large as an elephant--no kidding."
2512 PRINT"In a soft polite voice she says, << I'd love to have"
2513 PRINT"some nice cold MILK.  Many of my friends have tried"
2514 PRINT"to deliver cold MILK here and many have failed."
2515 PRINT"Perhaps you, ";P$(P);", would be kind enough"
2516 PRINT"to try.  I must warn you:  It will not be easy >>."
2517 RETURN
2520 PRINTN$(31);" purrs and says, <<Thank you,"
2522 PRINTP$(P);", for OFFERing me this good MILK>>."
2523 PRINT"She doesn't mention that it was warm."
2525 L(5)=0:L(3)=9:L(12)=0:L(11)=9:RETURN
2530 PRINT"Wow! ";N$(31);" can sure be loud when she purrs."
2531 PRINT"She looks at you and says, << ";P$(P);", you've"
2532 PRINT"made me so happy.  I know that it wasn't easy to get"
2533 PRINT"this BOWL of cold MILK to me.  You did it!"
2534 L(4)=0:L(3)=9:L(13)=0:L(14)=9:IFL(25)<>0THEN2537
2535 PRINT"Please accept this small present >>.  She offers you"
2536 PRINT"a small ";N$(21);".":L(25)=9
2537 RETURN
2540 PRINTN$(31);" is napping.  She's quite happy.":RETURN
2550 PRINTN$(29);" the white RABBIT is here and she says"
2551 PRINT"<<Well, my friend ";P$(P);", I hope that"
2552 PRINT"you are enjoying your adventure>>.":RETURN
2560 PRINTN$(29);" hops out of your arms and scurries off. She"
2561 PRINT"returns and says  <<Please TAKE this golden EGG that"
2562 PRINT"I hid here in the Gold Room>>."
2563 L(23)=5:L(15)=5:L(16)=0:C=C-1:RETURN
2570 PRINT:PRINTN$(29);" hops down and gobbles a growth of "
2571 PRINT"carrots that was blocking the east doorway.  She "
2572 PRINT"smiles and says,<< Thanks, friend ";P$(P);" >>."
2573 L(17)=0:L(15)=2:L(6)=0:C=C-1:RETURN
2580 PRINT"A white RABBIT is here.  She hops to you and says,"
2581 PRINT"<<I am ";N$(29);". Welcome to the Enchanted House,"
2582 PRINT"my friend, ";P$(P);">>.  She hugs you and says,"
2583 PRINT"<< I have a present saved for you in the room to"
2584 PRINT"the east of here.  If you say TAKE RABBIT ( or"
2585 PRINT"TAKE ";N$(29);") and next say GO EAST (or E or"
2586 PRINT"EAST), you can carry me to the Gold Room>>."
2587 L(18)=0:L(15)=4:RETURN
2590 PRINT"There is a magic MAP here.  If you GET the MAP, you"
2591 PRINT"may later CONSULT the MAP.":RETURN
2600 PRINT"An old gum WRAPPER is on the floor.":RETURN
2610 PRINT"A voice says, << Furman Smith's July 1980 ";
2611 PRINT"Recreational Computing":PRINT"article helps ";
2612 PRINT"programmers remodel this Enchanted House. >>.":RETURN
2620 PRINT"A heavy COIN is here.":RETURN
2630 PRINT"A golden EGG is on the floor.":RETURN
2640 PRINT"On the floor is a JEWEL.":RETURN
2650 PRINT"A magnificent VASE is here.":RETURN
3505 IFA$="EXTEND TIME"THENT9=T9+24:N=28:GOTO5315
3995 GOTO365
4015 IFV=0ORN=0THENM=0:RETURN
4020 IFN=24THENIFL(11)=9THENM=11:RETURNELSEM=14:RETURN
4025 FORI=1TOM9:IFN1(I)=NTHENM=I:RETURNELSENEXTI
4030 M=0:IFN<>22ANDN<>12THENRETURN
4035 FORJ=3TO5:IFL(J)<>0THENM=J:NEXTJELSENEXTJ
4040 RETURN
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
4105 IFR=R0THENGOSUB5175
4110 IFR<>3ANDR<>9THENRETURN
4115 IFL(2)=3THENL(2)=0:L(3)=3:L(7)=0:L(8)=3
4120 IFL(4)=9THENL(4)=0:L(3)=9:L(11)=0:L(13)=9
4125 IFL(5)=9THENL(5)=0:L(3)=9:L(11)=0:L(12)=9
4130 RETURN
4135 PRINT"You don't have the ";N$;"."
4140 PRINT"To see an inventory of what you're carrying, type"
4145 PRINT"INVENTORY.":RETURN
4310 I2=R7*R8:FORH=1TOR6
4315 IFH*(I2-1)<RANDR<=H*I2THEN4325ELSENEXTH
4325 GOTO4390
4390 I=1+INT((R-1)/8):J=R-8*(I-1)
4405 IFH=1THEN4445
4435 PRINT"The CHAIR is too heavy to carry UP.":GOTO305
4440 R=R-R7*R8:GOTO605
4445 PRINT"There's no way to GO UP here.":GOTO305
4505 I1=60:PRINT:PRINT"You are in the ";W4$(R);" Room now.";
4510 PRINTTAB(I1);"N":PRINTTAB(I1-2);"W + E"
4515 PRINTTAB(I1);"S"
4520 I2=R7*R8:FORI1=1TOR6
4525 IF(I1-1)*I2<RANDR<=I1*I2THEN4530ELSENEXTI1
4530 K0=(I1-1)*I2:K9=I1*I2:K=K0:J=1
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
4610 IFW4(K+J)>0THENPRINTW4$(K+J);ELSEPRINTW4$(0);
4615 J=J+1:GOTO4600
4620 K=K+R8:J=1:GOTO4535
5015 IFR=1THENIFL(10)<>-1THEN5020ELSE5030ELSE5040
5020 PRINT"The door is locked and you're not carrying the key."
5025 W1(4)=3.5:GOTO305
5030 W1(4)=2.5:R=4:GOTO605
5040 IFR=2THENIFL(6)=2THEN5045ELSE5060ELSE5070
5045 PRINT"You cannot enter--the doorway is blocked by a thick"
5050 PRINT"growth of carrots."
5055 W2(3)=3.5:GOTO305
5060 R=3:W2(3)=2.5:GOTO605
5070 IFR=4THENIFL(10)<>-1THEN5020ELSE5080
5075 R=2:GOTO605
5080 W1(4)=2.5:R=1:GOTO605
5105 PRINT"Your WATCH stays on your wrist for the whole visit."
5110 IFC=0THENPRINT"You aren't carrying anything else.":RETURN
5115 PRINT"Here is an inventory of what you're carrying:"
5120 FORJ=1TOM9
5125 IFL(J)=-1THENPRINTTAB(20);N$(N1(J)):NEXTJELSENEXTJ
5130 RETURN
5155 GOSUB5175:PRINT"You have ";I;" points out of 100."
5160 PRINT"You earn points by DEPOSITing valuable things in ";
5165 PRINT"the Bank.":RETURN
5175 I=0:FORJ=1TOM9:IFL(J)=R0THENI=I+W(J)
5180 IFP9>1THENA$="all "ELSEA$=""
5185 NEXTJ:IFI<100THENRETURNELSEPRINT:PRINT"Mr ";N$(30);
5190 PRINT" shouts, << You ";A$;"won! >>.";CHR$(7):GOTO620
5205 PRINT"Kicking ";N$;" will not help anything.":GOTO305
5255 IFNOT(N=11ORN=23ORN=24)THEN5305
5260 IFL(6)=0THEN5275
5265 PRINT"The ";N$(N1(M));" laughs and says, <<Well, I know"
5270 PRINT"that you know a carrot-liking RABBIT>>.":GOTO305
5275 ON2-W4(1)GOTO5280,5290
5280 PRINT"The ";N$(N1(M));" says, << ";P$(P);","
5285 PRINT"I think that you're doing great >>.":GOTO305
5290 PRINT"The ";N$(N1(M));" smiles mysteriously and says,"
5295 PRINT"<< I recommend that you SIT in the CHAIR >>."
5300 GOTO305
5305 IFN=15THENGOSUB4505:GOTO305
5310 IFN=20THENPRINT"It doesn't help here.":GOTO305
5315 IFN=28THENPRINTT;" scoots have passed"
5320 IFN=28THENPRINT"& ";T9-T;" scoots remain.":GOTO305
5325 GOTO5980
5355 IFN=9THEN5015ELSE5980
5405 IFN<>12THENIFN<>24THEN5980ELSEV=5:N=12:GOTO435
5410 IFR<>2ORL(3)<>-1THEN5425ELSEL(3)=0:L(4)=-1
5415 PRINT"Glurg; glurg; plink.  Your BOWL has cold MILK now."
5420 GOTO305
5425 IFN=22ANDM=3THENPRINT"The BOWL is empty.":N$="BOWL"
5430 IFV<>13THENIFL(M)=RTHENGOSUB4045:GOTO305
5435 IFM=4ORM=5THENPRINT"The BOWL is still full of MILK."
5440 IFR<>2THENPRINT"The Milk Room has lots of MILK."
5445 IFL(M)>-1ORM<3ORM>5THENPRINT"You need a BOWL."
5450 GOTO305
5605 IFN<>25THEN5980ELSEPRINT"I know these verbs:  ";
5610 FORI=1TOV9:A$=V$(I):GOSUB5655:NEXTI
5615 PRINT"I know these nouns:  ";
5620 FORI=1TON9:A$=N$(I):GOSUB5655:NEXTI
5625 PRINT"I know some one word messages (for example: HELP)"
5630 PRINT"and I know some two word messages (for examples:"
5635 PRINT"FILL BOWL, OFFER MILK).  Whatever you say, I'll look"
5640 PRINT"at the first and last words;  you could say"
5645 PRINT"SIT IN THE CHAIR or CONSULT THE SLEEPY MR ";N$(30);
5650 PRINT".":GOTO305
5655 PRINTA$;
5660 IFA$=V$(V9)ORA$=N$(N9)THENPRINT"."ELSEPRINT", ";
5665 IFPOS(1)<55THENRETURNELSEPRINT:RETURN
5705 IFM=0THEN5980ELSEPRINT"The ";N$;" is worth ";W(M);
5710 PRINT" points.":IFW(M)=0THENPRINT"The ";N$;ELSE305
5715 PRINT" might be useful as a tool.":PRINT
5720 IFN=11ORN=23ORN=24THENPRINT"Friends are valuable."
5725 IFN=20THENPRINT"A college education is a worthwhile and"
5730 IFN=20THENPRINT"stimulating  experience"
5735 GOTO305
5755 IFN<>10THEN5980ELSEIFR=1ORR=8THENR=9-R:GOTO605
5760 PRINT"There's no CHAIR in here.":GOTO305
5780 IFN<>22THEN5980ELSEIFL(4)=-1ORL(5)=-1THEN5790
5785 PRINT"You're not carring any MILK.":GOTO305
5790 PRINT"Glug; glug.":L(4)=0:L(5)=0:L(3)=-1:GOTO305
5805 PRINT:PRINT"Lost? --- CONSULT MAP"
5810 PRINT"Not sure what you're carrying?---INVENTORY"
5815 PRINT"How many scoots are left?---CONSULT WATCH"
5820 PRINT"Want the room redescribed?---RELOOK":PRINT
5825 PRINT"What's a certain thing worth?---WORTH that thing"
5830 PRINT"Examples: WORTH COIN, WORTH CATALOG, WORTH CAT"
5835 PRINT:PRINT"Want a hint?---CONSULT a friend"
5840 PRINT"Examples:  CONSULT BANKER, CONSULT ";N$(29):PRINT
5845 PRINT"Not sure how to say something? --- LIST WORDS":PRINT
5850 PRINT"Not sure what the clues mean? --- ";
5855 PRINT"think & experiment":GOTO305
5980 PRINT"The ";N$;" is hard to ";V$;".":GOTO305
