2  PRINT CHR$(27);"x5";
5  PRINT CHR$(27);"H";:PRINT CHR$(27);"E";
10 GOSUB 9000:GOSUB 45
15 GOSUB 10000:GOSUB 45
18 GOSUB 11000:GOSUB 45
19 GOSUB 12000:GOSUB 45
20 GOSUB 13000:GOSUB 45
21 GOSUB 14000:GOSUB 45
22 GOSUB 13000:GOSUB 45
23 GOSUB 14000:GOSUB 45
24 GOSUB 13000:GOSUB 45
25 GOSUB 14000:GOSUB 45
26 GOSUB 13000:GOSUB 45
27 GOSUB 14000:GOSUB 45
28 GOSUB 15000:GOSUB 45
29 GOSUB 16000:GOSUB 45
30 GOSUB 18000:GOSUB 45
31 GOSUB 19000:GOSUB 45
32 GOSUB 20000:GOSUB 45
33 GOSUB 18000:GOSUB 45
34 GOSUB 19000:GOSUB 45
35 GOSUB 20000:GOSUB 45
36 GOSUB 17000:GOSUB 45
37 GOSUB 15000:GOSUB 45
38 GOSUB 16000:GOSUB 45
39 GOSUB 17000:GOSUB 45
40 PRINT CHR$(27);"E";
41 GOSUB 22000:FOR X=1 TO 500:NEXT X
42 PRINT CHR$(27);"E";
43 GOTO 10
45 FOR X=1 TO 70:NEXT X:RETURN
9000 REM WIDTH 255
9002 PRINT CHR$(27);"F";CHR$(27);"Y F{{{";CHR$(27);"Y!Ex  ";
9003 PRINT "x";CHR$(27);"Y";CHR$(34);";{{{{{{{{{x  x{{{{{{{{";
9004 PRINT CHR$(27);"Y#:x                     y";CHR$(27);"Y$5    x    ";
9005 PRINT "           ";CHR$(27);"Y$Qy";CHR$(27);"Y%8x";CHR$(27);"Y%H ";
9006 PRINT "  ";CHR$(27);"Y%Ry";CHR$(27);"Y&7x";CHR$(27);"Y&Sy";
9007 PRINT CHR$(27);"Y'6x";CHR$(27);"Y'K ";CHR$(27);"C ";CHR$(27);"Y'Ty";
9008 PRINT CHR$(27);"Y(5x";CHR$(27);"Y(<x";CHR$(27);"Cy";CHR$(27);"Y(Kx";
9009 PRINT " y";CHR$(27);"Y(Uy";CHR$(27);"Y)4x";CHR$(27);"Y);x";
9010 PRINT CHR$(27);"C  y  ";CHR$(27);"Y)Jx   y";CHR$(27);"Y)Vy";
9011 PRINT CHR$(27);"Y*3x";CHR$(27);"Y*:x";CHR$(27);"Y*";CHR$(64);"y";
9012 PRINT CHR$(27);"Y*Ix";CHR$(27);"Y*N y";CHR$(27);"Y*Wy";CHR$(27);"Y";
9013 PRINT "+3|";CHR$(27);"Y+:zzzzzzz";CHR$(27);"Y+Izzzzzzz";CHR$(27);
9014 PRINT "Y+X|";CHR$(27);"Y,3|";CHR$(27);"Y,B  x";CHR$(27);"Cy ";
9015 PRINT CHR$(27);"Y,X|";CHR$(27);"Y-3|";CHR$(27);"Y-Cx ";CHR$(27);"Y";
9016 PRINT "-Gy";CHR$(27);"Y-X|";CHR$(27);"Y.3|";CHR$(27);"Y.Czzzzz ";
9017 PRINT CHR$(27);"Y.X|";CHR$(27);"Y/3y";CHR$(27);"Y/<{{{{{{{{{{{{{{{";
9018 PRINT "{{{{{{";CHR$(27);"Y/Wx";CHR$(27);"Y04y       y p";
9019 PRINT CHR$(27);"Cp";CHR$(27);"Cp";CHR$(27);"Cp";CHR$(27);"Cp";
9020 PRINT CHR$(27);"Cp";CHR$(27);"Cp";CHR$(27);"Cp";CHR$(27);"Cp x";
9021 PRINT CHR$(27);"Y0Vx";CHR$(27);"Y15y";CHR$(27);"Y1=y";CHR$(27);"Y1";
9022 PRINT "Ox";CHR$(27);"Y1Ux";CHR$(27);"Y26y";CHR$(27);"Y2>y";
9023 PRINT CHR$(27);"Clmlm";CHR$(27);"Clm";CHR$(27);"Clm";CHR$(27);"Clm";
9024 PRINT " x";CHR$(27);"Y2Tx";CHR$(27);"Y37y";CHR$(27);"Y3?zzzzzzzzzz";
9025 PRINT "zzzzz";CHR$(27);"Y3Sx";CHR$(27);"Y48y";CHR$(27);"Y4Rx";
9026 PRINT CHR$(27);"Y59y               ";CHR$(27);"Y5Qx";CHR$(27);"Y6:";
9027 PRINT "zzzzzzzzzzzzzzzzzzzzzzz";
9028 RETURN
10000 REM WIDTH 255
10002 PRINT CHR$(27);"Y'H       ";CHR$(27);"Y(G   ";
10003 PRINT CHR$(27);"F{{{{{";CHR$(27);"Y*Izzzzzzz";CHR$(27);"Y+I       ";
10004 PRINT "   ";CHR$(27);"Y,H           ";
10005 RETURN
11000 REM WIDTH 255
11002 PRINT CHR$(27);"Y'H       ";CHR$(27);"Y(G         ";
11003 PRINT CHR$(27);"Y)I ";CHR$(27);"Faaaaa";CHR$(27);"Y*H        ";
11004 PRINT CHR$(27);"Y+G          ";CHR$(27);"Y,H           ";
11006 RETURN
12000 REM WIDTH 255
12002 PRINT CHR$(27);"Y'H       ";CHR$(27);"Y(G    ";
12003 PRINT CHR$(27);"Fx y  ";CHR$(27);"Y)I x   y";CHR$(27);"Y*H x     y";
12004 PRINT CHR$(27);"Y+G  zzzzzzz ";CHR$(27);"Y,H           ";
12006 RETURN
13000 REM WIDTH 255
13005 PRINT CHR$(27);"Y1=y ";CHR$(27);"Clml";
13006 PRINT "m";CHR$(27);"Clm";CHR$(27);"Clm";CHR$(27);"Clm  x ";
13007 PRINT CHR$(27);"Y2= zzzzzzzzzzzzzzzzz  ";CHR$(27);"Y39               ";
13008 PRINT CHR$(27);"Y3=                      ";
13009 RETURN
14000 REM WIDTH 255
14001 PRINT CHR$(27);"Y1>                 ":PRINT CHR$(27);"Y2>               ";
14002 PRINT CHR$(27);"F";CHR$(27);"Y/<{{{{{{{{{{{{{{{{{{{{{";
14003 PRINT CHR$(27);"Y06      y p";CHR$(27);"Cp";CHR$(27);"Cp";
14004 PRINT CHR$(27);"Cp";CHR$(27);"Cp";CHR$(27);"Cp";CHR$(27);"Cp";
14005 PRINT CHR$(27);"Cp";CHR$(27);"Cp x";CHR$(27);"Y1=y";CHR$(27);"Y1Ox";
14006 PRINT CHR$(27);"Y2>y";CHR$(27);"Clmlm";CHR$(27);"Clm";CHR$(27);"Cl";
14007 PRINT "m";CHR$(27);"Clm x";CHR$(27);"Y3?zzzzzzzzzzzzzzz";
14008 PRINT CHR$(27);"Y49               ";
14009 RETURN
15000 REM WIDTH 255
15002 PRINT CHR$(27);"Y'9       ";CHR$(27);"Y(8         ";
15003 PRINT CHR$(27);"Y): ";CHR$(27);"Faaaaa";CHR$(27);"Y*9        ";
15004 PRINT CHR$(27);"Y+8          ";CHR$(27);"Y,9           ";
15006 RETURN
16000 REM WIDTH 255
16002 PRINT CHR$(27);"Y'9       ";CHR$(27);"Y(8         ";
16003 PRINT CHR$(27);"Y): ";CHR$(27);"Faaaaa";CHR$(27);"Y*9        ";
16004 PRINT CHR$(27);"Y+8          ";CHR$(27);"Y,9           ";
16006 RETURN
17000 REM WIDTH 255
17002 PRINT CHR$(27);"Y'9       ";CHR$(27);"Y(8    ";
17003 PRINT CHR$(27);"Fx y  ";CHR$(27);"Y): x   y";CHR$(27);"Y*9 x     y";
17004 PRINT CHR$(27);"Y+8  zzzzzzz ";CHR$(27);"Y,9           ";
17006 RETURN
18000 REM WIDTH 255
18002 PRINT CHR$(27);"Y B{{{{      ";CHR$(27);
18003 PRINT "Y!By   y    ";CHR$(27);"Y";CHR$(34);"Cy   y ";
18005 RETURN
19000 REM WIDTH 255
19002 PRINT CHR$(27);"Y A   ";CHR$(27);"F{{{{  ";CHR$(27);
19003 PRINT "Y!B }   } ";CHR$(27);"Y";CHR$(34);"C}   } ";
19005 RETURN
20000 REM WIDTH 255
20002 PRINT CHR$(27);"q";CHR$(27);"Y A     ";CHR$(27);"F{{{{ ";
20003 PRINT CHR$(27);"Y!B   x   x";CHR$(27);"Y";CHR$(34);"C x   x";
20005 RETURN
22000 REM WIDTH 255
22002 PRINT CHR$(27);"Y";CHR$(34);" {{{{{{{{{{";
22003 PRINT "{{{{{{{";CHR$(27);"Y";CHR$(34);"={{{{{{{{{{{{{{{{";
22004 PRINT CHR$(27);"Y";CHR$(34);"[{{{{{{{{{{{{{{{{";CHR$(27);"Y# `";
22005 PRINT CHR$(27);"Y#0 y";CHR$(27);"Y#<x";CHR$(27);"Y#My";CHR$(27);"Y";
22006 PRINT "#Zx";CHR$(27);"Y#ky";CHR$(27);"Y$ `";CHR$(27);"Y$%{{{{{{{{{";
22007 PRINT CHR$(27);"Y$0 ";CHR$(27);"Cy";CHR$(27);"Y$;x";CHR$(27);"Y$A{";
22008 PRINT "{{{{{{{{";CHR$(27);"Y$Ny";CHR$(27);"Y$Yx";CHR$(27);"Y$ly";
22009 PRINT CHR$(27);"Y% `";CHR$(27);"Y%$`";CHR$(27);"Y%- y";CHR$(27);"Y";
22010 PRINT "%2 y";CHR$(27);"Y%:x";CHR$(27);"Y%";CHR$(64);"x";CHR$(27);
22011 PRINT "Y%Jy";CHR$(27);"Y%Oy";CHR$(27);"Y%Xx";CHR$(27);"Y%]xzzzzzzzz";
22012 PRINT "zy";CHR$(27);"Y%my";CHR$(27);"Y& `";CHR$(27);"Y&$`";
22013 PRINT CHR$(27);"Y&/`";CHR$(27);"Y&3`";CHR$(27);"Y&:`";CHR$(27);"Y&";
22014 PRINT "> x";CHR$(27);"Y&Ky";CHR$(27);"Y&O`";CHR$(27);"Y&X`";
22015 PRINT CHR$(27);"Y&\x";CHR$(27);"Y&hy";CHR$(27);"Y&m`";CHR$(27);"Y'";
22016 PRINT " `";CHR$(27);"Y'$`";CHR$(27);"Y'/`";CHR$(27);"Y'3`";
22017 PRINT CHR$(27);"Y':`";CHR$(27);"Y'>`";CHR$(27);"Y'K`";CHR$(27);"Y'";
22018 PRINT "O`";CHR$(27);"Y'X`";CHR$(27);"Y'\`";CHR$(27);"Y'h`";
22019 PRINT CHR$(27);"Y'm`";CHR$(27);"Y( `";CHR$(27);"Y($`{{{{{{{{{x";
22020 PRINT CHR$(27);"Y(3x";CHR$(27);"Y(:`";CHR$(27);"Y(>`";CHR$(27);"Y(";
22021 PRINT "K`";CHR$(27);"Y(O`";CHR$(27);"Y(X`";CHR$(27);"Y(\`";
22022 PRINT CHR$(27);"Y(h`";CHR$(27);"Y(m`";CHR$(27);"Y) `";CHR$(27);"Y)";
22023 PRINT "2x";CHR$(27);"Y):`";CHR$(27);"Y)>`";CHR$(27);"Y)K`";
22024 PRINT CHR$(27);"Y)O`";CHR$(27);"Y)X`";CHR$(27);"Y)\`";CHR$(27);"Y)";
22025 PRINT "h`";CHR$(27);"Y)m`";CHR$(27);"Y* `";CHR$(27);"Y*%{{{{{{{{{";
22026 PRINT CHR$(27);"Y*2y";CHR$(27);"Y*:`";CHR$(27);"Y*>`";CHR$(27);"Y*";
22027 PRINT "K`";CHR$(27);"Y*O`";CHR$(27);"Y*X`";CHR$(27);"Y*\`";
22028 PRINT CHR$(27);"Y*h`";CHR$(27);"Y*m`";CHR$(27);"Y+ `";CHR$(27);"Y+";
22029 PRINT "%`";CHR$(27);"Y+.y";CHR$(27);"Y+3y";CHR$(27);"Y+:`";
22030 PRINT CHR$(27);"Y+>`";CHR$(27);"Y+K`";CHR$(27);"Y+O`";CHR$(27);"Y+";
22031 PRINT "X`";CHR$(27);"Y+\`";CHR$(27);"Y+h`";CHR$(27);"Y+m`";
22032 PRINT CHR$(27);"Y, `";CHR$(27);"Y,%`";CHR$(27);"Y,.`";CHR$(27);"Y,";
22033 PRINT "3`";CHR$(27);"Y,:`";CHR$(27);"Y,>`";CHR$(27);"Y,K`";
22034 PRINT CHR$(27);"Y,O`";CHR$(27);"Y,X`";CHR$(27);"Y,\`";CHR$(27);"Y,";
22035 PRINT "h`";CHR$(27);"Y,m`";CHR$(27);"Y- `";CHR$(27);"Y-%`";
22036 PRINT CHR$(27);"Y-.`";CHR$(27);"Y-2 `";CHR$(27);"Y-:`";CHR$(27);"Y";
22037 PRINT "-?y";CHR$(27);"Y-Jx";CHR$(27);"Y-O`";CHR$(27);"Y-X`";
22038 PRINT CHR$(27);"Y-]y";CHR$(27);"Y-gx";CHR$(27);"Y-l `";CHR$(27);"Y";
22039 PRINT ". `";CHR$(27);"Y.%`{{{{{{{x";CHR$(27);"Y.3`";CHR$(27);"Y.: ";
22040 PRINT "y";CHR$(27);"Y.? y{{{{{{{{x";CHR$(27);"Y.Nx";CHR$(27);"Y.Yy";
22041 PRINT CHR$(27);"Y.";CHR$(94);"y{{{{{{{x";CHR$(27);"Y.i   x";
22042 PRINT CHR$(27);"Y/ `";CHR$(27);"Y/2x";CHR$(27);"Y/<y";CHR$(27);"Y/";
22043 PRINT "Mx";CHR$(27);"Y/Zy";CHR$(27);"Y/kx";CHR$(27);"Y0 `";
22044 PRINT CHR$(27);"Y00 x";CHR$(27);"Y0=y";CHR$(27);"Y0Lx";CHR$(27);"Y";
22045 PRINT "0[y{{{{{{{{{{{{{{x";CHR$(27);"Y1 zzzzzzzzzzzzzzzzz";
22046 PRINT CHR$(27);"Y1>zzzzzzzzzzzzzz";
22047 RETURN
