/*
 *
 * The Abandoned Farm House Adventure
 *
 * Jeff Tranter <tranter@pobox.com>
 *
 * Written in standard C but designed to run on the Apple Replica 1
 * or Apple II using the CC65 6502 assembler.
 *
 * Copyright 2012-2024 Jeff Tranter
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Revision History:
 *
 * Version  Date         Comments
 * -------  ----         --------
 * 0.0      13 Mar 2012  First alpha version
 * 0.1      18 Mar 2012  First beta version
 * 0.9      19 Mar 2012  First public release
 * 1.0      06 Sep 2015  Lower case and other Apple II improvements.
 * 1.1      26 Jul 2022  Added backup/restore commands.
 * 2.0      11 Aug 2024  Align all three games with common code.
 *
 */

/* Uncomment the next line to define JOYSTICK if you want to enable
 *  support for moving using a joystick. You need to be on a platform
 *  with joystick support in cc65.
 */
//#define JOYSTICK 1

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if defined(__CC65__) || defined(CPM)
#include <conio.h>
#ifdef JOYSTICK
#include <joystick.h>
#endif /* JOYSTICK */
#endif /* __CC65__ */

/* Define FILEIO if you want backup and restore commands to use files.
 * Otherwise uses memory. Requires platform support for file i/o
 * (known to work on Apple 2 and Commodore 64 with cc65 as well as
 * Linux.
 */

#if defined(__linux__) || defined(__APPLE2ENH__) || defined(__C64__) || defined(CPM) || defined(HDOS)
#define FILEIO 1
#endif

/* CONSTANTS */

/* Maximum number of items user can carry */
#define MAXITEMS 5

/* Number of locations */
#define NUMLOCATIONS 32

/* Number of (memory-resident) saved games */
#define SAVEGAMES 10

/* TYPES */

/* To optimize for code size and speed, most numbers are 8-bit chars when compiling for CC65. */
#ifdef __CC65__
typedef char number;
#else
typedef int number;
#endif /* __CC65__ */

/* Directions */
typedef enum {
    North,
    South,
    East,
    West,
    Up,
    Down
} Direction_t;

/* Items */
typedef enum {
    NoItem,
    Key,
    Pitchfork,
    Flashlight,
    Lamp,
    Oil,
    Candybar,
    Bottle,
    Doll,
    ToyCar,
    Matches,
    GoldCoin,
    SilverCoin,
    StaleMeat,
    Book,
    Cheese,
    OldRadio,
    LastItem=OldRadio
} Item_t;

/* Locations */
typedef enum {
    NoLocation,
    Driveway1,
    Driveway2,
    Driveway3,
    Driveway4,
    Driveway5,
    Garage,
    WorkRoom,
    Hayloft,
    Kitchen,
    DiningRoom,
    BottomStairs,
    DrawingRoom,
    Study,
    TopStairs,
    BoysBedroom,
    GirlsBedroom,
    MasterBedroom,
    ServantsQuarters,
    LaundryRoom,
    FurnaceRoom,
    VacantRoom,
    Cistern,
    Tunnel,
    Woods24,
    Woods25,
    Woods26,
    WolfTree,
    Woods28,
    Woods29,
    Woods30,
    Woods31
} Location_t;

/* Structure to hold entire game state */
typedef struct {
    number valid;
    Item_t Inventory[MAXITEMS];
    Location_t locationOfItem[LastItem+1];
    Direction_t Move[NUMLOCATIONS][6];
    number currentLocation;
    int turnsPlayed;
    number lampLit;
    number lampFilled;
    number ateFood;
    number drankWater;
    number ratAttack;
    number wolfState;
} GameState_t;

/* TABLES */

/* Names of directions */
const char *DescriptionOfDirection[] = {
    "north", "south", "east", "west", "up", "down"
};

/* Names of items */
const char *DescriptionOfItem[LastItem+1] = {
    "",
    "key",
    "pitchfork",
    "flashlight",
    "lamp",
    "oil",
    "candybar",
    "bottle",
    "doll",
    "toy car",
    "matches",
    "gold coin",
    "silver coin",
    "stale meat",
    "book",
    "cheese",
    "old radio",
};

/* Names of locations */
const char *DescriptionOfLocation[NUMLOCATIONS] = {
    "",
    "in the driveway near your car",
    "in the driveway",
    "in front of the garage",
    "in front of the barn",
    "at the door to the house",
    "in the garage",
    "in the workroom of the barn",
    "in the hayloft of the barn",
    "in the kitchen",
    "in the dining room",
    "at the bottom of the stairs",
    "in the drawing room",
    "in the study",
    "at the top of the stairs",
    "in a boy's bedroom",
    "in a girl's bedroom",
    "in the master bedroom next to a bookcase",
    "in the servant's quarters",
    "in the basement laundry room",
    "in the furnace room",
    "in a vacant room next to a locked door",
    "in the cistern",
    "in an underground tunnel. There are rats here",
    "in the woods near a trapdoor",
    "in the woods",
    "in the woods",
    "in the woods next to a tree",
    "in the woods",
    "in the woods",
    "in the woods",
    "in the woods",
};

/* DATA */

/* Inventory of what player is carrying */
Item_t Inventory[MAXITEMS];

/* Location of each item. Index is the item number, returns the location. 0 if item is gone */
Location_t locationOfItem[LastItem+1];

/* Map. Given a location and a direction to move, returns the location it connects to, or 0 if not a valid move. Map can change during game play. */
Direction_t Move[NUMLOCATIONS][6] = {
    /* N  S  E  W  U  D */
    {  0, 0, 0, 0, 0, 0 }, /* 0 */
    {  2, 0, 0, 0, 0, 0 }, /* 1 */
    {  4, 1, 3, 5, 0, 0 }, /* 2 */
    {  0, 0, 6, 2, 0, 0 }, /* 3 */
    {  7, 2, 0, 0, 0, 0 }, /* 4 */
    {  0, 0, 2, 9, 0, 0 }, /* 5 */
    {  0, 0, 0, 3, 0, 0 }, /* 6 */
    {  0, 4, 0, 0, 8, 0 }, /* 7 */
    {  0, 0, 0, 0, 0, 7 }, /* 8 */
    {  0,10, 5, 0, 0,19 }, /* 9 */
    {  9, 0, 0,11, 0, 0 }, /* 10 */
    {  0, 0,10,12,14, 0 }, /* 11 */
    { 13, 0,11, 0, 0, 0 }, /* 12 */
    {  0,12, 0, 0, 0, 0 }, /* 13 */
    { 16, 0,15,17, 0,11 }, /* 14 */
    {  0, 0, 0,14, 0, 0 }, /* 15 */
    {  0,14, 0, 0, 0, 0 }, /* 16 */
    {  0, 0,14, 0, 0, 0 }, /* 17 */
    {  0, 0, 0, 0, 0,13 }, /* 18 */
    {  0, 0, 0,20, 9, 0 }, /* 19 */
    { 21, 0,19, 0, 0, 0 }, /* 20 */
    {  0,20, 0,22, 0, 0 }, /* 21 */
    {  0, 0,21, 0, 0, 0 }, /* 22 */
    { 24,21, 0, 0, 0, 0 }, /* 23 */
    { 29,23, 0,26, 0, 0 }, /* 24 */
    { 26, 0,24, 0, 0, 0 }, /* 25 */
    { 27,25,29, 0, 0, 0 }, /* 26 */
    {  0,26,28, 0, 0, 0 }, /* 27 */
    {  0,29,31,27, 0, 0 }, /* 28 */
    { 28,24,30,26, 0, 0 }, /* 29 */
    { 31, 0, 0,29, 0, 0 }, /* 30 */
    {  0,30, 0,29, 0, 0 }, /* 31 */
};

/* Current location */
number currentLocation;

/* Number of turns played in game */
int turnsPlayed;

/* True if player has lit the lamp. */
number lampLit;

/* True if lamp filled with oil. */
number lampFilled;

/* True if player ate food. */
number ateFood;

/* True if player drank water. */
number drankWater;

/* Incremented each turn you are in the tunnel. */
number ratAttack;

/* Tracks state of wolf attack */
number wolfState;

/* Set when game is over */
number gameOver;

#ifndef FILEIO
/* Memory-resident saved games */
GameState_t savedGame[SAVEGAMES];
#endif

const char *introText = "                          Abandoned Farmhouse Adventure\n                                 By Jeff Tranter\n\nYour three-year-old grandson has gone missing and was last seen headed in the\ndirection of the abandoned family farm. It's a dangerous place to play.\nYou have to find him before he gets hurt, and it will be getting dark soon...\n";

#ifdef FILEIO
const char *helpString = "Valid commands:\ngo east/west/north/south/up/down\nlook\nuse <object>\nexamine <object>\ntake <object>\ndrop <object>\ninventory\nbackup <file>\nrestore <file>\nhelp\nquit\nYou can abbreviate commands and directions to the first letter.\nType just the first letter of a direction to move.\n";
#else
const char *helpString = "Valid commands:\ngo east/west/north/south/up/down\nlook\nuse <object>\nexamine <object>\ntake <object>\ndrop <object>\ninventory\nbackup <number>\nrestore <number>\nhelp\nquit\nYou can abbreviate commands and directions to the first letter.\nType just the first letter of a direction to move.\n";
#endif

/* Line of user input */
char buffer[80];

#if defined(__OSIC1P__)

/* Have to implement fgets() ourselves as it is not available. */
char* _fgets(char* buf, size_t size, FILE*)
{
    int c;
    char *p;

    /* get max bytes or upto a newline */
    for (p = buf, size--; size > 0; size--) {
        if ((c = cgetc()) == EOF)
            break;
        cputc(c); /* echo back */
        *p++ = c;
        if (c == '\n' || c == '\r')
            break;
    }
    *p = 0;
    if (p == buf || c == EOF)
        return NULL;
    return (p);
}

#define fgets _fgets
#define printf cprintf
#endif

/*
 * Check if string str starts with command or abbreviated command cmd, e.g
 * "h", "he", "hel", or "help" matches "help". Not case sensitive. Ends
 * comparison when str contains space, end of string, or end of cmd reached.
 * Return 1 for match, 0 for non-match.
 */
number matchesCommand(const char *cmd, const char *str)
{
    unsigned int i;

    /* Make sure that at least the first character matches. */
    if (cmd[0] == '\0' || str[0] == '\0' || cmd[0] == ' ' || str[0] == ' ' || tolower(str[0]) != tolower(cmd[0])) {
        return 0; /* no match */
    }

    /* Now check rest of strings. */
    for (i = 1; i < strlen(cmd); i++) {
        if (cmd[i] == '\0' || str[i] == '\0' || cmd[i] == ' ' || str[i] == ' ') {
            return 1; /* A match */
        }
        if (tolower(str[i]) != tolower(cmd[i])) {
            return 0; /* Not a match */
        }
    }

    return 1; /* A match */
}

/* Clear the screen */
void clearScreen()
{
#if defined(__APPLE2__)
    clrscr();
#elif defined(__linux__)
    number i;
    for (i = 0; i < 24; ++i)
        printf("\n");
#else
    /* Heathkit H89/H19 screen clear */
    printf("\eE");
#endif
}

/* Return 1 if carrying an item */
number carryingItem(const char *item)
{
    number i;

    for (i = 0; i < MAXITEMS; i++) {
        if ((Inventory[i] != 0) && (!strcasecmp(DescriptionOfItem[Inventory[i]], item)))
            return 1;
    }
    return 0;
}

/* Return 1 if item it at current location (not carried) */
number itemIsHere(const char *item)
{
    number i;

    /* Find number of the item. */
    for (i = 1; i <= LastItem; i++) {
        if (!strcasecmp(item, DescriptionOfItem[i])) {
            /* Found it, but is it here? */
            if (locationOfItem[i] == currentLocation) {
                return 1;
            } else {
                return 0;
            }
        }
    }
    return 0;
}

/* Check for an abbreviated item name. Return full name of item if it
   uniquely matches. Otherwise returns the original name. Only check
   for items being carried or at current location. */
char *getMatch(char *name)
{
    int matches = 0;
    int index = 0;
    int i;

    for (i = 1; i <= LastItem; i++) {
        if (carryingItem(DescriptionOfItem[i]) || itemIsHere(DescriptionOfItem[i])) {
            if (!strncasecmp(DescriptionOfItem[i], name, strlen(name))) {
                index = i;
                matches++;
            }
        }
    }

    if (matches == 1) {
        strcpy(name, DescriptionOfItem[index]);
    }
    return name;
}

/* Inventory command */
void doInventory()
{
    number i;
    int found = 0;

    printf("%s", "You are carrying:\n");
    for (i = 0; i < MAXITEMS; i++) {
        if (Inventory[i] != 0) {
            printf("  %s\n", DescriptionOfItem[Inventory[i]]);
            found = 1;
        }
    }
    if (!found)
        printf("  nothing\n");
}

/* Help command */
void doHelp()
{
    printf("%s", helpString);
}

/* Look command */
void doLook()
{
    number i, loc, seen;

    printf("You are %s.\n", DescriptionOfLocation[currentLocation]);

    seen = 0;
    printf("You see:\n");
    for (i = 1; i <= LastItem; i++) {
        if (locationOfItem[i] == currentLocation) {
            printf("  %s\n", DescriptionOfItem[i]);
            seen = 1;
        }
    }
    if (!seen)
        printf("  nothing special\n");

    printf("You can go:");

    for (i = North; i <= Down; i++) {
        loc = Move[currentLocation][i];
        if (loc != 0) {
            printf(" %s", DescriptionOfDirection[i]);
        }
    }
    printf("\n");
}

/* Quit command */
void doQuit()
{
    printf("%s", "Are you sure you want to quit (y/n)? ");
    fflush(stdout);
    fgets(buffer, sizeof(buffer)-1, stdin);
    if (tolower(buffer[0]) == 'y') {
        gameOver = 1;
    }
}

/* Drop command */
void doDrop()
{
    number i;
    char *sp;
    char *item;

    /* Command line should be like "D[ROP] ITEM" Item name will be after first space. */
    sp = strchr(buffer, ' ');
    if (sp == NULL) {
        printf("Drop what?\n");
        return;
    }

    item = sp + 1;

    item = getMatch(item);

    /* See if we have this item */
    for (i = 0; i < MAXITEMS; i++) {
        if ((Inventory[i] != 0) && (!strcasecmp(DescriptionOfItem[Inventory[i]], item))) {
            /* We have it. Add to location. */
            locationOfItem[Inventory[i]] = currentLocation;
            /* And remove from inventory */
            Inventory[i] = 0;
            printf("Dropped %s.\n", item);
            ++turnsPlayed;
            return;
        }
    }
    /* If here, don't have it. */
    printf("Not carrying %s.\n", item);
}

/* Take command */
void doTake()
{
    number i, j;
    char *sp;
    char *item;

    /* Command line should be like "T[AKE] ITEM" Item name will be after first space. */
    sp = strchr(buffer, ' ');
    if (sp == NULL) {
        printf("Take what?\n");
        return;
    }

    item = sp + 1;

    item = getMatch(item);

    if (carryingItem(item)) {
        printf("Already carrying it.\n");
        return;
    }

    /* Find number of the item. */
    for (i = 1; i <= LastItem; i++) {
        if (!strcasecmp(item, DescriptionOfItem[i])) {
            /* Found it, but is it here? */
            if (locationOfItem[i] == currentLocation) {
            /* It is here. Add to inventory. */
            for (j = 0; j < MAXITEMS; j++) {
                if (Inventory[j] == 0) {
                    Inventory[j] = i;
                    /* And remove from location. */
                    locationOfItem[i] = 0;
                    printf("Took %s.\n", item);
                    ++turnsPlayed;
                    return;
                }
            }

            /* Reached maximum number of items to carry */
            printf("You can't carry any more. Drop something.\n");
            return;
            }
        }
    }

    /* If here, don't see it. */
    printf("I see no %s here.\n", item);
}

/* Go command */
void doGo()
{
    char *sp;
    char dirChar;
    Direction_t dir;

    /* Command line should be like "G[O] N[ORTH]" Direction will be
       the first letter after a space. Or just a single letter
       direction N S E W U D or full direction NORTH etc. */

    sp = strrchr(buffer, ' ');
    if (sp != NULL) {
        dirChar = *(sp+1);
    } else {
        dirChar = buffer[0];
    }
    dirChar = tolower(dirChar);

    if (dirChar == 'n') {
        dir = North;
    } else if (dirChar == 's') {
        dir = South;
    } else if (dirChar == 'e') {
        dir = East;
    } else if (dirChar == 'w') {
        dir = West;
    } else if (dirChar == 'u') {
        dir = Up;
    } else if (dirChar == 'd') {
        dir = Down;
    } else {
        printf("Go where?\n");
        return;
    }

    if (Move[currentLocation][dir] == 0) {
        printf("You can't go %s from here.\n", DescriptionOfDirection[dir]);
        return;
    }

    /* We can move */
    currentLocation = Move[currentLocation][dir];
    printf("You are %s.\n", DescriptionOfLocation[currentLocation]);
    ++turnsPlayed;
}

/* Examine command */
void doExamine()
{
    char *sp;
    char *item;

    /* Command line should be like "E[XAMINE] ITEM" Item name will be after first space. */
    sp = strchr(buffer, ' ');
    if (sp == NULL) {
        printf("Examine what?\n");
        return;
    }

    item = sp + 1;

    item = getMatch(item);

    ++turnsPlayed;

    /* Examine bookcase - not an object */
    if (!strcasecmp(item, "bookcase")) {
        printf("You pull back a book and the bookcase opens up to reveal a secret room.\n");
        Move[17][North] = 18;
        return;
    }

    /* Make sure item is being carried or is in the current location */
    if (!carryingItem(item) && !itemIsHere(item)) {
        printf("I don't see it here.\n");
        return;
    }

    /* Examine Book */
    if (!strcasecmp(item, "book")) {
        printf("It is a very old book entitled \"Apple 1 operation manual\".\n");
        return;
    }

    /* Examine Flashlight */
    if (!strcasecmp(item, "flashlight")) {
        printf("It doesn't have any batteries.\n");
        return;
    }

    /* Examine toy car */
    if (!strcasecmp(item, "toy car")) {
        printf("It is a nice toy car. Your grandson Matthew would like it.\n");
        return;
    }

    /* Examine old radio */
    if (!strcasecmp(item, "old radio")) {
        printf("It is a 1940 Zenith 8-S-563 console with an 8A02 chassis. You'd turn it on\nbut the electricity is off.\n");
        return;
    }

   /* Nothing special about this item */
   printf("You see nothing special about it.\n");
}

/* Use command */
void doUse()
{
    char *sp;
    char *item;

    /* Command line should be like "U[SE] ITEM" Item name will be after first space. */
    sp = strchr(buffer, ' ');
    if (sp == NULL) {
        printf("Use what?\n");
        return;
    }

    item = sp + 1;

    item = getMatch(item);

    /* Make sure item is being carried or is in the current location */
    if (!carryingItem(item) && !itemIsHere(item)) {
        printf("I don't see it here.\n");
        return;
    }

    ++turnsPlayed;

    /* Use key */
    if (!strcasecmp(item, "key") && (currentLocation == VacantRoom)) {
        printf("You insert the key in the door and it opens, revealing a tunnel.\n");
        Move[21][North] = 23;
        return;
    }

    /* Use pitchfork */
    if (!strcasecmp(item, "pitchfork") && (currentLocation == WolfTree) && (wolfState == 0)) {
        printf("You jab the wolf with the pitchfork. It howls and runs away.\n");
        wolfState = 1;
        return;
    }

    /* Use toy car */
    if (!strcasecmp(item, "toy car") && (currentLocation == WolfTree && wolfState == 1)) {
        printf("You show Matthew the toy car and he comes down to take it.\nYou take Matthew in your arms and carry him home.\n");
        wolfState = 2;
        return;
    }

    /* Use oil */
    if (!strcasecmp(item, "oil")) {
        if (carryingItem("lamp")) {
            printf("You fill the lamp with oil.\n");
            lampFilled = 1;
            return;
        } else {
            printf("You don't have anything to use it with.\n");
            return;
        }
    }

    /* Use matches */
    if (!strcasecmp(item, "matches")) {
        if (carryingItem("lamp")) {
            if (lampFilled) {
                printf("You light the lamp. You can see!\n");
                lampLit = 1;
                return;
            } else {
                printf("You can't light the lamp. It needs oil.\n");
                return;
            }
        } else {
            printf("Nothing here to light\n");
        }
    }

    /* Use candybar */
    if (!strcasecmp(item, "candybar")) {
        printf("That hit the spot. You no longer feel hungry.\n");
        ateFood = 1;
        return;
    }

    /* Use bottle */
    if (!strcasecmp(item, "bottle")) {
        if (currentLocation == Cistern) {
            printf("You fill the bottle with water from the cistern and take a drink.\nYou no longer feel thirsty.\n");
            drankWater = 1;
            return;
        } else {
            printf("The bottle is empty. If only you had some water to fill it!\n");
            return;
        }
    }

    /* Use stale meat */
    if (!strcasecmp(item, "stale meat")) {
        printf("The meat looked and tasted bad. You feel very sick and pass out.\n");
        gameOver = 1;
        return;
    }

    /* Default */
    printf("Nothing happens\n");
}

#ifdef FILEIO
/* Backup command - file version */
void doBackup()
{
    char *sp;
    char *name;
    number i, j;
    FILE *fp;

    /* Command line should be like "B[ACKUP] NAME" */
    /* Save file name will be after first space. */
    sp = strchr(buffer, ' ');
    if (sp == NULL) {
        printf("Backup under what name?\n");
        return;
    }

    name = sp + 1;

    printf("Backing up game state under name '%s'.\n", name);

    fp = fopen(name, "w");
    if (fp == NULL) {
        printf("Unable to open file '%s'.\n", name);
        return;
    }

    fprintf(fp, "%s\n", "#Adventure1 Save File");

    fprintf(fp, "Inventory:");
    for (i = 0; i < MAXITEMS; i++) {
        fprintf(fp, " %d", Inventory[i]);
    }
    fprintf(fp, "\n");

    fprintf(fp, "Items:");
    for (i = 0; i <= LastItem; i++) {
        fprintf(fp, " %d", locationOfItem[i]);
    }
    fprintf(fp, "\n");

    fprintf(fp, "Map:\n");
    for (i = 0; i < NUMLOCATIONS; i++) {
        for (j = 0; j < 6; j++) {
            fprintf(fp, " %d", Move[i][j]);
        }
        fprintf(fp, "\n");
    }

    fprintf(fp, "Variables: %d %d %d %d %d %d %d %d\n",
           currentLocation,
           turnsPlayed,
           lampLit,
           lampFilled,
           ateFood,
           drankWater,
           ratAttack,
           wolfState);

    i = fclose(fp);
    if (i != 0) {
        printf("Unable to close file, error code %d.\n", i);
    }
}
#else
/* Backup command - memory-resident version */
void doBackup()
{
    char *sp;
    number i, j, n;

    /* Command line should be like "B[ACKUP] <NUMBER>" */
    /* Number will be after first space. */
    sp = strchr(buffer, ' ');
    if (sp == NULL) {
        printf("Backup under what number?\n");
        return;
    }

    n = strtol(sp + 1, NULL, 10);
    if  (n <= 0 || n > SAVEGAMES) {
        printf("Invalid backup number. Specify %d through %d.\n", 1, SAVEGAMES);
        return;
    }

    printf("Backing up game state under number %d.\n", n);

    savedGame[n-1].valid = 1;
    for (i = 0; i < MAXITEMS; i++) {
        savedGame[n-1].Inventory[i] = Inventory[i];
    }
    for (i = 0; i < LastItem+1; i++) {
        savedGame[n-1].locationOfItem[i] = locationOfItem[i];
    }
    for (i = 0; i < NUMLOCATIONS; i++) {
        for (j = 0; j < 6; j++) {
            savedGame[n-1].Move[i][j] = Move[i][j];
        }
    }
    savedGame[n-1].currentLocation = currentLocation;
    savedGame[n-1].turnsPlayed = turnsPlayed;
    savedGame[n-1].lampLit = lampLit;
    savedGame[n-1].lampFilled = lampFilled;
    savedGame[n-1].ateFood = ateFood;
    savedGame[n-1].drankWater = drankWater;
    savedGame[n-1].ratAttack = ratAttack;
    savedGame[n-1].wolfState = wolfState;
}

#endif /* FILEIO */

#ifdef FILEIO
/* Restore command - file version */
void doRestore()
{
    char *sp;
    char *name;
    number i, j;
    FILE *fp;

    /* Command line should be like "R[ESTORE] NAME" */
    /* Save file name will be after first space. */
    sp = strchr(buffer, ' ');
    if (sp == NULL) {
        printf("Restore from what file?\n");
        return;
    }

    name = sp + 1;

    printf("Restoring game state from file '%s'.\n", name);

    fp = fopen(name, "r");
    if (fp == NULL) {
        printf("Unable to open file '%s'.\n", name);
        return;
    }

    /* Check for header line */
    fgets(buffer, sizeof(buffer) - 1, fp);
    if (strcmp(buffer, "#Adventure1 Save File\n")) {
        printf("File is not a valid game file (1).\n");
        fclose(fp);
        return;
    }

    /* Inventory: 3 0 0 0 0 */
    i = fscanf(fp, "Inventory: %d %d %d %d %d\n",
           (int*) &Inventory[0],
           (int*) &Inventory[1],
           (int*) &Inventory[2],
           (int*) &Inventory[3],
           (int*) &Inventory[4]);
    if (i != 5) {
        printf("File is not a valid game file (2).\n");
        fclose(fp);
        return;
    }

    /* Items: 0 1 8 0 7 6 9 2 16 15 18 25 29 10 12 19 17 */
    i = fscanf(fp, "Items: %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n",
           (int*) &locationOfItem[0],
           (int*) &locationOfItem[1],
           (int*) &locationOfItem[2],
           (int*) &locationOfItem[3],
           (int*) &locationOfItem[4],
           (int*) &locationOfItem[5],
           (int*) &locationOfItem[6],
           (int*) &locationOfItem[7],
           (int*) &locationOfItem[8],
           (int*) &locationOfItem[9],
           (int*) &locationOfItem[10],
           (int*) &locationOfItem[11],
           (int*) &locationOfItem[12],
           (int*) &locationOfItem[13],
           (int*) &locationOfItem[14],
           (int*) &locationOfItem[15],
           (int*) &locationOfItem[16]);

    if (i != 17) {
        printf("File is not a valid game file (3).\n");
        fclose(fp);
        return;
    }

    fscanf(fp, "Map:\n");

    for (i = 0; i < NUMLOCATIONS; i++) {
        j = fscanf(fp, " %d %d %d %d %d %d\n",
               (int*) &Move[i][0],
               (int*) &Move[i][1],
               (int*) &Move[i][2],
               (int*) &Move[i][3],
               (int*) &Move[i][4],
               (int*) &Move[i][5]);
        if (j != 6) {
            printf("File is not a valid game file (4).\n");
            fclose(fp);
            return;
        }
    }

    /* Variables: 1 0 0 0 0 0 0 0 */
    i = fscanf(fp, "Variables: %d %d %d %d %d %d %d %d\n",
           &currentLocation,
           &turnsPlayed,
           &lampLit,
           &lampFilled,
           &ateFood,
           &drankWater,
           &ratAttack,
           &wolfState);

    if (i != 8) {
        printf("File is not a valid game file (5).\n");
        fclose(fp);
        return;
    }

    i = fclose(fp);
    if (i != 0) {
        printf("Unable to close file, error code %d.\n", i);
    }
}
#else
/* Restore command - memory-resident version */
void doRestore()
{
    char *sp;
    number i, j, n;

    /* Command line should be like "R[ESTORE] <NUMBER>" */
    /* Number will be after first space. */
    sp = strchr(buffer, ' ');
    if (sp == NULL) {
        printf("Restore from what number?\n");
        return;
    }

    n = strtol(sp + 1, NULL, 10);
    if  (n <= 0 || n > SAVEGAMES) {
        printf("Invalid restore number. Specify %d through %d.\n", 1, SAVEGAMES);
        return;
    }

    if (savedGame[n-1].valid != 1) {
        printf("No game has been stored for number %d.\n", n);
        printf("Stored games:");
        for (i = 0; i < SAVEGAMES; i++) {
            if (savedGame[i].valid == 1) {
                printf(" %d", i+1);
            }
        }
        printf("\n");
        return;
    }

    printf("Restoring game state from number %d.\n", n);

    savedGame[n-1].valid = 1;
    for (i = 0; i < MAXITEMS; i++) {
        Inventory[i] = savedGame[n-1].Inventory[i];
    }
    for (i = 0; i < LastItem+1; i++) {
        locationOfItem[i] = savedGame[n-1].locationOfItem[i];
    }
    for (i = 0; i < NUMLOCATIONS; i++) {
        for (j = 0; j < 6; j++) {
            Move[i][j] = savedGame[n-1].Move[i][j];
        }
    }
    currentLocation = savedGame[n-1].currentLocation;
    turnsPlayed = savedGame[n-1].turnsPlayed;
    lampLit = savedGame[n-1].lampLit;
    lampFilled = savedGame[n-1].lampFilled;
    ateFood = savedGame[n-1].ateFood;
    drankWater = savedGame[n-1].drankWater;
    ratAttack = savedGame[n-1].ratAttack;
    wolfState = savedGame[n-1].wolfState;
}
#endif /* FILEIO */

/* Prompt user and get a line of input */
void prompt()
{
#ifdef __CC65__
#ifdef JOYSTICK
    unsigned char joy;
#endif /* JOYSTICK */
#endif /* __CC65__ */

    printf("\n? ");

#if defined(__CC65__) && !defined(__KIM1__)
    while (1) {
        if (kbhit()) {
            fgets(buffer, sizeof(buffer)-1, stdin); /* Get keyboard input */
            buffer[strlen(buffer)-1] = '\0'; /* Remove trailing newline */
            break;
#ifdef JOYSTICK
        } else {
            /* Check for joystick input */
            joy = joy_read(1);
            if (joy == JOY_UP_MASK) {
                strcpy(buffer, "n");
                while (joy_read(1) != 0)
                    ; /* Wait for joystick to be released */
                break;
            } else if (joy == JOY_DOWN_MASK) {
                strcpy(buffer, "s");
                while (joy_read(1) != 0)
                    ; /* Wait for joystick to be released */
                break;
            } else if (joy == JOY_RIGHT_MASK) {
                strcpy(buffer, "e");
                while (joy_read(1) != 0)
                    ; /* Wait for joystick to be released */
                break;
            } else if (joy == JOY_LEFT_MASK) {
                strcpy(buffer, "w");
                while (joy_read(1) != 0)
                    ; /* Wait for joystick to be released */
                break;
            } else if (joy == (JOY_UP_MASK|JOY_BTN_1_MASK)) {
                strcpy(buffer, "u");
                while (joy_read(1) != 0)
                    ; /* Wait for joystick to be released */
                break;
            } else if (joy == (JOY_DOWN_MASK|JOY_BTN_1_MASK)) {
                strcpy(buffer, "d");
                while (joy_read(1) != 0)
                    ; /* Wait for joystick to be released */
                break;
            }
#endif /* JOYSTICK */
        }
    }
#else
    /* Get keyboard input */
    fflush(stdout);
    fgets(buffer, sizeof(buffer)-1, stdin);

    /* Remove trailing newline */
    buffer[strlen(buffer)-1] = '\0';
#endif /* __CC65__ */
}

/* Do special things unrelated to command typed. */
void doActions()
{
    if ((turnsPlayed == 10) && !lampLit) {
        printf("It will be getting dark soon. You need some kind of light or soon you won't\nbe able to see.\n");
    }

    if ((turnsPlayed >= 60) && (!lampLit || (!itemIsHere("lamp") && !carryingItem("lamp")))) {
        printf("It is dark out and you have no light. You stumble around for a while and\nthen fall, hit your head, and pass out.\n");
        gameOver = 1;
        return;
    }

    if ((turnsPlayed == 20) && !drankWater) {
        printf("You are getting very thirsty. You need to get a drink soon.\n");
    }

    if ((turnsPlayed == 30) && !ateFood) {
        printf("You are getting very hungry. You need to find something to eat.\n");
    }

    if ((turnsPlayed == 50) && !drankWater) {
        printf("You pass out due to thirst.\n");
        gameOver = 1;
        return;
    }

    if ((turnsPlayed == 40) && !ateFood) {
        printf("You pass out from hunger.\n");
        gameOver = 1;
        return;
    }

    if (currentLocation == Tunnel) {
        if (itemIsHere("cheese")) {
            printf("The rats go after the cheese.\n");
        } else {
            if (ratAttack < 3) {
                printf("The rats are coming towards you!\n");
                ++ratAttack;
            } else {
                printf("The rats attack and you pass out.\n");
                gameOver = 1;
                return;
            }
        }
    }

    /* wolfState values:  0 - wolf attacking 1 - wolf gone, Matthew in tree. 2 - Matthew safe, you won. Game over. */
    if (currentLocation == WolfTree) {
        switch (wolfState) {
            case 0:
                printf("A wolf is circling around the tree. Matthew is up in the tree. You have to\nsave him! If only you had some kind of weapon!\n");
                break;
            case 1:
                printf("Matthew is afraid to come down from the tree. If only you had\nsomething to coax him with.\n");
                break;
            case 2:
                printf("Congratulations! You succeeded and won\nthe game. I hope you had as much fun playing the game as I did creating it.\n- Jeff Tranter <tranter@pobox.com>\n");
                gameOver = 1;
                return;
                break;
            }
    }
}

/* Set variables to values for start of game */
void initialize()
{
    currentLocation = Driveway1;
    lampFilled = 0;
    lampLit = 0;
    ateFood = 0;
    drankWater = 0;
    ratAttack = 0;
    wolfState = 0;
    turnsPlayed = 0;
    gameOver = 0;

    /* These doors can get changed during game and may need to be reset */
    Move[17][North] = 0;
    Move[21][North] = 0;

    /* Set inventory to default */
    memset(Inventory, 0, sizeof(Inventory[0])*MAXITEMS);
    Inventory[0] = Flashlight;

    /* Put items in their default locations */
    locationOfItem[0]  = 0;                /* NoItem */
    locationOfItem[1]  = Driveway1;        /* Key */
    locationOfItem[2]  = Hayloft;          /* Pitchfork */
    locationOfItem[3]  = 0;                /* Flashlight */
    locationOfItem[4]  = WorkRoom;         /* Lamp */
    locationOfItem[5]  = Garage;           /* Oil */
    locationOfItem[6]  = Kitchen;          /* Candybar */
    locationOfItem[7]  = Driveway2;        /* Bottle */
    locationOfItem[8]  = GirlsBedroom;     /* Doll */
    locationOfItem[9]  = BoysBedroom;      /* ToyCar */
    locationOfItem[10] = ServantsQuarters; /* Matches */
    locationOfItem[11] = Woods25;          /* GoldCoin */
    locationOfItem[12] = Woods29;          /* SilverCoin */
    locationOfItem[13] = DiningRoom;       /* StaleMeat */
    locationOfItem[14] = DrawingRoom;      /* Book */
    locationOfItem[15] = LaundryRoom;      /* Cheese */
    locationOfItem[16] = MasterBedroom;    /* OldRadio */
}

/* Main program (obviously) */
int main(void)
{

#ifdef __CC65__
#ifdef JOYSTICK
    unsigned char Res;
    Res = joy_load_driver(joy_stddrv);
    Res = joy_install(joy_static_stddrv);
#endif /* JOYSTICK */
#endif /* __CC65__ */

#ifndef FILEIO
    /* Mark all saved games as initially invalid */
    int i;
    for (i = 0; i < SAVEGAMES; i++) {
        savedGame[i].valid = 0;
    }
#endif

    while (1) {
        initialize();
        clearScreen();
        printf("%s", introText);
        while (!gameOver) {
            prompt();
            if (buffer[0] == '\0') {
                /* Ignore empty line */
            } else if (matchesCommand("help", buffer)) {
                doHelp();
            } else if (matchesCommand("inventory", buffer)) {
                doInventory();
            } else if (matchesCommand("go", buffer)
                       || !strcasecmp(buffer, "n") || !strcasecmp(buffer, "s")
                       || !strcasecmp(buffer, "e") || !strcasecmp(buffer, "w")
                       || !strcasecmp(buffer, "u") || !strcasecmp(buffer, "d")
                       || !strcasecmp(buffer, "north") || !strcasecmp(buffer, "south")
                       || !strcasecmp(buffer, "east") || !strcasecmp(buffer, "west")
                       || !strcasecmp(buffer, "up") || !strcasecmp(buffer, "down")) {
                doGo();
            } else if (matchesCommand("look", buffer)) {
                doLook();
            } else if (matchesCommand("take", buffer)) {
                doTake();
            } else if (matchesCommand("examine", buffer)) {
                doExamine();
            } else if (matchesCommand("use", buffer)) {
                doUse();
            } else if (matchesCommand("drop", buffer)) {
                doDrop();
            } else if (tolower(buffer[0]) == 'b') {
                doBackup();
            } else if (tolower(buffer[0]) == 'r') {
                doRestore();
            } else if (matchesCommand("quit", buffer)) {
                doQuit();
            } else {
                printf("I don't understand. Try 'help'.\n");
            }

            /* Handle special actions. */
            doActions();
        }

        printf("Game over after %d turns.\n", turnsPlayed);
        printf("%s", "Do you want to play again (y/n)? ");
        fflush(stdout);
        fgets(buffer, sizeof(buffer)-1, stdin);
        if (tolower(buffer[0]) == 'n') {
            break;
        }
    }
    return 0;
}
