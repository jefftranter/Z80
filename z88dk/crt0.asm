;
; A configurable CRT for bare-metal targets
;
;

    MODULE z80_crt0 

;-------
; Include zcc_opt.def to find out information about us
;-------

    defc    crt0 = 1
    INCLUDE "zcc_opt.def"

;-------
; Some general scope declarations
;-------

    EXTERN    _main           ;main() is always external to crt0 code
    PUBLIC    __Exit         ;jp'd to by exit()
    PUBLIC    l_dcal          ;jp(hl)
    EXTERN	  asm_im1_handler
    EXTERN	  asm_nmi_handler

IF DEFINED_CRT_ORG_BSS
    defc    __crt_org_bss = CRT_ORG_BSS
ENDIF

IFNDEF      CRT_ORG_CODE
    defc    CRT_ORG_CODE = 0x0000
ENDIF

IF CRT_ORG_CODE = 0x0000
    ; By default we don't have any rst handlers
    defc    TAR__crt_enable_rst = $0000
ENDIF

    ; Default, don't change the stack pointer
    defc    TAR__register_sp = -1
    ; Default, 32 functions can be registered for atexit()
    defc    TAR__clib_exit_stack_size = 32
    ; Default, halt loop
    defc    TAR__crt_on_exit = 0x10001

    defc    __CPU_CLOCK = 4000000
    INCLUDE "crt/classic/crt_rules.inc"


    org    	CRT_ORG_CODE


IF CRT_ORG_CODE = 0x0000
    jp      start
    INCLUDE "crt/classic/crt_z80_rsts.inc"
ENDIF

IFDEF CRT_INCLUDE_PREAMBLE
    INCLUDE "crt_preamble.asm"
ENDIF

start:
    INCLUDE "crt/classic/crt_init_sp.inc"
    ; Setup BSS memory and perform other initialisation
    call    crt0_init
    ; Make room for the atexit() stack
    INCLUDE "crt/classic/crt_init_atexit.inc"

    ; Setup heap if required
    INCLUDE "crt/classic/crt_init_heap.inc"

    ; Setup the desired interrupt mode
    INCLUDE "crt/classic/crt_init_interrupt_mode.inc"
    ; Turn on interrupts if desired
    INCLUDE "crt/classic/crt_init_eidi.inc"

    ; Initialization of console under HDOS
IF  HDOS = 1
SCALL macro call            ; SYSCALL macro
    rst     $38
    db      call
    endm
CONSL  equ  6               ; CONSL system call
EXIT   equ  0               ; EXIT system call
CSLMD  equ  0               ; Index for console mode
CSLECH equ  %10000000       ; Bit for suppress echo
CSLCHR equ  %00000001       ; Bit for update in character mode
    ld      a,CSLMD         ; Index
    ld      b,CSLECH|CSLCHR ; Suppress echo and update in character mode
    ld      c,CSLECH|CSLCHR ; Mask
    SCALL   CONSL           ; Initialize HDOS console
ENDIF

; Handle command line arguments under HDOS.
; Command line argument string starts at (SP) (with a space), ends at
; $227F (terminated with a null). If no command line options passed,
; (SP) = $2280.
; Example: "foo 1 22 33"
; SP -> $2276
; $2276 $2277 $2278 $2279 $227A $227B $227C $227D $227E' $227F
;  ' '   '1'   ' '   '2'   '2'   ' '   '3'   '3'   '3'    $00

IF HDOS = 1 && CRT_ENABLE_COMMANDLINE = 1
EOS equ     $227e
    ld      hl,sp       ; Check for case of no options, i.e. SP = $2280
    ld      a,$80
    cp      l
    jr      nz,args     ; Branch if arguments
    ld      a,$22
    cp      h
    jr      nz,args

    ; Handle case of no arguments
    ld      hl,argv0    ; name of program (null)
    push    hl
    ld      hl,0
    add     hl,sp	; address of argv
    ld      bc,1        ; argc is 1
    jp      pshargs

argv0:
    defb    0

args:
    ld      hl,EOS   ; Address of end of arguments
    sbc     hl,sp    ; Subtract SP to calculate length
    ld      c,l      ; Put length in C
    ld      b,0      ; Put zero in B
    ld      hl,EOS   ; Put end address back in HL

    ; Command line parsing code below wants (HL) = end of arguments,
    ; (C) = length of arguments, (B) = 0.

    INCLUDE	"crt/classic/crt_command_line.inc"
pshargs:
    push    hl	; argv
    push    bc	; argc
ELSE
    ld      hl,0
    push    hl  ; argv
    push    hl  ; argc
ENDIF

    ; Entry to the user code
    call    _main
    ; Exit code is in hl
__Exit:
    ; crt0_exit any resources
    call    crt0_exit

    ; Set the interrupt mode on exit
    INCLUDE "crt/classic/crt_exit_eidi.inc"

IF  HDOS = 1
    ld      a,0             ;   Flag for normal exit
    SCALL   EXIT            ;   HDOS EXIT system call
ENDIF

    ; How does the program end?
    INCLUDE "crt/classic/crt_terminate.inc"

l_dcal:
    jp      (hl)

    INCLUDE "crt/classic/crt_runtime_selection.inc"

    ; If we were given a model then use it
IF DEFINED_CRT_MODEL
    defc __crt_model = CRT_MODEL
ELIF DEFINED_CRT_ORG_BSS
    ;; If BSS is defined, then assume we're ROM model
    defc __crt_model = 1
ENDIF
    INCLUDE	"crt/classic/crt_section.inc"
