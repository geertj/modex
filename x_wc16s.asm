;;----------------------------------------------------------------------------|
;                                                                            |
;  Module x_wc16.asm                                                              |
;                                                                            |
;----------------------------------------------------------------------------|
;                                                                            |
;  Author       : G.T.Jansen                                                 |
;  Written for  : Watcom C/C++ 16-bit compiler.                              |
;  Date         : 9-11-'94                                                   |
;  Updated with : Turbo Assembler 4.0 / Editor Borland Pascal                |
;  Update       : 13-03-'95                                                  |
;  Update	: 7-09-'95
;                                                                            |
;----------------------------------------------------------------------------|
%TITLE "Mode X library for use with Turbo Assembler"

          IDEAL
          P386
          LOCALS @@
          MODEL small

PUBLIC    initx_, closex_, setpal_
PUBLIC    getpal_, putpixel_
PUBLIC    clrscr_, setvpage_, setapage_
PUBLIC    setvstart_, setastart_, waitvrt_
PUBLIC    fpoly_, fadepal_


;----------------------------------------------------------------------------|
;                          Equates                                           |
;----------------------------------------------------------------------------|

AttrIndex       EQU 03C0h         ; Attribute controller index register
MisIndex        EQU 03C2h         ; Miscellaneous output register
SequIndex       EQU 03C4h         ; Sequence controller index
SequData        EQU 03C5h         ; Sequence data register index
DacRead         EQU 03C7h         ; DAC read index register
DacWrite        EQU 03C8h         ; DAC write index register
DacData         EQU 03C9h         ; DAC data register
GrapIndex       EQU 03CEh         ; Graphics controller index
CrtCIndex       EQU 03D4h         ; CRT controller index
InpStatus       EQU 03DAh         ; Input status 0 register


True            EQU 1
False           EQU 0


;----------------------------------------------------------------------------|
;                          MACRO DEFINITIONS                                 |
;----------------------------------------------------------------------------|

;--------------------------------------------------------------------------|
; WritePixel: Writes a pixel to the screen in mode X. The X-Coordinat is   |
;             passed in AX, the Y in BX, the VGA-DMA Area address is       |
;             passed in ES, and the color in gs.                           |
;             This macro is going to be expanded in the line and Filltri   |
;             procedures.                                                  |
;                                                                          |
; Rights:     This macro may destroy dx, if other registers are used, they |
;             should be stored temporarily on the stack.                   |
;--------------------------------------------------------------------------|

MACRO     WritePixel
          push   ax
          push   bx
          push   cx

          mov    cx, ax
          shr    ax, 2
          shl    bx, 4      ; bx = 16 * bx
          mov    dx, bx     ; dx = 16 * bx
          shl    dx, 2      ; dx = 64 * bx
          add    bx, dx     ; bx = ( 16 + 64) * bx = 80 * bx
          add    bx, ax     ; bx = ( 320/4) * Y + X/4
          mov    dx, fs
          add    bx, dx     ; Add the offset of the active page to support
                            ;  paging.

          mov    dx, 03c4h  ; Address of the sequencer
          and    cx, 3h
          mov    ah, 1h
          shl    ah, cl
          mov    al, 02h    ; Index of the 'Select plane' service
          out    dx, ax

          mov    ax, gs
          mov    [es:bx], al

          pop    cx
          pop    bx
          pop    ax
ENDM      WritePixel



;----------------------------------------------------------------------------|
;                   END MACRO DEFINITIONS                                    |
;----------------------------------------------------------------------------|




;----------------------------------------------------------------------------|
;                   BEGIN DATA SEGMENT                                       |
;----------------------------------------------------------------------------|
DATASEG

Columns          dw  ?         ; Nr of columns on a full screen
Rows             dw  ?         ; Nr of rows in a full screen
PageSize         dw  ?         ; Nr of bytes in a Page( in DMA-Area, not
                                    ;  on the VGA-Card!
ScreenWidth      dw  ?         ; Nr of bytes in a scanline( in DMA-Area)
ActiveStart      dw  0         ; Ofs in DMA where the active page begins
VisibleStart     dw  0         ; Ofs in DMA where the visible page begins

LastMode         db  ?

Offsets          dw  ?
                 dw  ?

ENDS _DATA

;----------------------------------------------------------------------------|
;                   BEGIN CODE SEGMENT                                       |
;----------------------------------------------------------------------------|
CODESEG

PROC      initx_ NEAR
; First let's get the old videomode
          push eax cx dx di es

          mov  ah, 0Fh
          int  10h
          mov  [LastMode], al

; mode x is based on mode 13h, so lets set that mode
          mov  ax, 0013h
          int  10h

;  Turn off chain-4 bit( port 3c4h, index 2, bit 4) to go to unchained
;   (planar) mode. Now we have access to 256 Kb of video memory.
          mov  dx, SequIndex
          mov  ax, 0604h
          out  dx, ax

; Turn off word mode, by setting the Mode Control register
;  of the CRT Controller (index 0x17, port 0x3d4):
          mov  dx, CrtCIndex
          mov  ax, 0e317h
          out  dx, ax

; Turn off doubleword mode, by setting the Underline Location
;  register (index 0x14, port 0x3d4):
          mov  ax, 0014h
          out  dx, ax

; Increase resolution to 320x240 256 color, also known as mode X

; Modify the vertical sync polarity bits in the Misc. Output
; Register to achieve square aspect ratio:
          mov  dx, MisIndex
          mov  al, 0e3h
          out  dx, al

; Modify the vertical timing registers to reflect the increased
;  vertical resolution, and to center the image as good as
;  possible:
          mov  dx, CrtCIndex
          mov  ax, 2c11h          ; turn off write protect
          out  dx, ax
          mov  ax, 0d06h          ; vertical total
          out  dx, ax
          mov  ax, 03e07h           ; overflow register
          out  dx, ax
          mov  ax, 0ea10h         ; vertical retrace start
          out  dx, ax
          mov  ax, 0ac11h         ; vertical retrace end AND wr.prot
          out  dx, ax
          mov  ax, 0df12h          ; vertical display enable end
          out  dx, ax
          mov  ax, 0e715h         ; start vertical blanking
          out  dx, ax
          mov  ax, 0616h          ; end vertical blanking
          out  dx, ax

; X mode is now set, let's clear the screen
          mov  dx, SequIndex
          mov  ax, 0f02h          ; Select all planes
          out  dx, ax
          mov  ax, 0a000h
          mov  es, ax
          xor  di, di
          xor  eax, eax
          mov  cx, 16384d
          rep  stosd

; Set the global video-parameters correctly
          mov  [ScreenWidth], 80d
          mov  [Columns], 320d
          mov  [Rows], 240d
          mov  ax, [ScreenWidth]
          mul  [Columns]
          mov  [PageSize], ax
          xor  ax, ax
          mov  [ActiveStart], ax
          mov  [VisibleStart], ax

          pop  es di dx cx eax
          cld                          ; little req. from watcom.
          ret
ENDP      initx_


PROC      closex_ NEAR
          push ax

          mov  al, [LastMode]
          mov  ah, 0
          int  10h

          pop  ax
          ret
ENDP      closex_


;----------------------------------------------------------------------------
; PROCEDURE SetActivePage: Sets the active page, that is the one on wich the
;                          drawing operations take place.
;----------------------------------------------------------------------------
; prototype void setapage( int apage);
;---------------------------------------------------------------------------
; Watcom passes: APage: ax
;---------------------------------------------
PROC      setapage_
          push  bx

          mov   bx, 320*240/4
          mul   bx
          mov   [ActiveStart], ax

          pop   bx
          ret
ENDP      setapage_


PROC      setastart_ NEAR
          mov   [ActiveStart], ax

          ret
ENDP      setastart_


PROC      setvpage_ NEAR

          push  bx dx

          mov   bx, 320*240/4
          mul   bx

          mov   dx, CrtCIndex
          mov   bx, ax
          mov   al, 0ch     ; Send high byte to sequencer, index 0ch
          out   dx, ax
          mov   ah, bl
          mov   al, 0dh     ; Send low byte to sequencer, index 0dh
          out   dx, ax

          pop   dx bx
          ret
ENDP      setvpage_

PROC      setvstart_ NEAR
          push  bx dx

          mov   dx, CrtCIndex
          mov   bx, ax
          mov   al, 0ch     ; Send high byte to sequencer, index 0ch
          out   dx, ax
          mov   ah, bl
          mov   al, 0dh     ; Send low byte to sequencer, index 0dh
          out   dx, ax

          pop   dx bx
          ret
ENDP      setvstart_


PROC     waitvrt_ NEAR
          mov     dx,3dah
VRT:      in      al,dx
          test    al,8
          jnz     VRT         ;wait until Verticle Retrace starts
NoVRT:    in      al,dx
          test    al,8
          jz      NoVRT       ;wait until Verticle Retrace Ends
          ret
ENDP      waitvrt_


;----------------------------------------------------------------------------|
; PROCEDURE FPoly:   -Fills a polygon on the screen with <color>. The polygon|
;                    has te be _CONVEX_ and the vertices have to be passed   |
;                    in an (counter)clockwise order.                         |
;                    -A DDA algorithm is used to determine the begin and end-|
;                    ing points of the horizontal line with which the polygon|
;                    is filled.                                              |
;                    -The polygon is clipped automatically to the screen     |
;                    screen.                                                 |
;                    -This procedure can also be used with other resolutions |
;                    than the 320x240 of mode X. This is because the polygon |
;                    is calculated with 32-bit fixed-point arithmetic. The   |
;                    only thing that has to be rewritten is the HorLine      |
;                    macro.
;----------------------------------------------------------------------------|
; prototype:  void fpoly( int nrvertices, void *pvertices, int color)        |
;----------------------------------------------------------------------------|
; Watcom passes the parameters as follows:                                   |
; nrvertices: ax                                                             |
; pvertices : ds:dx                                                          |
; color     : bx                                                             |
;----------------------------------------------------------------------------|

  ;--------------------------------------------------------------------------|
  ; MACRO LinFit: This procedure calculates a linear equation out of two of  |
  ;               it's solutions and returns the coefficients of it.         |
  ;--------------------------------------------------------------------------|
  ; Input:  AX= X1, BX=Y1, CX=X2, DX=Y2                                      |
  ;--------------------------------------------------------------------------|
  ; Output: EAX=a, ECX=b (See notes)  FIXED!                                       |
  ;--------------------------------------------------------------------------|
  ; Notes: The line is represented here by the equation X = a*Y + b, not by  |
  ;        the always used Y = a*X + B. This is because we fill the polygon  |
  ;        with horizontal lines, as we can draw them much faster than ver-  |
  ;        tical lines. So with the former equation we can calculate X much  |
  ;        faster.                                                           |
  ;--------------------------------------------------------------------------|

  MACRO     LinFit
            push  dx

            sub   bx, dx
            movsx ebx, bx
            sub   ax, cx
            movsx eax, ax
            sal   eax, 12
            cdq
            idiv  ebx

            pop   dx
            movsx edx, dx
            imul  edx, eax
            movsx ecx, cx
            sal   ecx, 12
            sub   ecx, edx
  ENDM      LinFit

  ;--------------------------------------------------------------------------|
  ;  MACRO HorLine:  This macro draws a horizontal line between two points   |
  ;                                                                          |
  ;--------------------------------------------------------------------------|
  ;  Input: The Y-coordinat is passed in edx (integer), the two x-es are     |
  ;         passed via the stack (32-bit fixed-point).                       |
  ;         The VGA DMA-Area segment address has to be in es, the offset     |
  ;         of the active part has to be in fs.                              |
  ;         The color of the line is passed in gs.                           |
  ;--------------------------------------------------------------------------|
  ;  Output: -                                                               |
  ;--------------------------------------------------------------------------|

  MACRO     HLine2

            push  edx
            push  edi

            xchg  edx, ebx               ; Y is passed in edx, should be ebx
            cmp   ebx, 240
            jae   @@done

            cmp   eax, edx        ; Make sure that eax<edx
            jle   @@dont_swap
            xchg  eax, edx
  @@dont_swap:


  ;--Clip the line if it falls outside the screen.

            cmp   eax, 0
            jge   @@hl2
            cmp   edx, 0
            jge   @@hl3
            jmp   @@done
  @@hl3:    mov   eax, 0
            cmp   edx, 320
            jl    @@clip_end
            mov   edx, 319
            jmp   @@clip_end

  @@hl2:    cmp   eax, 320
            jge   @@done
            cmp   edx, 320
            jl    @@clip_end
            mov   edx, 319
  @@clip_end:


  ;--We now have two x-values, one in ax and one in dx. We must now draw a
  ;--line from the left (=ax)  to the right (=dx).

            shl   bx, 4       ; bx holds the y-coordinat
            mov   cx, bx
            shl   bx, 2
            mov   di, bx
            add   di, cx      ; di = 320 * ( y/4)
            mov   bx, fs
            add   di, bx      ; Add offset of the active page to support paging.

            sub   dx, ax
            inc   dx          ; dx = # of pixels to write
            cmp   dx, 4h
            ja    @@bigline   ; If # of pixels > 4 it will be sure that the
                              ; line crosses a byte-boundary in VGA-mem.
                              ; If not we write pixel by pixel, if yes, we
                              ; use an algorith that writes many pixels at one
                              ; time

            mov   cx, dx
            mov   dx, ax

            dec   dx
wpixel:     inc   dx
            push  cx
            mov   cx, dx
            and   cx, 03h
            mov   ah, 1h
            shl   ah, cl
            pop   cx

            mov   al, 02h
            push  dx
            mov   dx, 03c4h
            out   dx, ax
            pop   dx

            mov   bx, dx
            shr   bx, 2
            mov   ax, gs
            mov   [es:di + bx ], al
            loop  wpixel
            jmp   @@done

@@bigline:  mov   bx, ax
            shr   bx, 2
            add   di, bx      ; di = offset (in VGA DMA-Area) leftmost pixel.

            mov   bx, ax      ; bx = left x=coordinat.

            mov   cx, ax
            and   cx, 3h
            mov   ah, 0fh
            shl   ah, cl
            and   ah, 0fh

            mov   al, cl      ; al = index of first plane to write to.

            xor   cx, 3h
            inc   cx          ; cx := 4 - cx (= nr of pixels we intend to
                              ;  write at es:di
            push  dx          ; Store # of pixels for later use

            mov   al, 02h
            mov   dx, 03c4h
            out   dx, ax
            mov   ax, gs
            mov   [es:di], al
            inc   di

            pop   ax
            sub   ax, cx      ; Sub the # of pixels that have been written alr.

            mov   cx, ax
            and   ax, 3h      ; ax = # of pixels to write after all bytes and
            push  ax          ;  dwords are written.
            shr   cx, 2       ; cx = # of bytes to write
            mov   ax, cx
            and   ax, 3h      ; ax = number of bytes to write after all dwords
            push  ax          ;  are written.
            shr   cx, 2       ; cx = # of dwords to write

            mov   dx, 03c4h   ; Sequencer Address
            mov   ax, 0f02h   ; Select all planes.
            out   dx, ax

            mov   ax, gs      ; The color is stored in gs.
            rol   eax, 16
            mov   ax, gs      ; eax contains 4 X the color value
            rep   stosd       ; And store 16!! pixels at a time.

            pop   cx
            rep   stosb       ; And store 4 pixels at a time.

            pop   cx          ; cx = number of pixels that last
            mov   ah, 0f0h    ; fill upper nybble with 1's
            rol   ah, cl      ; the lower nybble contains the planes that must
                              ;  written to.
            and   ah, 0fh     ; Mask out high nybble
            mov   al, 02h     ; Select index 02h: select write plane,
            out   dx, ax      ;  of the sequencer.
            mov   ax, gs      ; The color is stored in gs
            mov   [es:di], al ; and write to DMA-Area.

  @@done:   pop   edi
            pop   edx

  ENDM      HLine2


%NEWPAGE
PROC      fpoly_  NEAR
          LOCAL Smallest: WORD, Biggest: WORD, Vert1: DWORD, Vert2:DWORD,\
                B1: DWORD, B2: DWORD, Done: BYTE, LastVertix1: BYTE,\
                LastVertix2: BYTE, MaxY1: DWORD, MaxY2: DWORD,\
                PVertices: DWORD, NrVertices: WORD =StackSize

          push  bp
          mov   bp, sp
          sub   sp, StackSize

          push  cx si di es fs gs

          mov   bh, bl
          mov   gs, bx      ;   The color of the line is stored in gs
                            ;    during the entire procedure.
          mov   bx, gs

          mov   cx, [ActiveStart]
          mov   fs, cx      ;

          mov   [word ptr PVertices], dx
          mov   [word ptr PVertices + 2], ds
          xor   edi, edi
          les   di, [dword ptr PVertices]


          xor   esi, esi
          mov   si, ax                  ; si = nrvertices
          mov   [NrVertices], ax
          dec   si
          mov   dx, [es:edi + esi*4 +  2] ; Assume the last Y-coordinat is the
          mov   bx, dx                  ; smallest and the biggest for now.
          mov   [Smallest], si          ; The smallest value is stored in dx, the
          mov   [Biggest], si           ; biggest in bx.

;-------Loop all Y-coordinats to find the smallest and the biggest.

loop1:    dec   si
          mov   ax, [es:edi + esi*4 + 2]
          cmp   ax, dx
          jge   fp1
          mov   dx, ax
          mov   [Smallest], si
fp1:      cmp   ax, bx
          jle   fp2
          mov   bx, ax
          mov   [Biggest], si
fp2:      test  si, si
          jnz   loop1

          mov   ax, [Biggest]
          mov   ax, [Smallest]

;--Initialise edx(=y), es, MaxY1 and MaxY2.

          mov   [LastVertix1], False
          mov   [LastVertix2], False
          mov   [Done], False

          xor   ebx, ebx
          mov   bx, [Smallest]
          mov   [Vert1], ebx
          mov   [Vert2], ebx
          shl   bx, 2
          mov   ax, [es:di + bx + 2]
          cwde
          mov   edx, eax           ; ebx = y
          mov   [MaxY1], edx
          mov   [MaxY2], edx
          mov   ax, 0a000h
          mov   es, ax
          dec   edx

;--Fill the polygon scanline by scanline.

NextScanLine:
          inc   edx

          cmp   edx, [MaxY1]
          jne   getx1

;----We reached another vertix: Change the A & B and MaxY, and check if
;----y has reached it's maximum value.

          push  edx
          push  edi
          push  es
next_vertix1:
          cmp   [LastVertix1], True
          jne   fp3
          mov   [Done], True
          jmp   fp5
fp3:
          xor   edi, edi
          les   di, [dword ptr PVertices]
          mov   ebx, [Vert1]                ; Which is the vertix we reached?
          mov   cx, [es:edi + ebx*4]        ; Store X1 and Y1 for the linfit
          mov   dx, [es:edi + ebx*4 + 2]    ; macro.
          inc   ebx                         ; Go to the next vertix.

          cmp   bx, [NrVertices]            ; Walk circular through the list
          jl    fp4
          sub   bx, [NrVertices]            ; "
fp4:      mov   [Vert1], ebx

          cmp   bx, [Biggest]               ; Have we reached the last vertix
          jne   not_at_end1                 ; yet?
          mov   [LastVertix1], True
not_at_end1:
          mov   ax, [es:edi + ebx*4 + 2]
          push  ax
          cwde
          mov   [MaxY1], eax
          mov   ax, [es:edi + ebx*4]        ; Store X2 and Y2 for the LinFit
          pop   bx                          ; macro.
          cmp   bx, dx
          je    next_vertix1           ; Check if the line is horizontal.
                                       ; If it is: skip this vertix and pro-
                                       ; ceed with the next one. We will
                                       ; 'catch' this also with the next
                                       ; horizontal line.
          LinFit
          mov   esi, eax
          mov   [B1], ecx

fp5:      pop   es
          pop   edi
          pop   edx

getx1:    mov   eax, edx               ; eax = y (integer)
          imul  eax, esi               ; eax = a*y (fixed)
          add   eax, [B1]              ; eax = a*y + b (fixed}
          push  eax                    ; Store this x-coordinat for the
                                       ; horline-macro.

;----Proceed with the second line.

          cmp   edx, [MaxY2]           ; check if y has reached the next
          jne   getx2                  ; vertix.

;----We reached another vertix: Change the A2 & B2 and MaxY2. and check if
;----y has reached it's maximum value.

          push  edx
          push  esi
          push  es
next_vertix2:
          cmp   [LastVertix2], True
          je    fp7

          xor   esi, esi
          les   si, [dword ptr PVertices]   ; This destroys A1 in esi.
          mov   ebx, [Vert2]                ; Which is the vertix we reached?
          mov   cx, [es:esi + ebx*4]        ; Store X1 and Y1 for the linfit
          mov   dx, [es:esi + ebx*4 + 2]    ; macro.
          dec   bx                          ; Go to the next vertix.

          cmp   bx, 0                       ; Walk circular through the list
          jge   fp6
          add   bx, [NrVertices]            ; "
          movsx ebx, bx
fp6:      mov   [Vert2], ebx

          cmp   bx, [Biggest]               ; Have we reached the last vertix
          jne   not_at_end2                 ; yet?
          mov   [LastVertix2], True
not_at_end2:
          mov   ax, [es:esi + ebx*4 +2]
          push  ax
          cwde
          mov   [MaxY2], eax
          mov   ax, [es:esi + ebx*4]        ; Store X2 and Y2 for the LinFit
          pop   bx                          ; macro.
          cmp   bx, dx
          je    next_vertix2

          LinFit
          mov   edi, eax
          mov   [B2], ecx

fp7:      pop   es
          pop   esi
          pop   edx

getx2:    mov   eax, edx               ; eax = y (integer)
          imul  eax, edi               ; eax = a2*y (fixed)
          add   eax, [B2]              ; eax = a2*y + b2 (fixed}
          pop   ebx                    ; ebx = other x
          sar   eax, 12
          sar   ebx, 12

          HLine2                       ; draw a horizontal line, y: ebx,
                                       ; x-es: eax, ebx
          cmp   [Done], True
          jne   NextScanLine

;--The polygon is filled: Free used stack-space and return to the caller

          pop   gs fs es di si cx
          mov   sp, bp
          pop   bp
          ret
ENDP      fpoly_


;----------------------------------------------------------------------------|
; Procedure PutPixel: Puts a pixel on the screen at ( X, Y) in the color     |
;                     <Color>.                                               |
;----------------------------------------------------------------------------|
; prototype: void putpixel( int x, int y, int color);
;--------------------------------------------------------
; parameters: x     : AX
;             y     : DX
;             color : BX
;---------------------------------------------------------------------------

PROC      putpixel_ NEAR

          pusha
          cmp   ax, 320
          jae   @@Done

          cmp   dx, 240
          jae   @@Done

          mov   cx, 0a000h ; VGA DMA-Area segment address
          mov   es, cx

          shl   dx, 4      ; dx =
          mov   cx, dx
          shl   dx, 2
          add   dx, cx     ; dx = 80*y

          mov   di, ax     ; di = x
          shr   di, 2      ; di = x/4
          add   di, dx     ; di = 80*y + x/4
          add   di, [ActiveStart]
          mov   cx, ax
          and   cx, 3h
          mov   ah, 1
          shl   ah, cl
          mov   al, 02h
          mov   dx, 03c4h
          out   dx, ax
          mov   [byte ptr es:di], bl
@@Done:
          popa
          ret
ENDP      putpixel_

;----------------------------------------------------------------------------|
; Procedure ClearScreen: Fills the entire VGA DMA area with zeroes.          |
;----------------------------------------------------------------------------|
; Input:  -                                                                  |
;----------------------------------------------------------------------------|
; Output: -                                                                  |
;----------------------------------------------------------------------------|
PROC      clrscr_ NEAR
          push  ax cx dx es

          mov   dx, 03c4h
          mov   ax, 0f02h
          out   dx, ax        ; Select all planes

          mov   ax, 0a000h    ; VGA DMA Area.
          mov   es, ax
          mov   di, [ActiveStart]
          xor   ax, ax        ; zero eax, we want to copy 0's.
          mov   cx, 9600      ; cx = nr of words that fit on the screen.
          cld
          rep   stosw

          pop   es dx cx ax
          ret
ENDP      clrscr_

;----------------------------------------------------------------------------|
; Procedure SetPalette: Sets the palette's DAC-registers.                    |
;----------------------------------------------------------------------------|
; prototype: void setpal( int firstdac, int nrdacs, void *pal);              |
;----------------------------------------------------------------------------|
; Parameters: ax = FirstDAC                                                  |
;             dx = NrDACs                                                    |
;             ds:bx => Palette                                               |
;----------------------------------------------------------------------------|

PROC      setpal_ NEAR
          push  cx si        ; Push all registers that aren't used to pass
                             ; parameters (requirement from watcom).
          mov   si, bx       ; bx:si now points to palette.

          test  ah, ah       ; check if ax =< 255
          jnz   @@done

          mov   cx, dx
          jcxz  @@done

          mov   dx, cx
          add   dx, cx
          add   cx, dx       ; cx = 3*[NrDACs]

          mov   dx, DacWrite
          out   dx, al

          mov   dx, DacData
          cld
          rep   outsb

@@done:   pop   si cx
          ret
ENDP      setpal_

;----------------------------------------------------------------------------|
; Procedure GetPalette: Gets the palette's DAC-registers.                    |
;----------------------------------------------------------------------------|
; prototype: void getpal( int firstdac, int nrdacs, void *pal);              |
;----------------------------------------------------------------------------|
; Parameters: ax = FirstDAC                                                  |
;             dx = NrDACs                                                    |
;             ds:bx => Palette                                               |
;----------------------------------------------------------------------------|
PROC      getpal_  NEAR

          push  cx di es   ; Push all registers that AREN'T used to pass a
                           ; parameter (requirement from watcom).
          push  ds
          pop   es
          mov   di, bx

          test  ah, ah
          jnz   @@done

          mov   cx, dx     ; cx = NrDACs
          jcxz  @@done

          mov   bx, cx
          add   bx, cx
          add   cx, bx     ; cx = 3*NrDACs

          mov   dx, DacRead
          out   dx, al
          mov   dx, DacData
          cld
          rep   insb

@@done:   pop   es di cx
          ret
ENDP      getpal_

;----------------------------------------------------------------------------|
; Procedure FadePal: This procedure fades the current palette to the palette |
;                    pointed to by ToPal. The fading is done in 64 steps.    |
;                    No tricks, just 1/64 of the difference is added to the  |
;                    colorvalue in one step. A 16-bit 8:8 fixed point is used|
;                    to store the stepsize per color.                        |
;----------------------------------------------------------------------------|
; Notes:  -This proc. uses about 3k of stackspace, make sure there is enough |
;         space.                                                             |
;----------------------------------------------------------------------------|
; prototype: void fadepal( void *topal);                                     |
;----------------------------------------------------------------------------|
; Parameters: ds:ax => topal                                                 |
;----------------------------------------------------------------------------|
PROC      fadepal_ NEAR
          LOCAL FadeTab: WORD:768, CurPal: WORD:768 = StackSize

          push  bp
          mov   bp, sp
          sub   sp, StackSize

          push  bx cx dx si di es

          push  ds
          pop   es
          mov   bx, ax       ; es:bx point to <topal>

          mov   dx, DacRead
          xor   ax, ax
          out   dx, al

          mov   dx, DacData
          xor   si, si

RPal:     in    al, dx
          xor   ah, ah

          xor   ch, ch
          mov   cl, [byte ptr es:bx + si]    ; cl = Topal[ si]
          sub   cx, ax
          sal   cx, 2   ; cx = (Topal[ si] - CurPal[ si]) /64 in 16-bit
                        ; 8:8 fixed point.
          mov   di, si
          shl   di, 1
          mov   [word ptr FadeTab + di], cx
          shl   ax, 8                        ; Store the current value in
          mov   [word ptr CurPal + di], ax   ; 16-bit 8:8 fixed point.

          inc   si
          cmp   si, 768                ; Have we read the entire palette yet?
          jne   RPal

          mov   cx, 64                 ; We fade in 64 steps.
WPal:     call  NEAR waitvrt_
          xor   ax, ax
          mov   dx, DacWrite
          out   dx, al

          mov   dx, DacData
          xor   di, di

WPal1:    mov   ax, [word ptr CurPal + di]
          mov   bx, [word ptr FadeTab + di]
          add   ax, bx
          mov   [word ptr CurPal + di], ax
          shr   ax, 8
          out   dx, al
          add   di, 2
          cmp   di, 2*768
          jne   WPal1

          loop  WPal

          pop   es di si dx cx bx

          mov   sp, bp
          pop   bp
          ret
ENDP      fadepal_

          END
