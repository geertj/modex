;----------------------------------------------------------------------------|
;                                                                            |
;  Module x_wc32f.asm                                                        |
;                                                                            |
;----------------------------------------------------------------------------|
;                                                                            |
;  Author       : G.T.Jansen                                                 |
;  Written for  : Watcom C-32 (Using DOS4GW)                                 |
;  Date         : 15-09-95                                                   |
;                                                                            |
;----------------------------------------------------------------------------|
%TITLE "Mode X library for use with Watom C32/DOS4GW"

          IDEAL
          P386
          LOCALS @@
          MODEL flat                ; YESS!! FUCK U DOS!!


GLOBAL    initx_: PROC, closex_: PROC, setpal_: PROC
GLOBAL    getpal_: PROC, putpixel_: PROC, hblit_: PROC
GLOBAL    clrscr_: PROC, setvpage_: PROC, setapage_: PROC
GLOBAL    setvstart_: PROC, setastart_: PROC, waitvrt_: PROC
GLOBAL    fpoly_: PROC, fadepal_: PROC



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

SCREEN_LIN_ADDRESS    EQU 0a0000h

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
ActiveStart      dd  SCREEN_LIN_ADDRESS
                               ; Linear address where the active page begins

LastMode         db  ?


;----------------------------------------------------------------------------|
;                   BEGIN CODE SEGMENT                                       |
;----------------------------------------------------------------------------|
CODESEG

PROC      initx_ NEAR
; First let's get the old videomode
          push eax ecx edx edi

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

          mov  edi, SCREEN_LIN_ADDRESS
          xor  eax, eax
          mov  ecx, 4000h         ; nr of dwords that fit in 64k.
          cld
          rep  stosd

          pop  edi edx ecx eax
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
; Watcom passes: APage: eax
;---------------------------------------------
PROC      setapage_
          imul  eax, 320*240/4;
          add   eax, SCREEN_LIN_ADDRESS
          mov   [ActiveStart], eax
          ret
ENDP      setapage_


PROC      setastart_
          add   eax, SCREEN_LIN_ADDRESS
          mov   [ActiveStart], eax
          ret
ENDP      setastart_

;-------------------------------------------------------------------
; Procedure setvpage: Sets the visible page on the vga-card, that is the one
;                     which is displayed on the screen.
;---------------------------------------------------------------------------
; prototype: void setvpage( int page);
;---------------------------------------------------
; parameters: page   :  eax  ( as it is unsigned, also ax)
;---------------------------------------------------------------------------

PROC      setvpage_
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
;                    is scan-converted with 32-bit fixed-point arithmetic.   |
;                    The only thing that has to be rewritten is the HorLine  |
;                    macro.                                                  |
;----------------------------------------------------------------------------|
; prototype:  void fpoly( int nrvertices, void *pvertices, int color)        |
;----------------------------------------------------------------------------|
; Watcom passes the parameters as follows:                                   |
; nrvertices: eax                                                            |
; pvertices : [edx]                                                          |
; color     : ebx                                                            |
;----------------------------------------------------------------------------|

  ;--------------------------------------------------------------------------|
  ; MACRO LinFit: This procedure calculates a linear equation out of two of  |
  ;               it's solutions and returns the coefficients of it.         |
  ;--------------------------------------------------------------------------|
  ; Input:  EAX= X1, EBX=Y1, ECX=X2, EDX=Y2                                  |
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
            push  edx

            sub   ebx, edx
            sub   eax, ecx
            sal   eax, 12
            cdq
            idiv  ebx

            pop   edx
            imul  edx, eax
            sal   ecx, 12
            sub   ecx, edx
  ENDM      LinFit

  ;--------------------------------------------------------------------------|
  ;  MACRO HorLine:  This macro draws a horizontal line between two points   |
  ;                                                                          |
  ;--------------------------------------------------------------------------|
  ;  Input: The Y-coordinat is passed in edx (integer), the two x-es are     |
  ;         passed in eax and ebx.                                           |
  ;--------------------------------------------------------------------------|
  ;  Output: -                                                               |
  ;--------------------------------------------------------------------------|

  MACRO     HLine3
            push  edx edi

            cmp   eax, ebx
            jl    @@dont_swap
            xchg  eax, ebx
  @@dont_swap:

            cmp   eax, 0
            jl    @@hl1

;----The left x > 0.

            cmp   eax, 320
            jge   @@exit
            cmp   ebx, 320
            jl    @@clip_end
            mov   ebx, 319
            jmp   @@clip_end

;----The left x < 0.

  @@hl1:    cmp   ebx, 0
            jl    @@exit
            cmp   ebx, 320
            jl    @@hl2
            xor   eax, eax
            mov   ebx, 319
            jmp   @@clip_end

  @@hl2:    xor   eax, eax
            jmp   @@clip_end

  @@clip_end:

;----Set up a pointer in edi to the first pixel in the line.

            xor   edi, edi
            mov   di, dx               ; di = dx = Y
            shl   edi, 4
            mov   ecx, edi
            shl   edi, 2
            add   edi, ecx             ; edi = 80 * Y
            mov   ecx, eax
            shr   ecx, 2
            add   edi, ecx             ; edi = 80*y + (x/4)
            add   edi, SCREEN_LIN_ADDRESS

;----Check if the line is bigger than 4 pixels.

            sub   bx, ax
            inc   bx                   ; dx = number of pixels to write.
            cmp   bx, 4h
            ja    @@bigline

;----The line consists of less than 3 pixels. We write them one at a time,
;----for such a small line it isn't worth redering them more at a time.

            mov   cx, ax               ; cx = left x.
            and   cx, 3h               ; |
            mov   ah, 1h               ; |
            shl   ah, cl               ; | Select the right plane
            mov   al, 2h               ; |
            mov   dx, SequIndex        ; |
            out   dx, ax               ; |
            mov   cx, [Color]          ; Yep, color is stored in cx.
            jmp   @@wp3                ; Jump into the loop, after the part
                                       ;   where a new plane is selected.
  @@wpix:   shl   ah, 1                ; Next plane.
            and   ah, 0fh              ; Test if we have crossed a byte-boun
            jnz   @@wp2                ;   dary
            mov   ah, 1h               ; Yes: Select the first plane of the
            inc   edi                  ;   next byte in VGA-mem.
  @@wp2:    out   dx, ax               ; Select the plane.

  @@wp3:    mov   [edi], cl            ; Store the color in VGA-mem.
            dec   bx                   ; Are we done yet?
            jnz   @@wpix               ; No, then loop another time.
            jmp   @@exit               ; Yes, exit.


;----The line consists of more than 4 pixels. So a byte boundary in VGA-mem
;----will be crossed. We write pixels until we arrive at that byte-boundary.
;----Then we select ALL the planes and write 4 (or 16) pixels at a time

  @@bigline:not   ax
            inc   ax
            and   ax, 3h               ; ax = (4 - ax&3h) & 3h). This is the
                                       ; number of px to write untill the
                                       ; next byte boundary in VGA-mem.
            jz    @@hl3                ; If 0, skip the stuff below.
            sub   bx, ax               ; Substract these of the ones that last.

            mov   ah, 0f0h
            mov   cx, ax
            shr   ah, cl
            and   ah, 0fh
            mov   al, 02h              ; sequencer function 02h:
            mov   dx, SequIndex        ; "select write plane"
            out   dx, ax
            mov   ax, [Color]
            mov   [byte ptr edi], al   ; and store the pixs.
            inc   edi

;----Now we must determine how much pixels, 4-pixels (bytes) and 16-pixels
;----(dwords) we have to write.

  @@hl3:    mov   dx, bx
            and   dx, 3h
            push  dx                   ; dx = number of pixels to write after
                                       ; bytes 'n dword are written.
            shr   bx, 2
            mov   dx, bx
            and   dx, 3h
            push  dx                   ; dx = number of bytes to write after
                                       ; all dword are written.
            shr   bx, 2
            mov   cx, bx               ; High word of ecx is still zero.

            mov   ax, 0f02h
            mov   dx, SequIndex
            out   dx, ax

;----Store the dwords in VGA-mem, this renders 16 pixels at a time.

            mov   ax, [Color]
            rol   eax, 16
            mov   ax, [Color]
            cld
            rep   stosd

;----Store the words in VGA-mem, this renders 4 pixels at a time.

            pop   cx
            rep   stosb

;----Write the pixels that last (0<=pxls<=3)

            pop   cx                   ; cx = pixels that last.
            mov   ah, 0f0h             ; rotate those planes into ah
            rol   ah, cl               ;  that should be written to.
            and   ah, 0fh              ; Mask out the ones not used.
            mov   al, 02h              ; Index 02h of the sequencer:
            out   dx, ax               ;  "select write plane".
            mov   ax, [Color]
            mov   [byte ptr edi], al

;----Exit, the line is drawn.

  @@exit:   pop   edi edx
  ENDM      HLine3


%NEWPAGE
PROC      fpoly_  NEAR
          LOCAL     B1: DWORD, B2: DWORD, MaxY1: DWORD, MaxY2: DWORD,\
                    PVertices: DWORD, Vert1: DWORD, Vert2: DWORD,\
                    Smallest: DWORD, Biggest: DWORD, NrVertices: DWORD,\
                    Done: BYTE, LastVertix1: BYTE, LastVertix2: BYTE,\
                    Color: WORD =StackSize

          push  ebp
          mov   ebp, esp
          sub   esp, StackSize

          push  ecx esi edi

          mov   bh, bl
          mov   [Color], bx

          mov   [PVertices], edx
          mov   edi, edx                ; [edi] = Vertices

          mov   esi, eax                ; si = nrvertices
          mov   [NrVertices], eax
          dec   esi
          mov   edx, [edi + esi*8 +  4] ; Assume the last Y-coordinat is the
          mov   ebx, edx                ; smallest and the biggest for now.
          mov   [Smallest], esi         ; The smallest value is stored in edx, the
          mov   [Biggest], esi          ; biggest in ebx.

;-------Loop all Y-coordinats to find the smallest and the biggest.

loop1:    dec   esi
          mov   eax, [edi + esi*8 + 4]
          cmp   eax, edx
          jge   fp1
          mov   edx, eax
          mov   [Smallest], esi
fp1:      cmp   eax, ebx
          jle   fp2
          mov   ebx, eax
          mov   [Biggest], esi
fp2:      test  esi, esi
          jnz   loop1

;--Initialise edx(=y), MaxY1 and MaxY2.

          mov   [LastVertix1], False
          mov   [LastVertix2], False
          mov   [Done], False

          mov   ebx, [Smallest]
          mov   [Vert1], ebx
          mov   [Vert2], ebx
          mov   edx, [edi + ebx*8 + 4]   ; edx = y
          mov   [MaxY1], edx
          mov   [MaxY2], edx
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
next_vertix1:
          cmp   [LastVertix1], True
          jne   fp3
          mov   [Done], True
          jmp   fp5
fp3:
          mov   edi, [PVertices]
          mov   ebx, [Vert1]                ; Which is the vertix we reached?
          mov   ecx, [edi + ebx*8]          ; Store X1 and Y1 for the linfit
          mov   edx, [edi + ebx*8 + 4]      ; macro.
          inc   ebx                         ; Go to the next vertix.

          cmp   ebx, [NrVertices]           ; Walk circular through the list
          jl    fp4
          sub   ebx, [NrVertices]           ; "
fp4:      mov   [Vert1], ebx

          cmp   ebx, [Biggest]              ; Have we reached the last vertix
          jne   not_at_end1                 ; yet?
          mov   [LastVertix1], True
not_at_end1:
          mov   eax, [edi + ebx*8]
          mov   ebx, [edi + ebx*8 + 4]
          mov   [MaxY1], ebx                ; Store X2 and Y2 for the LinFit
                                            ; macro.
          cmp   ebx, edx
          je    next_vertix1           ; Check if the line is horizontal.
                                       ; If it is: skip this vertix and pro-
                                       ; ceed with the next one. We will
                                       ; 'catch' this also with the next
                                       ; horizontal line.
          LinFit
          mov   esi, eax
          mov   [B1], ecx

fp5:      pop   edi
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
next_vertix2:
          cmp   [LastVertix2], True
          je    fp7

          mov   esi, [PVertices]
          mov   ebx, [Vert2]                ; Which is the vertix we reached?
          mov   ecx, [esi + ebx*8]          ; Store X1 and Y1 for the linfit
          mov   edx, [esi + ebx*8 + 4]      ; macro.
          dec   ebx                         ; Go to the next vertix.

          cmp   ebx, 0                      ; Walk circular through the list
          jge   fp6
          add   ebx, [NrVertices]           ; "
fp6:      mov   [Vert2], ebx

          cmp   ebx, [Biggest]              ; Have we reached the last vertix
          jne   not_at_end2                 ; yet?
          mov   [LastVertix2], True
not_at_end2:
          mov   eax, [esi + ebx*8]
          mov   ebx, [esi + ebx*8 + 4]
          mov   [MaxY2], ebx                ; mov   eax, [esi + ebx*8]          ; Store X2 and Y2 for the LinFit
                                            ; macro.
          cmp   ebx, edx
          je    next_vertix2

          LinFit
          mov   edi, eax
          mov   [B2], ecx

fp7:      pop   esi
          pop   edx

getx2:    mov   eax, edx               ; eax = y (integer)
          imul  eax, edi               ; eax = a2*y (fixed)
          add   eax, [B2]              ; eax = a2*y + b2 (fixed}
          pop   ebx                    ; ebx = other x

          sar   eax, 12
          sar   ebx, 12

          HLine3                       ; draw a horizontal line, y: edx,
                                       ; x-es: eax, ebx
          cmp   [Done], True
          jne   NextScanLine

;--The polygon is filled: Free used stack-space and return to the caller

          pop   edi esi ecx
          mov   esp, ebp
          pop   ebp
          ret
ENDP      fpoly_


;----------------------------------------------------------------------------|
; Procedure PutPixel: Puts a pixel on the screen at ( X, Y) in the color     |
;                     <Color>.                                               |
;----------------------------------------------------------------------------|
; prototype: void putpixel( int x, int y, int color);                        |
;----------------------------------------------------------------------------|
; parameters: x     : eax                                                    |
;             y     : edx                                                    |
;             color : ebx                                                    |
;----------------------------------------------------------------------------|

PROC      putpixel_ NEAR

          push  ecx edi

          cmp   eax, 320
          jae   @@Done

          cmp   edx, 240
          jae   @@Done

          shl   edx, 4     ; edx = y
          mov   ecx, edx
          shl   edx, 2
          add   edx, ecx   ; edx = 80*y

          mov   edi, eax   ; di = x
          shr   edi, 2     ; di = x/4
          add   edi, edx   ; di = 80*y + x/4
          add   edi, [ActiveStart] ; Add _LINEAR_ address of active page.

          mov   cx, ax
          and   cx, 3h
          mov   ah, 1
          shl   ah, cl
          mov   al, 02h
          mov   dx, 03c4h
          out   dx, ax
          mov   [edi], bl
@@Done:
          pop   edi ecx
          ret
ENDP      putpixel_

;---------------------------------------------------------------------------|
; Procedure hblit_: This procedure copies one horizontal line from a buffer |
;                   to the VGA memory in mode-x (In mode 13h it would be    |
;                   ONE movsd:) )                                           |
;---------------------------------------------------------------------------|
; prototype:  void hblit ( void *LineBuf, int y);                           |
;---------------------------------------------------------------------------|
; parameters: eax -> LineBuf                                                |
;             edx =  y                                                      |
;---------------------------------------------------------------------------|

PROC hblit_
          push  ebx ecx esi edi

          mov   esi, eax               ; esi -> LBuf

          mov   edi, edx
          shl   edi, 4
          mov   edx, edi
          shl   edi, 2
          add   edi, edx               ; edi = 80*y
          add   edi, [ActiveStart]     ; add _LINEAR_ address of active page.

          mov   dx, SequIndex          ; Sequencer index.

;--Copy the bytes at index 4m.

          mov   ax, 0102h              ; select plane 1 with function 02h
          out   dx, ax                 ;  of the sequencer.
          mov   ebx, 0
@@l1:     mov   cl, [esi + ebx*8]
          mov   ch, [esi + ebx*8 + 4]
          mov   [edi + ebx*2], cx
          inc   ebx
          cmp   ebx, 40
          jne   @@l1

;--Copy the bytes at index 4m + 1

          shl   ah, 1
          out   dx, ax
          mov   ebx, 0
@@l2:     mov   cl, [esi + ebx*8 + 1]
          mov   ch, [esi + ebx*8 + 4 + 1]
          mov   [edi + ebx*2], cx
          inc   ebx
          cmp   ebx, 40
          jne   @@l2

;--Copy the bytes at index 4m + 2

          shl   ah, 1
          out   dx, ax
          mov   ebx, 0
@@l3:     mov   cl, [esi + ebx*8 + 2]
          mov   ch, [esi + ebx*8 + 4 + 2]
          mov   [edi + ebx*2], cx
          inc   ebx
          cmp   ebx, 40
          jne   @@l3

;--Copy the bytes at index 4m + 3

          shl   ah, 1
          out   dx, ax
          mov   ebx, 0
@@l4:     mov   cl, [esi + ebx*8 + 3]
          mov   ch, [esi + ebx*8 + 4 + 3]
          mov   [edi + ebx*2], cx
          inc   ebx
          cmp   ebx, 40
          jne   @@l4

;--All bytes are copied, clear the mess and return.

          pop   edi esi ecx ebx
          ret
ENDP hblit_


;----------------------------------------------------------------------------|
; Procedure ClearScreen: Fills the entire VGA DMA area with zeroes.          |
;----------------------------------------------------------------------------|
; Input:  -                                                                  |
;----------------------------------------------------------------------------|
; Output: -                                                                  |
;----------------------------------------------------------------------------|
PROC      clrscr_ NEAR
          push  eax ecx edx edi

          mov   dx, 03c4h
          mov   ax, 0f02h
          out   dx, ax        ; Select all planes

          mov   edi, [ActiveStart]
          xor   eax, eax        ; zero eax, we want to copy 0's.
          mov   ecx, 12c0h      ; cx = nr of dwords that fit on the screen.
          cld
          rep   stosd

          pop   edi edx ecx eax
          ret
ENDP      clrscr_

;----------------------------------------------------------------------------|
; Procedure SetPalette: Sets the palette's DAC-registers. !!!!!!             |
;----------------------------------------------------------------------------|
; prototype: void setpal( int firstdac, int nrdacs, void *pal);              |
;----------------------------------------------------------------------------|
; Parameters: eax = FirstDAC                                                 |
;             edx = NrDACs                                                   |
;             [ebx] = Palette                                                |
;----------------------------------------------------------------------------|

PROC      setpal_ NEAR
          push  ecx esi      ; Push all registers that aren't used to pass
                             ; parameters (requirement from watcom).
          mov   esi, ebx     ; esi now points to palette.

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

@@done:   pop   esi ecx
          ret
ENDP      setpal_

;----------------------------------------------------------------------------|
; Procedure GetPalette: Gets the palette's DAC-registers.                    |
;----------------------------------------------------------------------------|
; prototype: void getpal( int firstdac, int nrdacs, void *pal);              |
;----------------------------------------------------------------------------|
; Parameters: eax = FirstDAC                   !!!!!!                        |
;             edx = NrDACs                                                   |
;             [ebx] = Palette                                                |
;----------------------------------------------------------------------------|
PROC      getpal_  NEAR

          push  ecx edi    ; Push all registers that AREN'T used to pass a
                           ; parameter (requirement from watcom).
          mov   edi, ebx   ; edi now points to palette.

          test  ah, ah
          jnz   @@done

          mov   ecx, edx   ; ecx = NrDACs
          jcxz  @@done

          mov   ebx, ecx
          add   ebx, ecx
          add   ecx, ebx   ; ecx = 3*NrDACs

          mov   dx, DacRead
          out   dx, al
          mov   dx, DacData
          cld
          rep   insb

@@done:   pop   edi ecx
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
; Parameters: [eax] = topal                                                  |
;----------------------------------------------------------------------------|
PROC      fadepal_ NEAR
          LOCAL FadeTab: WORD:768, CurPal: WORD:768 = StackSize

          push  ebp
          mov   ebp, esp
          sub   esp, StackSize

          push  ebx ecx edx esi edi

          mov   ebx, eax       ; ebx points to <topal>

          mov   dx, DacRead
          xor   ax, ax
          out   dx, al

          mov   dx, DacData
          xor   esi, esi

RPal:     in    al, dx
          xor   ah, ah

          xor   ch, ch
          mov   cl, [ebx + esi]       ; cl = Topal[ si]
          sub   cx, ax
          sal   cx, 2   ; cx = (Topal[ si] - CurPal[ si]) /64 in 16-bit
                        ; 8:8 fixed point.
          mov   [word ptr FadeTab + esi*2], cx
          shl   ax, 8                          ; Store the current value in
          mov   [word ptr CurPal + esi*2], ax  ; 16-bit 8:8 fixed point.

          inc   esi
          cmp   esi, 768               ; Have we read the entire palette yet?
          jne   RPal

          mov   ecx, 64                 ; We fade in 64 steps.
WPal:     call  NEAR waitvrt_
          xor   ax, ax
          mov   dx, DacWrite
          out   dx, al

          mov   dx, DacData
          xor   edi, edi

WPal1:    mov   ax, [word ptr CurPal + edi*2]
          mov   bx, [word ptr FadeTab + edi*2]
          add   ax, bx
          mov   [word ptr CurPal + edi*2], ax
          shr   ax, 8
          out   dx, al
          inc   edi
          cmp   edi, 768
          jne   WPal1

          loop  WPal

          pop   edi esi edx ecx ebx

          mov   esp, ebp
          pop   ebp
          ret
ENDP      fadepal_

          END
