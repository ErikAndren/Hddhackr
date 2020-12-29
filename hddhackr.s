;

[ORG 0x100]

   MOV AX, DS
   MOV ES, AX   ; let ES = DS


   mov ax, 3
   int 10h      ;clear screen

   mov     ah, 9
   mov     dx, introtext ; show introtext
   int     0x21

   mov si,80h
   cmp byte [si], 3   ;size => 3 = space + 2 chars
   jne optionerror

   inc si
   inc si

   cmp word [si], '-u'
   jne noU
   mov byte [option], 1   ;-u = 1 = undo
   jmp Optiongood
noU:
   cmp word [si], '-d'
   jne noD
   mov byte [option], 2   ;-d = 2 = dump
   jmp Optiongood


noD:
   cmp word [si], '-f'
   jne optionerror
   mov byte [option], 3   ;-f = 3 = file (flash from file)
   jmp Optiongood

optionerror:
   mov     ah, 9
   mov     dx, option_error
   int     0x21
   jmp exit

Optiongood:


   cmp word [mprt], 'MP'
   je normalop

   mov     ah, 9
   mov     dx, manualmode ; show introtext
   int     0x21

   mov byte [pcount], 2

        mov  bx, portlist
        mov ax, word [mprt]
   mov byte [bx],byte ah
   mov byte [bx+1],byte al
        mov ax, word [mprt+2]
   mov byte [bx+2],byte ah
   mov byte [bx+3],byte al

   jmp contop

normalop:
   mov     ah, 9
   mov     dx, detecting
   int     0x21
   call Findports
contop:


   CLI      ;disable interupts

mainlus:

        nop
        nop
        nop
        nop
        nop
        nop

   mov bx, portlist
   add bx, word [testcounter]   ;aantal ports dat ie al heeft getest
   add bx, word [testcounter]   ;2x, omdat het om words gaat
   mov dx, [bx]
   mov word [basereg], dx

   MOV DX, word [basereg];device/head register
   add dx, 6
   MOV AL, 0xA0 ; select device 0 (master)
   OUT DX, AL

   MOV DX, word [basereg] ;command register
   add dx, 7
   MOV AL, 0ECh ;"IDENTIFY DRIVE" command
   OUT DX, AL ;sends the command!

   call Wait_For_NBSY2
   cmp ax, 1
   je ietsgevonden

   ;now try same again, but for slave mode.

   mov bx, portlist
   add bx, word [testcounter]   ;aantal ports dat ie al heeft getest
   add bx, word [testcounter]   ;2x, omdat het om words gaat
   mov dx, [bx]
   mov word [basereg], dx

   MOV DX, word [basereg];device/head register
   add dx, 6
   MOV AL, 0xB0 ; select device 0 (slave)
   OUT DX, AL

   MOV DX, word [basereg] ;command register
   add dx, 7
   MOV AL, 0ECh ;"IDENTIFY DRIVE" command
   OUT DX, AL ;sends the command!

   call Wait_For_NBSY2
   cmp ax, 1
   je slavefound
   jmp searchndrv

slavefound:

   mov byte [head], 0xB0   ;set head to slave

ietsgevonden:

   mov dx, word [basereg]
   add dx, 7
   in al, dx
        cmp al, 58h
   jz gevonden

        jmp searchndrv


gevonden:

   mov     ah, 9
   mov     dx, devicefoundtext ; show introtext
   int     0x21



gevondenx:

;----------------------------------------------------------------------------
   call processEC   ;read EC reply and copy the relevant data to vars


;----------------------------------------------------------------------------------------

      ;safe EC buffer
   mov     bx, buff   ;dx = pointer to buffer (ds:dx)
   mov     dx, ecFilename
   mov     cx, 512
   call SaveFile


   cmp byte [option], 1
   je startundo
   jmp notoption1

startundo:

            ;User wants to flash undo.bin, so let's do that
   mov     ah, 9
   mov     dx, ECserialstring
   int     0x21
   mov     ah, 9
   mov     dx, ECModelstring
   int     0x21
   mov     ah, 9
   mov     dx, ECBiosstring
   int     0x21

   mov     ah, 9
   mov     dx, suretoundomsg
   int     0x21

       mov     ah,0
       int     16h         ;get a key, returned in AX
                        ;AL is the ASCII part
                        ;AH is the SCAN CODE
       push    ax
       mov     dl,al
       mov     ah,2
       int     21h         ;print character in dl
       pop     ax

       cmp     al,"Y"      ;was the character a 'Y'?
       je     Yesx2      ;nope it was Not Equal
       cmp     al,"y"      ;was the character a 'Y'?
       je     Yesx2      ;nope it was Not Equal

       mov     dx, NoyesMessage
       mov     ah,9
       int     21h
       jmp     exit

Yesx2:

   call readundofile

   cmp word [secDbuff], 'RO'
   je secDok

invalidFile:

       mov     dx, invalidFileErr
       mov     ah,9
       int     21h
   jmp exit


secDok:
   cmp word [sec2buff], 'RO'
   je sec2ok
   jmp invalidFile

sec2ok:
   call SetNativeStatus
   jmp FlashFW
;--------------------------------------------------

notoption1:

   cmp byte [option], 2
   je dump_sec16_22
   jmp Startflashproc




;---------------------------------------------------------------------------------------------
         ;User selected to dump the sectors 16 to 22, so let's do that.
dump_sec16_22:

        call readsec16_22
   call verify16_22

   cmp byte [dataokbool], 1
   je savetobin

   mov     ah, 9
   mov     dx, dataerr
   int     0x21
   jmp exit

savetobin:

   mov     bx, sec16
   mov     dx, hddssFilename
   mov     cx, 3584
   call SaveFile         ;save modified modID 2e, for checking


   mov     ah, 9
   mov     dx, donesaving
   int     0x21
   jmp exit


;----------------------------------------------------------------------------------------------

Startflashproc:      ;Main procedure that reads the hddss.bin file, saves it to the sectors 16-22 and then flashes the FW

        call readsec16_22
   call verify16_22

   ;cmp byte [dataokbool], 1
   ;je dontwrite

   call readhdssfile   ;read the hddss.bin
   call verify16_22
   cmp byte [dataokbool], 1
   je datagood

   mov     ah, 9
   mov     dx, datafileerr
   int     0x21
   jmp exit

datagood:

   call writesectors
   mov     ah, 9
   mov     dx, wrotedata
   int     0x21
   jmp cmp16vsEC

dontwrite:

   mov     ah, 9
   mov     dx, datawasok
   int     0x21



   call readhdssfile   ;read the hddss.bin
   call verify16_22

   cmp byte [dataokbool], 1
   je cmp16vsEC

   mov     ah, 9
   mov     dx, datafileerr
   int     0x21
   jmp exit


cmp16vsEC:
   mov bx, sec16   ;point to serial
   mov di, 0

copys16serial:
        mov ax, [bx]
        mov [s16serial+di], ax
   add bx, 2
   add di,2
   cmp di, 20
   jne copys16serial

   mov     ah, 9
   mov     dx, ECserialstring
   int     0x21

   mov     ah, 9
   mov     dx, s16serialstring
   int     0x21

;------------------------------------
; copy the s16 model to the var

   mov bx, sec16
   add bx, 28   ;point to model
   mov di, 0

copys16Model:
        mov ax, [bx]
        mov [s16Model+di], ax
   add bx, 2
   add di,2
   cmp di, 40
   jne copys16Model

   mov     ah, 9
   mov     dx, ECModelstring
   int     0x21

   mov     ah, 9
   mov     dx, s16Modelstring
   int     0x21


;------------------------------------
; copy the bios version to the var

   mov bx, sec16
   add bx, 20   ;point to bios
   mov di, 0

copys16Bios:
        mov ax, [bx]
        mov [s16Bios+di], ax
   add bx, 2
   add di,2
        cmp di, 8
   jne copys16Bios

   mov     ah, 9
   mov     dx, ECBiosstring
   int     0x21

   mov     ah, 9
   mov     dx, s16Biosstring
   int     0x21
;--------------------------------------------
; copy the sect count to the var

   mov bx, sec16
   add bx, 88   ;point to sector count
   mov di, 0

copys16scount:
        mov ax, [bx]
        mov [s16scount+di], ax
   add bx, 2
   add di,2
        cmp di, 4
   jne copys16scount





   MOV AX, DS
   MOV ES, AX                         ; Let ES equal DS

   MOV DI, s16serial
   MOV SI, ECserial
   CLD
   MOV CX, 20
   REPE CMPSB
   JNZ notequall

   MOV DI, s16Model
   MOV SI, ECModel
   CLD
   MOV CX, 40
   REPE CMPSB
   JNZ notequall

   MOV DI, s16Bios
        MOV SI, ECBios
   CLD
   MOV CX, 8
   REPE CMPSB
   JNZ notequall

   MOV DI, s16scount
   MOV SI, ECscount
   CLD
   MOV CX, 4
   REPE CMPSB
   JZ equallx

   MOV DI, s16scount
   MOV SI, ECscount2
   CLD
   MOV CX, 4
   REPE CMPSB
   JnZ notequall


equallx:
   mov     ah, 9
   mov     dx, equal
   int     0x21
   jmp exit

notequall:

   mov     ah, 9
   mov     dx, notequal
   int     0x21

    mov     ah,0
    int     16h         ;get a key, returned in AX
                        ;AL is the ASCII part
                        ;AH is the SCAN CODE
    push    ax
    mov     dl,al
    mov     ah,2
    int     21h         ;print character in dl
    pop     ax


    cmp     al,"Y"      ;was the character a 'Y'?
    je     Yes      ;nope it was Not Equal
    cmp     al,"y"      ;was the character a 'Y'?
    je     Yes      ;nope it was Not Equal



    mov     dx, NoyesMessage
    mov     ah,9
    int     21h
    jmp     exit



Yes:

   call SetNativeStatus


; Now read the FW Module D ***************************************************************************

   call   Wait_For_DRDY
   call    Wait_For_NBSY
   call   SEND_cfgsect_ATA   ;Tell drive that the Key sector is coming
   call    Wait_For_NBSY

   mov    DX, [basereg]
   add    DX, 7       ;status register
   in    al, dx
   cmp    al, 58h
   jz   DRQOK1
   jmp   DRQERR
DRQOK1:

   call   Wait_For_DRDY
   MOV    bx, SCT_Packet_rD
   call    send_Key_sector    ;send the key sector
   call    Wait_For_NBSY

   mov    DX, [basereg]
   add    DX, 7       ;status register
   in    al, dx

   call   Wait_For_DRDY
   mov     al, 0xD5       ;features, set to 'read'
   mov     Bl, 1          ;sector count
   call    send_Get_sector      ; Send request for transfer of data
   call    Wait_For_NBSY

   mov    DX, [basereg]
   add    DX, 7       ;status register
   in    al, dx
   cmp    al, 58h
   jz   DRQOK3
   jmp DRQERR
DRQOK3:


   mov   cx, 0
   mov    DI, secDbuff

LOOP3x:

   mov    DX, [basereg]
   add   dx, 7
   in      al,dx
   test    al,8            ;Wait for sector buffer ready.
   jz      LOOP3x
   mov    DX, [basereg]


   mov     cx,0x100       ;One sector /2

   mov     dx, [basereg]        ;Data port - data comes in and out of here.
      rep     insw

   call    Wait_For_NBSY

   mov    DX, [basereg]
   add    DX, 7       ;status register
   in    al, dx


        ;cmp     al, 58h
        ;jnz     DRQERR


;...And now we can display the contents of buff!

;   MOV BX,  secDbuff
;
;       MOV CX, 256
;   MOV AH, 2 ;"display output" option for INT 21
;LOOP4:
;   MOV DL, [BX] ;moves the contents of the byte from "buff" into DL
;   INT 21h
;   INC BX
;   LOOPNZ LOOP4 ;does this 256 times, because CX was set to 256



;-------------
; Now read the FW Module 2 ***************************************************************************

   call   Wait_For_DRDY
   call    Wait_For_NBSY
   call   SEND_cfgsect_ATA   ;Tell drive that the Key sector is coming
   call    Wait_For_NBSY

   mov    DX, [basereg]
   add    DX, 7       ;status register
   in    al, dx
   cmp    al, 58h
   jz   DRQOK2
   jmp   DRQERR
DRQOK2:

   call   Wait_For_DRDY
   MOV    bx, SCT_Packet_r2
   call    send_Key_sector    ;send the key sector
   call    Wait_For_NBSY

   mov    DX, [basereg]
   add    DX, 7       ;status register
   in    al, dx

   call   Wait_For_DRDY
   mov     al, 0xD5       ;features, set to 'read'
   mov     Bl, 2          ;sector count
   call    send_Get_sector      ; Send request for transfer of data
   call    Wait_For_NBSY

   mov    DX, [basereg]
   add    DX, 7       ;status register
   in    al, dx
   cmp    al, 58h
   jz   DRQOK5
   jmp   DRQERR
DRQOK5:

   mov   cx, 0
   mov    DI, sec2buff

LOOP3xx:

   mov    DX, [basereg]
   add   dx, 7
   in      al,dx
   test    al,8            ;Wait for sector buffer ready.
   jz      LOOP3xx
   mov    DX, [basereg]
   IN AX, DX
   stosw
   inc cx
   cmp cx, 0x200      ; read 2 sectors
   jnz LOOP3xx

   call    Wait_For_NBSY

   mov    DX, [basereg]
   add    DX, 7       ;status register
   in    al, dx


        ;cmp     al, 58h
        ;jnz     DRQERR


;...And now we can display the contents of buff!

;   MOV BX,  sec2buff

;        MOV CX, 512
;   MOV AH, 2 ;"display output" option for INT 21
;LOOP4x:
;   MOV DL, [BX] ;moves the contents of the byte from "buff" into DL
;   INT 21h
;   INC BX
;   LOOPNZ LOOP4x ;does this 256 times, because CX was set to 256



askquestion:               ; ASK if user wants to create an undo file

   mov     ah, 9
   mov     dx, undoquestion
   int     0x21

    mov     ah,0
    int     16h         ;get a key, returned in AX
                        ;AL is the ASCII part
                        ;AH is the SCAN CODE
    push    ax
    mov     dl,al
    mov     ah,2
    int     21h         ;print character in dl
    pop     ax


    cmp     al,"Y"      ;was the character a 'Y'?
    je     Yesx      ;nope it was Not Equal
    cmp     al,"y"      ;was the character a 'Y'?
    je     Yesx      ;nope it was Not Equal

    cmp     al,"n"      ;was the character a 'Y'?
    je     Nox      ;nope it was Not Equal
    cmp     al,"N"      ;was the character a 'Y'?
    je     Nox      ;nope it was Not Equal

    mov     dx, InvalidMessage
    mov     ah,9
    int     21h
    jmp     askquestion

Yesx:

   mov dx,undoFilename    ; put address of filename in dx
   mov al,0       ; access mode - read only
   mov ah,3Dh       ; function 3Dh -open a file
   int 21h       ; call DOS service

   jc createit       ; if error opening, then it is ok and we can create it

       mov     dx, Fileexistmsg
       mov     ah,9
       int     21h
   jmp exit


createit:

   mov     bx, secDbuff   ;dx = pointer to buffer (ds:dx)
   mov     dx, undoFilename
   mov     cx, 1536
   call SaveFile         ;save modified modID 2e, for checking


Nox:



;**************************************************************************************************************************
; Now copy the vars from the FW


; First see if the EC string can be found in sector 2. The FW code adds spaces to the BEGINNING. so remove these first:

   mov di, 0

removespaces:
   mov bx, ECserial
   mov al, [bx+di]
   cmp al, 20h
   jnz startserzooi
   inc di
   jmp removespaces

startserzooi

        mov dx, di                      ; save number of spaces to dx
   mov word [serialpos], 0
        mov bx, 0

seriallus:

   MOV DI, sec2buff
   add di, bx
   MOV SI, ECserial
        add si, dx
   CLD
        MOV CX, 7            ; => compare only first 7 bytes, it's possible that the string in FW is shorter than EC string
   REPE CMPSB
   jnz serialnotfoundyet
   mov word [serialpos], bx
   mov bx, 1003 ; EXIT AFTER THE FIRST MATCH !!!

serialnotfoundyet:
   add bx, 1
   cmp bx, 1004   ;1024-length of string
   jne seriallus

   cmp word [serialpos], 0
   jne serialfound

   mov     ah, 9
   mov     dx, serialerrorstring
   int     0x21
   jmp    exit

serialfound:

;Now make sure that there is space in FW for the string !

;algo: search for first 0 byte, then look for the first byte after his one that is NOT zero. Max length is one less than this.


   mov bx, sec2buff      ; so search for first 0 byte in FW serial string ...
   add bx, word [serialpos]
   mov word [serialzero], 0
   mov cx,0

serspace:
   mov bx, sec2buff
   add bx, word [serialpos]
   add bx, cx
   mov al, [bx]
   cmp al, 0
   jne nozero

   mov word [serialzero],cx
        mov cx, 20    ; EXIT AFTER FIRST MATCH !!!
nozero:
   add cx, 1
        cmp cx, 21
   jne serspace

               ;So, we found the first 0 byte, now look for the first byte that is NOT zero
testzero:
   mov bx, sec2buff
   add bx, word [serialpos]
   add bx, word [serialzero]
   mov al, [bx]

   cmp al, 0
   jne nozero2

   inc word [serialzero]
   jmp testzero

nozero2:            ; now max size of serial is [serialzero] - 1

               ; so now calculate how long much space we need, thus see
               ; how long the serial from sec16 EX spaces in the end is ...
               ; algo: look at FIRST character of the string, check for a 'space', if so,
               ; check next. Needed size is 20 - number of spaces



   mov cx, 0
serspac:

   cmp cx, 20
   jne nok
   jmpFatalerrorstring      ; Source contains ONLY spaces, this aint good !!
nok:
   mov bx, s16serial
   add bx, cx
   mov al, [bx]
   inc cx
   cmp al, 20h   ; check for a 'space' character
   je serspac

sourcechecked:            ;Now check if there is enough space for the serial
   mov ax, word [serialzero]
   ;sub ax, 1         ;ax now contains the free length in FW
   mov dx, 20
   sub cx, 1         ;cx was one too high, so substract it
   sub dx, cx         ;dx now contains the length of the source
   cmp ax, dx         ;dx now contains the needed length
   JA serialisok

serrerr
        mov     ah, 9              ;if not, display error and exit
   mov     dx, noserspacestring
   int     0x21
   jmp exit


serialisok:            ; Now copy the serial from sec16 into the FW.

   cmp dx, 21
   JB sizeisok

fataler:
   mov     ah, 9
   mov     dx, Fatalerrorstring  ; If it wants to copy more than 20 bytes, than exit this stuff !!
   int     0x21
   jmp    exit
               ;Now we're going to copy the serial from sec 16 EX the spaces !
               ;cx still contains the number of spaces !!

sizeisok:



   mov di, 0

copysec2serial:

   mov bx, s16serial
   add bx, cx         ;add number of spaces, we dont want them copied !
        mov al, [bx+di]

   mov bx, sec2buff
   add bx, word [serialpos]
        mov byte [bx+di], al

   inc di
   cmp di, dx      ;dx still contains the length
   jne copysec2serial

            ;now zero the rest of the OLD string, if this was longer !
            ;so check if next byte is 0, if not, zero it !!
zeroit:
   mov bx, sec2buff
   add bx, word [serialpos]
        mov al, [bx+di]
   cmp al, 0
   je domodel
   mov byte [bx+di], 0
   inc di
   jmp zeroit




;NOW DO THE MODEL *******************************************************************

domodel:

   mov word [modelpos], 0
   mov bx, 0

modellus:
   MOV DI, sec2buff
   add di, bx
   MOV SI, ECModel
   CLD
        MOV CX, 10   ; => compare only first 10 bytes,possible that the string in FW is shorter than EC string
   REPE CMPSB
   jnz modelnotfoundyet
   mov word [modelpos], bx
   mov bx, 1003 ; EXIT AFTER THE FIRST MATCH !!!

modelnotfoundyet:
   add bx, 1
   cmp bx, 1004   ;1024-length of string
   jne modellus

   cmp word [modelpos], 0
   jne modelfound

   mov     ah, 9
   mov     dx, modelerrorstring
   int     0x21
   jmp    exit

modelfound:

      ; Check length of Modelstring from sec 16. Start at byte 40, look for first character that is NOT a space.
      ; Then check length of space in FW: Look for first zero byte and then for the first next NON zero byte after this one.

      ;so first check length of source string => find last byte that is NOT a space

   mov cx, 40
modspac:
   dec cx
   cmp cx, 0
   jne modn
   jmp modspacerr         ; Source contains NO spaces, this ain't never gonna fit
modn:
   mov bx, s16Model
   add bx, cx
   mov al, [bx]
   cmp al, 20h   ; check for a 'space' character
        je modspac

   add cx, 1
   mov word [lengths16modstring], cx   ;save length of source string

      ; now calculate available space
      ;algo: search for first 0 byte, then look for the first byte after his one that is NOT zero. Max length is one less than this.

   mov bx, sec2buff      ; so search for first 0 byte in FW model string ...
   add bx, word [modelpos]
   mov word [modelzero], 0
   mov cx,0

serspacee:
   mov bx, sec2buff
   add bx, word [modelpos]
   add bx, cx
   mov al, [bx]
   cmp al, 0
   jne nozeroo

   mov word [modelzero],cx
        mov cx, 40    ; EXIT AFTER FIRST MATCH !!!
nozeroo:
   add cx, 1
        cmp cx, 41
   jne serspacee

               ;So, we found the first 0 byte, now look for the first byte that is NOT zero
testzeroo:
   mov bx, sec2buff
   add bx, word [modelpos]
   add bx, word [modelzero]
   mov al, [bx]

   cmp al, 0
   jne nozeroo2x

   inc word [modelzero]
   jmp testzeroo

nozeroo2x:               ; now max size of model is [modelzero] - 1
                  ; so compare availabe size to needed size now ...
   mov ax, word [modelzero]
   sub ax, 1            ;ax contains available size
   mov bx, word [lengths16modstring]   ;bx contains needed size
   cmp ax, bx
   JA copymodeltoFW

   mov     ah, 9
   mov     dx, nomodspacestring
   int     0x21
   jmp    exit

copymodeltoFW:            ;now finally copy the string into the FW

   mov di, 0
        mov dx, word [lengths16modstring]

copysec2model:

   mov bx, s16Model
        mov al, [bx+di]

   mov bx, sec2buff
   add bx, word [modelpos]
        mov byte [bx+di], al

   inc di
   cmp di, dx      ;dx still contains the length
   jne copysec2model

            ;now zero the rest of the OLD string, if this was longer !
            ;so check if next byte is 0, if not, zero it !!
zeroitt:
   mov bx, sec2buff
   add bx, word [modelpos]
        mov al, [bx+di]
   cmp al, 0
        je dobios
   mov byte [bx+di], 0
   inc di
   jmp zeroitt



; NOW DO THE BIOS *********************************************************************
; check availabe space and assume we NEED 8 bytes available !


dobios:
   mov word [Biospos], 0
   mov bx, 0

Bioslus:
   MOV DI, secDbuff
   add di, bx
   MOV SI, ECBios
   CLD
        MOV CX, 6   ; => compare only first 6 bytes of Bios string
   REPE CMPSB
   jnz Biosnotfoundyet
   mov word [Biospos], bx
   mov bx, 1003 ; EXIT AFTER THE FIRST MATCH !!!

Biosnotfoundyet:
   add bx, 1
   cmp bx, 1004   ;1024-length of string
   jne Bioslus

   cmp word [Biospos], 0
   jne Biosfound

   mov     ah, 9
   mov     dx, Bioserrorstring
   int     0x21
   jmp    exit

Biosfound:            ;See if we have enough space to store 8 bytes ...

   mov bx, secDbuff      ; so search for first 0 byte in FW Bios string ...
   add bx, word [Biospos]
   mov word [Bioszero], 0
   mov cx,0

serspaceex:
   mov bx, secDbuff
   add bx, word [Biospos]
   add bx, cx
   mov al, [bx]
   cmp al, 0
   jne nonzeroox

   mov word [Bioszero],cx
        mov cx, 8   ; EXIT AFTER FIRST MATCH !!!
nonzeroox:
   add cx, 1
        cmp cx, 9
   jne serspaceex

               ;So, we found the first 0 byte, now look for the first byte that is NOT zero
testzeroox:
   mov bx, secDbuff
   add bx, word [Biospos]
   add bx, word [Bioszero]
   mov al, [bx]

   cmp al, 0
   jne nonzeroox2x

   inc word [Bioszero]
   jmp testzeroox

nonzeroox2x:
   mov ax, word [Bioszero]
   ;sub ax, 1            ;ax contains available size
   mov bx, 8            ; want 8 available places
   cmp ax, bx
   JA copyBiostoFW

   mov     ah, 9
   mov     dx, nobiosspacestring
   int     0x21
   jmp    exit

copyBiostoFW:


   mov di, 0
        mov dx, 8       ;copy 8 bytes

copysecDBios:

   mov bx, s16Bios
        mov al, [bx+di]

   mov bx, secDbuff
   add bx, word [Biospos]
        mov byte [bx+di], al

   inc di
   cmp di, dx      ;dx still contains the length
   jne copysecDBios

            ;now zero the rest of the OLD string, if this was longer !
            ;so check if next byte is 0, if not, zero it !!
zeroitt2:
   mov bx, secDbuff
   add bx, word [Biospos]
        mov al, [bx+di]
   cmp al, 0
        je finishedcopy
   mov byte [bx+di], 0
   inc di
   jmp zeroitt2



finishedcopy:



; AND NOW DO THE SECTOR COUNT, FIRST OCCURENCE ********************************************************

;first create the search string, by lowering the ECcount by one and copying this 4 times



createscountstring:

   mov bx, ECscount

start2:
        mov ax, [bx]
   dec ax            ;lower the sector count searchstring with one !!
        mov [scountsearch], ax
   add bx, 2
        mov ax, [bx]
        mov [scountsearch+2], ax

   mov word [Scountpos], 0
   mov bx, 0
scountlus2:
   MOV DI, sec2buff
   add di, bx
        MOV SI, scountsearch
   CLD
        MOV CX, 4            ; now only search for 1 time the sector count
   REPE CMPSB
   jnz Scountnotfoundyet2
   mov word [Scountpos], bx
   mov bx, 1003 ; EXIT AFTER THE FIRST MATCH !!!

Scountnotfoundyet2:
   add bx, 1
   cmp bx, 1004   ;1024-length of string
   jne scountlus2

   cmp word [Scountpos], 0
   je Scountnotfound      ; If this didn't yield anything, then there's something wrong.


   ;now look for the first position with a DIFFERENT MOST sign. byte. 2 bytes later starts the position we want. But then check if we
   ;indeed find 4 times in a row this MSB.

   mov   bx, sec2buff
   add    bx, word [Scountpos]

   mov     ax, word [bx+2]   ; MSB
zoekbegin:

   mov    cx, word [bx-2]
   cmp    aX, cx
   jne begingevonden
   sub    bx, 4
   jmp zoekbegin



begingevonden:
         ;now see if we indeed find 4 times the same msb in a row

   mov di, 0

vlgndzk:
   mov cx, word [bx+2]
   cmp cx, word [bx+di+6]
   jne klaarmetcmp
   add di, 4
   jmp vlgndzk


klaarmetcmp:

        cmp di, 12
   jne Scountnotfound
   sub bx, sec2buff
   mov word [Scountpos], bx

   jmp Scountfound

Scountnotfound:

   cmp byte [searchbool], 1
   je searchfailed
   mov byte [searchbool], 1
   mov bx, ECscount2
   jmp start2

searchfailed:
   mov     ah, 9
   mov     dx, Scounterrorstring
   int     0x21
   jmp    exit

Scountfound:
   mov   bx, sec2buff            ;Now copy the s16scount into sector 2
   add    bx, word [Scountpos]   ;target
   mov    di, 0


   dec   word [s16scount]   ;decrease the sector count by one before saving it to FW !!!
copyscountintos2:

   mov    ax, word [s16scount]
   mov   [bx+di],ax
   mov    ax, word [s16scount+2]
   mov   [bx+di+2],ax
   add    di, 4
   cmp   di, 16
   jne copyscountintos2


FlashFW:

        mov     bx, sec2buff    ;dx = pointer to buffer (ds:dx)
        mov     dx, S2Filename
        mov     cx, 1024
        call SaveFile                   ;save modified modID 2e, for checking

        mov     bx, secDbuff    ;dx = pointer to buffer (ds:dx)
        mov     dx, SDFilename
        mov     cx, 512
        call SaveFile                   ;save modified modID 2e, for checking


   mov ax, 3
   int 10h      ;clear screen

   mov     ah,9
   mov     dx, YesMessage
   int     21h

        call writesec2
        call writesecD

        mov     ah, 9
   mov     dx, flashedtext
   int     0x21


   jmp    exit





searchndrv:
   mov ax, word [testcounter]
   mov bx, 0
   mov bl, byte [pcount]
   inc word [testcounter]
   cmp ax, bx
   jne mainlus

        mov     ah, 9
   mov     dx, trylegacy
   int     0x21

   call findIDE


exit:
   STI        ; re enable interrupts
   mov ah,0x4C  ;terminate program
   int 21h
;---------------------

modspacerr:
        mov     ah, 9
   mov     dx, nomodspacestring
   int     0x21
   jmp exit

;---------------------------------------------------------------

DRQERR:

        mov     ah, 9
   mov     dx, DRQ_error
   int     0x21
   jmp exit
;---------------------------------------------------------------------

writesecD:

   call   Wait_For_DRDY
   call    Wait_For_NBSY
   call   SEND_cfgsect_ATA
   call    Wait_For_NBSY

   mov    DX, [basereg]
   add    DX, 7       ;status register
   in    al, dx
   cmp    al, 58h
   jz   DRQOK7
   jmp    DRQERR
DRQOK7:

   call   Wait_For_DRDY
   mov    bx, SCT_Packet_wD
   call    send_Key_sector
   call    Wait_For_NBSY

   mov    DX, [basereg]
   add    DX, 7       ;status register
   in    al, dx

   call   Wait_For_DRDY

   mov     al, 0xD6       ;features, set to 'write'
   mov     Bl, 1          ;sector count
   call    send_Get_sector
   call    Wait_For_NBSY

   mov    DX, [basereg]
   add    DX, 7       ;status register
   in    al, dx

   cmp al, 0x58
   jz   DRQOK8
   jmpDRQERR
DRQOK8:

   mov     cx, 0
   mov     si, secDbuff

ogle2:
   mov     dx, [basereg]
   add   dx, 7
   in      al,dx
   test    al,8            ;Wait for sector secDbuffer ready.
   jz      ogle2

   mov     cx,0x100       ;One sector /2

   mov     dx, [basereg]        ;Data port - data comes in and out of here.
      rep     outsw

   call    Wait_For_NBSY

   mov    DX, [basereg]
   add    DX, 7       ;status register
   in    al, dx
   ret

;---------------------------------------------------------------------------
writesec2:

   call   Wait_For_DRDY
   call    Wait_For_NBSY
   call   SEND_cfgsect_ATA
   call    Wait_For_NBSY

   mov    DX, [basereg]
   add    DX, 7       ;status register
   in    al, dx
   cmp    al, 58h
   jz   DRQOK9
   jmp    DRQERR
DRQOK9:

   call   Wait_For_DRDY
   mov    bx, SCT_Packet_w2
   call    send_Key_sector
   call    Wait_For_NBSY

   mov    DX, [basereg]
   add    DX, 7       ;status register
   in    al, dx

   call   Wait_For_DRDY

   mov     al, 0xD6       ;features, set to 'write'
   mov     Bl, 2          ;sector count
   call    send_Get_sector
   call    Wait_For_NBSY

   mov    DX, [basereg]
   add    DX, 7       ;status register
   in    al, dx

   cmp al, 0x58
   jz   DRQOK10
   jmp   DRQERR
DRQOK10:

   mov     cx, 0
   mov     si, sec2buff

ogle3:
   mov     dx, [basereg]
   add   dx, 7
   in      al,dx
   test    al,8            ;Wait for sector sec2buffer ready.
   jz      ogle3

   mov     dx, [basereg]        ;Data port - data comes in and out of here.
   outsw                 ;Send it.
   inc    cx
   cmp    cx, 512   ; send 2 sectors = 512 words
   jne    ogle3

   call    Wait_For_NBSY

   mov    DX, [basereg]
   add    DX, 7       ;status register
   in    al, dx
   ret


;---------------------------------------------
;This proc sends the SCT packet containing the command
;needs: bx = pointer to SCT packet

send_Key_sector:



   mov     cx, 0
   mov     si, bx

oogle:
   mov     dx, [basereg]
   add   dx, 7
   in      al,dx
   test    al,8            ;Wait for sector buffer ready.
   jz      oogle

   mov     cx,0x100       ;One sector /2
   mov     dx, [basereg]        ;Data port - data comes in and out of here.
      rep     outsw

   ret







;------------------------------------------------------------------
; This ATA tells the drive that a SCT key packet is coming. That SCT packet contains the
; command that the drive has to execute.


SEND_cfgsect_ATA:

   push ax
   push dx
   mov    DX, [basereg]

   mov     al, 0xD6 ;features
   inc   dx
   OUT    DX, AL

   mov     al, 1 ;sector count
   inc   dx
   OUT    DX, AL

   mov     al, 0xBE;sector
   inc   dx
   OUT    DX, AL

   mov     al, 0x4F ;cylinder low
   inc    dx
   OUT    DX, AL

   mov     al, 0XC2;cylinder high
   inc    dx
   OUT    DX, AL

   mov     al, byte [head] ;0xA0 ;drivehead
   inc    dx
   OUT    DX, AL

   MOV    AL, 0xB0
   inc   dx
   OUT    DX, AL

   pop dx
   pop ax
   ret


;--------------------------------------------------------------------------------
; This function wants inputs:
; BL = sector count
; Al = features


send_Get_sector

   mov    DX, [basereg]

   inc   dx
   OUT    DX, AL

   mov     al, bl ;sector count
   inc   dx
   OUT    DX, AL

   mov     al, 0xBF;sector
   inc   dx
   OUT    DX, AL

   mov     al, 0x4F ;cylinder low
   inc    dx
   OUT    DX, AL

   mov     al, 0XC2;cylinder high
   inc    dx
   OUT    DX, AL

   mov     al, byte [head] ;0xA0 ;drivehead
   inc    dx
   OUT    DX, AL

   MOV    AL, 0xB0
   inc   dx
   OUT    DX, AL

   ret

;-----------------------------------------------------

SetNativeStatus:

   call   Wait_For_DRDY
   call    Wait_For_NBSY
   call   SEND_nstat_ATA   ;set native status ATA
   call    Wait_For_NBSY

   mov    DX, [basereg]
   add    DX, 7       ;status register

   in    al, dx

   cmp    al, 51h
   jz    drive_not_sup_err
   cmp    al, 53h
   jz    drive_not_sup_err
   jmp    drive_supported

drive_not_sup_err:

        mov     ah, 9
   mov     dx, DRv_n_supp
   int     0x21
   jmp exit

drive_supported:

   ret

;----------------------------------------------------------
SEND_nstat_ATA:

   push ax
   push dx
   mov    DX, [basereg]

   mov     al, 0x45 ;features
   inc   dx
   OUT    DX, AL

   mov     al, 0xB0  ;sector count
   inc   dx
   OUT    DX, AL

   mov     al, 0 ;sector
   inc   dx
   OUT    DX, AL

   mov     al, 0x44 ;cylinder low
   inc    dx
   OUT    DX, AL

   mov     al, 0X57 ;cylinder high
   inc    dx
   OUT    DX, AL

   mov     al, byte [head] ;0xA0 ;drivehead
   inc    dx
   OUT    DX, AL

   MOV    AL, 0x80
   inc   dx
   OUT    DX, AL

   pop dx
   pop ax
   ret


;--------------------------------------------------------
writesectors:

        mov     si, sec16
        mov bx, 0

writel1:
   call Wait_For_DRDY
   call Wait_For_NBSY

; send write command

   mov    DX, [basereg]
   mov     al, 1 ;sector count
   INC DX
        INC DX
   OUT DX, AL
        mov     al, 17 ;sector
        add     al, bl
   INC DX
   OUT DX, AL
   mov     al, 0 ;cylinder low
   INC DX
   OUT DX, AL
   mov     al, 0 ;cylinder high
   INC DX
   OUT DX, AL
        mov     al, byte [head] ;0xA0 ;drivehead
   INC DX
   OUT DX, AL
   INC DX
   MOV AL, 0x30   ;write with retry
   OUT DX, AL

; now send the sector

r20lussw:
   in      al,dx
   test    al,8            ;Wait for sector buffer ready.
   jz      r20lussw

   mov     cx,0x100       ;One sector /2

   mov     dx, [basereg]        ;Data port - data comes in and out of here.
      rep     outsw
        add bx, 1
        cmp bx, 7
        jne writel1
   ret


;--------------------------------------------------------
readsec16_22:

        mov     di, sec16
        mov bx, 0

readl1:
   call Wait_For_DRDY
   call Wait_For_NBSY

; send read command

   mov    DX, [basereg]
   mov     al, 1 ;sector count
   INC DX
        INC DX
   OUT DX, AL
        mov     al, 17 ;sector
        add     al, bl
   INC DX
   OUT DX, AL
   mov     al, 0 ;cylinder low
   INC DX
   OUT DX, AL
   mov     al, 0 ;cylinder high
   INC DX
   OUT DX, AL
        mov     al, byte [head] ;0xA0 ;drivehead
   INC DX
   OUT DX, AL
   INC DX
   MOV AL, 0x20   ;read with retry
   OUT DX, AL

; now receive the sector

r20luss:
   in      al,dx
   test    al,8            ;Wait for sector buffer ready.
   jz      r20luss

   mov     cx,0x100       ;One sector /2

   mov     dx, [basereg]        ;Data port - data comes in and out of here.
      rep     insw
        add bx, 1
        cmp bx, 7
        jne readl1
   ret


verify16_22:

   cmp word [sec16], 0
   je verifyerr

   mov ax,0
   mov bx, sec16
   add bx, 512      ;skip first sector

verlus:
   add ax, [bx]
   add bx, 2
   cmp bx, sec16+3582
   jne verlus
   cmp ax,   0x6E13
   jne verifyerr
        mov byte [dataokbool],1

verifyerr:

   ret


;-------------------------------------------------------------------------------

Wait_For_NBSY2:

                 push    ax
                 push    bx
                 push    cx
                 push    dx
                 xor     cx, cx
                 xor     bx, bx

poc_104E2x:
                 inc     cx
                 cmp     cx, 0FFF0h
                 jnb     short loc2xxx
                 nop
                 nop
                 jmp     short poc_104F8x

loc2xxx:
                 xor     cx, cx
                 inc     bx
                 cmp     bx, 0x20
                 jnb     short poc_10509xx
                 nop
                 nop

poc_104F8x:
                 mov     dx,  word [basereg]
                 add     dx, 7
                 in      al, dx
                 and     al, 80h
                 jnz     short poc_104E2x
                 pop     dx
                 pop     cx
                 pop     bx
                 pop     ax
      mov    ax, 1      ;drive found
                 ret
; ---------------------------------------------------------------------------
poc_10509xx:
                 pop     dx
                 pop     cx
                 pop     bx
                 pop     ax
      mov ax, 0   ;failed, timed out
      ret

;---------------------------------------------------------------------------------
Wait_For_NBSY:

                 push    ax
                 push    bx
                 push    cx
                 push    dx
                 xor     cx, cx
                 xor     bx, bx

loc_104E2:
                 inc     cx
                 cmp     cx, 0FFF0h
                 jnb     short loc_104ED
                 nop
                 nop
                 jmp     short loc_104F8

loc_104ED:
                 xor     cx, cx
                 inc     bx
                 cmp     bx, 0x5a
                 jnb     short loc_10509
                 nop
                 nop

loc_104F8:
                 mov     dx,  word [basereg]
                 add     dx, 7
                 in      al, dx
                 and     al, 80h
                 jnz     short loc_104E2
                 pop     dx
                 pop     cx
                 pop     bx
                 pop     ax
                 ret
; ---------------------------------------------------------------------------
loc_10509:
                 pop     dx
                 pop     cx
                 pop     bx
                 pop     ax
            mov     ah, 9           ;
            mov     dx, strTimeOut  ;
           int     0x21            ;
                 jmp exit

;---------------------------------------------------------------------------

Wait_For_DRDY:

                 push    ax
                 push    bx
                 push    cx
                 push    dx
                 xor     cx, cx
                 xor     bx, bx

loc_104E2x:
                 inc     cx
                 cmp     cx, 0FFF0h
                 jnb     short loc_104EDx
                 nop
                 nop
                 jmp     short loc_104F8x

loc_104EDx:
                 xor     cx, cx
                 inc     bx
                 cmp     bx, 50
                 jnb     short loc_10509x
                 nop
                 nop

loc_104F8x:
                 mov     dx, word [basereg]
                 add     dx, 7
                 in      al, dx
                 and     al, 40h
                 jz     short loc_104E2x
                 pop     dx
                 pop     cx
                 pop     bx
                 pop     ax
                 ret
; ---------------------------------------------------------------------------
loc_10509x:
                 pop     dx
                 pop     cx
                 pop     bx
                 pop     ax
            mov     ah, 9           ;
            mov     dx, strTimeOut2  ;
           int     0x21            ;
       jmp exit


;-------------------------------------------------------------------------------

processEC:


   call Wait_For_NBSY
   call Wait_For_DRDY

   MOV DX,word [basereg] ;status register
   add dx, 7
LOOP3:
   IN AL, DX

   cmp     al, 58h;if DRQ is not high, the device doesn't have data for us
   jz readreply

   mov     ah, 9
   mov     dx, DRQ_error
   int     0x21
   jmp exit

readreply:


   MOV DX, word [basereg] ;data register
   MOV DI,  buff ;points DI to the buffer we're using
   MOV CX, 256 ;256 decimal. This controls the REP command.
   CLD ;clear the direction flag so INSW increments DI (not decrements it)
   REP INSW

   ;We now have the string data in buff.


   ;safe EC buffer

   ;mov     bx, buff   ;dx = pointer to buffer (ds:dx)
   ;mov     dx, ecFilename
   ;mov     cx, 512
   ;call SaveFile         ;save modified modID 2e, for checking



               ; now copy the serial to the var

   mov bx, buff
   add bx, 20   ;point to serial
   mov di, 0

copyserial:
        mov ax, [bx]
   mov byte [ECserial+di], ah
   mov byte [ECserial+di+1], al
   add bx, 2
   add di,2
   cmp di, 20
   jne copyserial


               ; copy the EC model to the var

   mov bx, buff
   add bx, 54   ;point to serial
   mov di, 0

copyModel:
        mov ax, [bx]
   mov byte [ECModel+di], ah
   mov byte [ECModel+di+1], al
   add bx, 2
   add di,2
   cmp di, 40
   jne copyModel


               ; copy the bios version to the var

   mov bx, buff
   add bx, 46   ;point to serial
   mov di, 0

copyBios:
        mov ax, [bx]
   mov byte [ECBios+di], ah
   mov byte [ECBios+di+1], al
   add bx, 2
   add di,2
        cmp di, 8
   jne copyBios


               ; copy the sect count to the var

   mov bx, buff
   add bx, 120   ;point to sector count
   mov di, 0

copyECscount:
        mov ax, [bx]
        mov [ECscount+di], ax
   add bx, 2
   add di,2
        cmp di, 4
   jne copyECscount

   mov bx, buff
   add bx, 0xc8   ;point to sector count backup position
   mov di, 0

copyECscount2:
        mov ax, [bx]
        mov [ECscount2+di], ax
   add bx, 2
   add di,2
        cmp di, 4
   jne copyECscount2





   ret


;-----------------------------------

readundofile:

   mov dx,undoFilename    ; put address of filename in dx
   mov al,0       ; access mode - read only
   mov ah,3Dh       ; function 3Dh -open a file
   int 21h       ; call DOS service

   mov [FileHandle],ax       ; save file handle for later
   jc ErrorOpening    ; jump if carry flag set - error!

   mov dx,secDbuff    ; address of buffer in dx
   mov bx,[FileHandle]      ; handle in bx
   mov cx,1536       ; amount of bytes to be read
   mov ah,3Fh       ; function 3Fh - read from file
   int 21h       ; call dos service

   jc ErrorReading    ; jump if carry flag set - error!

   cmp ax, 1536
   jne ErrorReading

   mov bx,[FileHandle]       ; put file handle in bx
   mov ah,3Eh       ; function 3Eh - close a file
   int 21h       ; call DOS service

   ret

;----------------------------------------------------------------
readhdssfile:

   mov dx,hddssFilename   ; put address of filename in dx
   mov al,0       ; access mode - read only
   mov ah,3Dh       ; function 3Dh -open a file
   int 21h       ; call DOS service

   mov [FileHandle],ax       ; save file handle for later
   jc ErrorOpening    ; jump if carry flag set - error!

   mov dx,sec16    ; address of buffer in dx
   mov bx,[FileHandle]      ; handle in bx
   mov cx,3584      ; amount of bytes to be read
   mov ah,3Fh       ; function 3Fh - read from file
   int 21h       ; call dos service

   jc ErrorReading    ; jump if carry flag set - error!

   cmp ax, 3584
   jne ErrorReading

   mov bx,[FileHandle]       ; put file handle in bx
   mov ah,3Eh       ; function 3Eh - close a file
   int 21h       ; call DOS service

   ret

ErrorOpening:

            mov     ah, 9
          mov     dx, File_open_error
           int     0x21
                 jmp exit

ErrorReading:

            mov     ah, 9
          mov     dx, File_read_error
           int     0x21
                 jmp exit

;--------------------------------------------------------------------------
SaveFile:

; wants: dx = filename (pointer to string)
; cx = length
; bx = pointer buffer


; mov     bx, sec2buff   ;dx = pointer to buffer (ds:dx)
; mov     dx, S2Filename
; mov     cx, 1024

      push    cx
      push   bx
                 ;mov     dx, Filename
                 mov     cx, 20h
                 mov     ah, 3Ch
                 int     21h             ; DOS - 2+ - CREATE A FILE WITH HANDLE (CREAT)
                                         ; CX = attributes for file
                                         ; DS:DX -> ASCIZ filename (may include drive and path)
                 jb      FileErr
                 nop
                 nop
                 mov     [FileHandle], ax

loc_10521:
                 mov     bx, [FileHandle]
                 mov     cx, 0
                 mov     dx, 0
                 mov     al, 2
                 mov     ah, 42h
                 int     21h             ; SeekFile
                                         ; AL = method: offset from end of file
                 jb      FileErr
                 nop
                 nop

                 mov     ah, 40h
                 mov     bx, [FileHandle]
       pop    dx         ;mov     dx, sec2buff   ;dx = pointer to buffer (ds:dx)
                 pop   cx          ;mov     cx, 1024   ;cx = save 512 bytes
                 int     21h             ; WriteFile
                                         ; BX = file handle, CX = number of bytes to write, DS:DX -> buffer
                 jb      short FileErr
                 nop
                 nop


loc_10570:
                 mov     bx, [FileHandle]
                 mov     ah, 3Eh
                 int     21h             ; DOS - 2+ - CLOSE A FILE WITH HANDLE
                                         ; BX = file handle
      ret
;-------------------------------------------------------------------------------


FileErr:
            mov     ah, 9           ;
          mov     dx, File_error   ;
           int     0x21            ;
       jmp exit

;------------------------------------------------------------------------
findIDE:

   MOV DX, 0x176
   MOV AL, 0xA0 ; select device 0 (master)
   OUT DX, AL

   MOV DX, 0x177
   MOV AL, 0ECh ;"IDENTIFY DRIVE" command
   OUT DX, AL ;sends the command!

   mov word [basereg], 0x170
   call Wait_For_NBSY2

   cmp ax, 1
   jne nietsgevonden1

   mov dx, word [basereg]
   add dx, 7
   in al, dx
        cmp al, 58h
        jnz nietsgevonden1

        mov di, gevondenIDE
        mov bx,0
        mov bl, byte [aantalIDE]
        mov byte [di+bx],1
        add byte [aantalIDE], 1

   call processEC
   mov     ah, 9
   mov     dx, IDElijn
   int     0x21
   mov     ah, 9
   mov     dx, ECModel
   int     0x21
   add byte [IDElijn+3], 1



nietsgevonden1:

   MOV DX, 0x176
   MOV AL, 0xB0 ; select device 0 (slave)
   OUT DX, AL

   MOV DX, 0x177
   MOV AL, 0ECh ;"IDENTIFY DRIVE" command
   OUT DX, AL ;sends the command!

   mov word [basereg], 0x170
   call Wait_For_NBSY2


        cmp ax, 1
   jne nietsgevonden2

   mov dx, word [basereg]
   add dx, 7
   in al, dx
        cmp al, 58h
        jnz nietsgevonden2


        mov di, gevondenIDE
        mov bx,0
        mov bl, byte [aantalIDE]
        mov byte [di+bx],2
        add byte [aantalIDE], 1


   call processEC
   mov     ah, 9
   mov     dx, IDElijn
   int     0x21
   mov     ah, 9
   mov     dx, ECModel
   int     0x21

   add byte [IDElijn+3], 1

nietsgevonden2:

   MOV DX, 0x1F6
   MOV AL, 0xA0 ; select device 0 (master)
   OUT DX, AL

   MOV DX, 0x1F7
   MOV AL, 0ECh ;"IDENTIFY DRIVE" command
   OUT DX, AL ;sends the command!

   mov word [basereg], 0x1F0
   call Wait_For_NBSY2
   cmp ax, 1
   jne nietsgevonden3
   mov dx, word [basereg]
   add dx, 7
   in al, dx
        cmp al, 58h
        jnz nietsgevonden3


        mov di, gevondenIDE
        mov bx,0
        mov bl, byte [aantalIDE]
        mov byte [di+bx],3
        add byte [aantalIDE], 1


   call processEC
   mov     ah, 9
   mov     dx, IDElijn
   int     0x21
   mov     ah, 9
   mov     dx, ECModel
   int     0x21

   add byte [IDElijn+3], 1

nietsgevonden3:


   MOV DX, 0x1F6
   MOV AL, 0xB0 ; select device 0 (slave)
   OUT DX, AL

   MOV DX, 0x1F7
   MOV AL, 0ECh ;"IDENTIFY DRIVE" command
   OUT DX, AL ;sends the command!

   mov word [basereg], 0x1F0
   call Wait_For_NBSY2
   cmp ax, 1
   jne nietsgevonden4
   mov dx, word [basereg]
   add dx, 7
   in al, dx
        cmp al, 58h
        jnz nietsgevonden4


        mov di, gevondenIDE
        mov bx,0
        mov bl, byte [aantalIDE]
        mov byte [di+bx],4
        add byte [aantalIDE], 1


        call processEC
   mov     ah, 9
   mov     dx, IDElijn
   int     0x21
   mov     ah, 9
   mov     dx, ECModel
   int     0x21



nietsgevonden4:

   mov     ah, 9
   mov     dx, Selectdrvmsg
   int     0x21

     mov     ah,0
        int     16h         ;get a key, returned in AX
                        ;AL is the ASCII part
                        ;AH is the SCAN CODE
      push    ax
       mov     dl,al
       mov     ah,2
       int     21h         ;print character in dl
       pop     ax
   cmp    al, 'x'
   je exit
   cmp    al, 'X'
   je exit
       cmp     al,   0x2F
   JA mingoed

   mov     ah, 9
   mov     dx, invalidinput
   int     0x21
   jmp nietsgevonden4

mingoed:
   mov bl, 0x30
   add bl, byte [aantalIDE]
   cmp al, bl
   JNA mingoed2

   mov     ah, 9
   mov     dx, invalidinput
   int     0x21
   jmp nietsgevonden4

mingoed2:
        sub al, 0x31
   mov bx, 0
   mov bl,al
        mov al, byte [gevondenIDE+bx]

   cmp al, 1
   jne nx1
   mov word [basereg], 0x170
   mov byte [head], 0xA0
   jmp idefound
nx1:

   cmp al, 2
        jne nx2
   mov word [basereg], 0x170
   mov byte [head], 0xB0
   jmp idefound
nx2:
   cmp al, 3
        jne nx3
   mov word [basereg], 0x1F0
   mov byte [head], 0xA0
   jmp idefound
nx3:
   cmp al, 4
        jne nx4
   mov word [basereg], 0x1F0
   mov byte [head], 0xB0
   jmp idefound
nx4:

   mov     ah, 9
   mov     dx, invalidinput
   int     0x21

   jmp exit


idefound:
        ;mov word [basereg], 0x170
        ;mov byte [head], 0xB0

   MOV DX, word [basereg];device/head register
   add dx, 6
   MOV AL, byte [head]; select head
   OUT DX, AL

   MOV DX, word [basereg] ;command register
   add dx, 7
   MOV AL, 0ECh ;"IDENTIFY DRIVE" command
   OUT DX, AL ;sends the command!

   call Wait_For_NBSY2
   cmp ax, 1
   jne nietsgevonden5
   mov dx, word [basereg]
   add dx, 7
   in al, dx
        cmp al, 58h
        jnz nietsgevonden5


        jmp gevondenx


nietsgevonden5:
   mov     ah, 9
   mov     dx, invalidinput
   int     0x21
   jmp exit


;---------------------------------------------------------------------------
Findports:

   mov byte [bus], 0
detectbuslus:
   mov byte [deviceid], 0
detectdevlus:
   mov byte [func], 0

detectfunclus:
   mov ax,0xb108      ;'read configuration byte' functio
   mov bl,[deviceid]
   shl bl,3
   add bl,[func]      ;bl = device nr + function nr
   mov bh,[bus]      ;bh = bus nr
   mov di,0      ;di = index, read first index byte
   int $1a
   jnc nolookupERR
   jmp lookuperr

nolookupERR:

   mov dl, cl   ;save reg value

   mov ax,0xb108      ;'read configuration byte' functio
   mov bl,[deviceid]
   shl bl,3
   add bl,[func]      ;bl = device nr + function nr
   mov bh,[bus]      ;bh = bus nr
   mov di,1      ;di = index, read 2nd index byte
   int $1a
   jnc nolookupERR2
   jmp lookuperr

nolookupERR2:

   cmp dl, cl
   jnz devicegevonden
   cmp cl, 0xFF
   jnz devicegevonden
   jmp niksgevonden

devicegevonden:               ;IF CL=DL=$FF, then no device found

        ;mov     ah, 9               ;PCI device found, show message
     ;mov     dx, gevonden_msg
     ;int     0x21

   mov ax,0xb108      ;'read configuration byte' functio
   mov bl,[deviceid]
   shl bl,3
   add bl,[func]      ;bl = device nr + function nr
   mov bh,[bus]      ;bh = bus nr
   mov di,0xB      ;di = index, class
   int $1a
   jnc nolookupERR3
   jmp lookuperr

nolookupERR3:

   cmp cl, 1      ;class 1 = mass storage controller
   jz doorgaan
   jmp niksgevonden

            ;Going to test for the sub class now, to be not 0 (scsi), 2 (floppy) or 3 (ipi)
doorgaan:

   mov ax,0xb108      ;'read configuration byte' functio
   mov bl,[deviceid]
   shl bl,3
   add bl,[func]      ;bl = device nr + function nr
   mov bh,[bus]      ;bh = bus nr
   mov di,0xA      ;di = index, SUBclass
   int $1a
   jnc nolookupERRx
   jmp lookuperr

nolookupERRx:

   cmp cl, 0      ;subclass 0 = SCSI
   jne n1
   jmp niksgevonden
n1:
   cmp cl, 2      ;class 2 = Floppy
   jne n2
   jmp niksgevonden
n2:
   cmp cl, 3      ;class 3 = ipi
   jne n3
   jmp niksgevonden
n3:
   cmp cl, 1      ;class 1 = IDE
   jne n4

   ;device = IDE controller, see if it is a 'normal IDE', with ports 170 or 1F0, if so, skip this entry

   mov ax,0xb108      ;'read configuration byte' functio
   mov bl,[deviceid]
   shl bl,3
   add bl,[func]      ;bl = device nr + function nr
   mov bh,[bus]      ;bh = bus nr
   mov di,0x9      ;di = index
   int $1a
   jnc nolookupERR3x
   jmp lookuperr

nolookupERR3x:
   mov al, cl    ;make backup
   and al, 1
   cmp al, 0
   jne n3a
   jmp niksgevonden ;controller has port 1F0 available, skip it !
n3a:
   and cl, 4
   cmp cl,0
   jne n4
   jmp niksgevonden; ;controller has port 170 available, skip it !


n4:



   ;now see if it has ports available

         ;mov     ah, 9
      ;mov     dx, ctr_gevonden_msg  ;found a mass storage controller
      ;int     0x21


   mov ax,0xb108      ;'read configuration byte' functio
   mov bl,[deviceid]
   shl bl,3
   add bl,[func]      ;bl = device nr + function nr
   mov bh,[bus]      ;bh = bus nr
   mov di,0xE      ;di = index: 0xe='header type
   int $1a
   jnc nolookupERR4
        jmp lookuperr

nolookupERR4:

   and cl, 0x7F
   jz v1
   jmp niksgevonden
v1:


   mov word [nnt], 0      ;nnt = aantal poorten = 0 t/m 4 (5 poorten)
hlusje:
   mov word [nn], 0      ;nn loopt van 0 t/m 3
   mov word [nnsum], 0

lusje:
   mov ax,0xb108      ;'read configuration byte' functio
   mov bl,[deviceid]
   shl bl,3
   add bl,[func]      ;bl = device nr + function nr
   mov bh,[bus]      ;bh = bus nr

   xor di,di
   mov di,word [nn]
   add di, word [nnt]
   add di,0x10      ;di = index


   int $1a
   jnc nolookupERR5
        jmp lookuperr

nolookupERR5:

   and cx, 0xff
   add word [nnsum], cx

   cmp word [nn], 0
   jne geeneerste

   and cx, 1      ;check eerste van de 4 entries, als deze and met cx eindigt op 1, dan is het een adress en GEEN port !!
   cmp cx, 1
   jne noportfound

   mov byte [plbyte], cl   ;save port adress for later

geeneerste:
   cmp word [nn], 1
   jne geentweede
   mov byte [phbyte], cl

geentweede:

   inc word [nn]
   cmp word [nn], 4
   jnz lusje

   cmp word [nnsum], 0
   jz noportfound

   mov cl, byte [plbyte]
   and cl, 0xFC
   mov ch, byte [phbyte]

   ;now cx contains port adress, see if we have had that adress before, if not, store it



   cmp byte [pcount], 0
   jz newpfound      ;no entries in table, so store it

   ;now check if entry is in table already

   mov byte [alreadystored], 0   ;this is the boolean we use to see if we found a match in the table
   mov ax,0
   mov al, byte [pcount]   ;save number of already stored ports to ax and use that as counter
tablelus:
   mov bx, portlist
   add bx, ax
   add bx, ax      ;add two times, since we're using words, not bytes
   cmp word [bx-2], cx
   jne next

   mov byte [alreadystored], 1 ;match found, set boolean

next:

   dec ax
   cmp ax, 0
   jne tablelus

   cmp byte [alreadystored], 1
   je noportfound      ;port was in table already


newpfound:
   mov bx, portlist
   mov ax, 0
   mov al, byte [pcount]
   add bx, ax
   add bx, ax
   inc byte [pcount]
   mov word [bx], cx      ;store port adress in table




    ;mov     ah, 9
    ;mov     dx, port_found   ;found a 6 ports mass storage controller
      ;int     0x21

noportfound:

   add word [nnt], 4

   cmp word [nnt], 24 ;do all 6 ports
   jnz hlusje

niksgevonden:
   inc byte [func]
   cmp byte [func], 8
   jnz detectfunclus

   inc byte [deviceid]
   cmp byte [deviceid], 32
   jnz detectdevlus

   inc byte [bus]
   cmp byte [bus], 255
   jnz detectbuslus

   ret


lookuperr:

   mov     ah, 9
     mov     dx, lookup_error ;error, something went wrong, terminate
     int     0x21
   jmp exit


DRV_NF_error db     'Error ! Drive not found!$'
DRQ_error   db     'Error ! Drive has no ID data packet for us !$'
strTimeOut  db     'Error ! BUSY timeout expired !$'
strTimeOut2  db     'Error ! DRDY timeout expired !$'
buff: times 512 db 0 ;buffer to hold the drive identification info
deviceid    db 0
func      db 0
bus      db 0
index      dw 0
nn      dw 0
nnsum      dw 0
nnt      dw 0
plbyte      db 0
phbyte      db 0
alreadystored   db 0

portlist   times 24 dw 0
pcount      db 0




fiveportsfound_msg   db     'Controller has 5 ports !! !$'
ctr_gevonden_msg db     'Device Is a Mass Storage controller !! !$'
lookup_error db     'Error while looking up PCI device reg !$'
port_found db   'Addr port found !$'
testcounter dw 0
basereg dw 0
S2Filename db 's2.bin',0
SDFilename db 'sd.bin',0
ecFilename db 'ec.bin',0
hddssFilename db 'hddss.bin',0
undoFilename db 'undo.bin',0
FileHandle dw 0
File_error db     'File acces error, could not save data !$'

ECserialstring: db 0xD,0xA,    'Serial IDENTIFY DEV: '
ECserial: times 20 db 0
   db 0xD,0xA,'$'
ECModelstring: db     'Model IDENTIFY DEV: '
ECModel: times 40 db 0
   db 0xD,0xA,'$'
ECBiosstring: db     'Bios version IDENTIFY DEV: '
ECBios: times 8 db 0
   db 0xD,0xA,'$'

ECscount: times 4 db 0
ECscount2: times 4 db 0

s16serialstring: db     'Serial in hddss.bin file: '
s16serial: times 20 db 0
   db 0xD,0xA,'$'
s16Modelstring: db     'Model string in hddss.bin file: '
s16Model: times 40 db 0
   db 0xD,0xA,'$'
s16Biosstring: db     'Bios version in hddss.bin file: '
s16Bios: times 8 db 0
   db 0xD,0xA,'$'

s16scount times 4 db 0


Sec2serialstring: db     'Serial on sector 2: '
Sec2serial: times 20 db 0
   db 0xD,0xA,'$'
sec2Modelstring: db     'Model on sector 2: '
Sec2Model: times 40 db 0
   db 0xD,0xA,'$'
SecDBiosstring: db     'Bios version on sector 2: '
SecDBios: times 8 db 0
   db 0xD,0xA,'$'

Sec2scount times 4 db 0

introtext: times 40 db '*'
   db  0xD,0xA
   db '*    HddHacker  v0.90                  *',0xD,0xA
   db '*    For the WD BEVS Scorpio series    *',0xD,0xA
   db '*                                      *',0xD,0xA
   db '*    (c) 2007 The Specialist           *' ,0xD,0xA
   times 40 db '*'
   db 0xD,0xA
   db 0xD,0xA,'$'
detecting:
   db 'Detecting drives, please wait...    ' ,0xD,0xA,'$'

option_error:
   db  0xD,0xA
   db 'Usage:',0xD,0xA
   db 'HddHackr -d      To dump sector 16-22 to file hddss.bin',0xD,0xA
   db 'HddHackr -f      Flash FW with hddss.bin file ',0xD,0xA
   db 'HddHackr -u      Restore original FW from undo.bin',0xD,0xA,'$'

sec16: times 3584 db 0
equal: db 0xD,0xA,'Drive should work in your 360 !!',0xD,0xA, '$'

IDElijn: db 0xD,0xA,'(1) ' ,'$'
serialerrorstring: db 'Fatal error, the EC serial string was not found in the FW',0xD,0xA, '$'
modelerrorstring: db 'Fatal error, the EC model string was not found in the FW',0xD,0xA, '$'
Bioserrorstring: db 'Fatal error, the EC bios string was not found in the FW',0xD,0xA, '$'
Scounterrorstring: db 'Fatal error, the EC sector count was not found in the FW',0xD,0xA, '$'
Fatalerrorstring: db 'Fatal error occured',0xD,0xA, '$'
devicefoundtext: db 'Some SATA device responded ... ',0xD,0xA, '$'
notequal: db 'Information on sector 16 does NOT match the firmware info.',0xD,0xA
        db 'Want to flash this firmware to make it compatible with sector 16 ? (y/n)$'
YesMessage: db 0xD,0xA,'Trying to flash your firmware, DO NOT turn off your computer',0xD,0xA, '$'
NoyesMessage: db 0xD,0xA,'User Aborted',0xD,0xA, '$'
DRv_n_supp   db     'Error ! drive not supported, make sure it a SATA WD Scorpio BEVS !',0xD,0xA, '$'
DRv_supp db 'Drive supports flash commands',0xD,0xA, '$'
SCT_Packet_r2 dw 8, 1, 2   ;command, read/write, modID
SCT_Packet_w2 dw 8, 2, 2   ;command, read/write, modID
SCT_Packet_rD dw 8, 1, 0X0D   ;command, read/write, modID
SCT_Packet_wD dw 8, 2, 0X0D   ;command, read/write, modID
secDbuff: times 512 db 0
sec2buff: times 1024 db 0
serialpos: dw 0
modelpos: dw 0
Biospos: dw 0
Scountpos: dw 0
serialzero: dw 0
Bioszero: dw 0
modelzero: dw 0
noserspacestring: db 'Fatal error, Not enough room in FW for your serial',0xD,0xA, '$'
nomodspacestring: db 'Fatal error, Not enough room in FW for your model string',0xD,0xA, '$'
nobiosspacestring: db 'Fatal error, Not enough room in FW for your Bios string',0xD,0xA, '$'
File_open_error: db 0xD,0xA,'Error, could not open file ',0xD,0xA, '$'
File_read_error: db 0xD,0xA,'Error reading file ',0xD,0xA, '$'
scountsearch: times 16 db 0
lengths16modstring: dw 0
nodrivefound: db 'Could not detect drive. Make sure you are running this from pure MS-DOS',0xD,0xA, '$'
undoquestion: db 0xD, 0xA, 'Do you want to create an undo file ? (y/n) $'
suretoundomsg: db 0xD, 0xA, 'Are you sure you want to flash undo.bin to this drive ? (y/n) $'

InvalidMessage: db 0xD,0xA,'Error, please select y/n ',0xD,0xA, '$'
invalidFileErr:db 0xD, 0xA, 'Error, undo.bin is invalid $'


option: db 0
flashedtext:
   db  0xD,0xA
   db 'Uploaded flash to drive. NOW TURN OFF YOUR COMPUTER !!!',0xD,0xA
   db 'Then wait 10 seconds, turn on your computer and  restart this',0xD,0xA
        db 'tool to see if it worked with the same option again: hddhackr -f ',0xD,0xA, '$'
donesaving: db  0xD,0xA
        db 'Saved sectors to file hddss.bin. Now shut down your computer and replace',0xD,0xA
        db 'the HDD with the western digital. Then run:',0xD,0xA
        db ' HddHackr -f',0xD,0xA,'$'
dataerr: db 0xD,0xA,'Error, data on sec 16 till 22 incorrect ',0xD,0xA, '$'
datafileerr: db 0xD,0xA,'Error, data in hddss.bin seems incorrect ',0xD,0xA, '$'
dataok : db 0xD,0xA,'Data on sectors 17 to 22 seems correct ',0xD,0xA, '$'
dataokbool: db 0
datawasok: db 0xD,0xA,'Data on sectors 17 to 22 seemed correct, so didnt touch that ',0xD,0xA, '$'
wrotedata: db 0xD,0xA,'Copied data from hddss.bin to sector 16-22 ',0xD,0xA, '$'
trylegacy: db 0xD,0xA,'No drive found in Enhanced mode, now scanning in Legacy mode',0xD,0xA, '$'
Selectdrvmsg: db 0xD,0xA,'Type the nr of the drive you want to use (1 to 4, x=exit) ', '$'
invalidinput: db 0xD,0xA,'Invalid Input ',0xD,0xA, '$'
head: db 0xA0
gevondenIDE: times 4 db 0
aantalIDE: db 0
searchbool: db 0

Fileexistmsg: db 0xD,0xA,'Undo.bin file already exists, exiting program, see readme file !!! ', '$'

manualmode: db 0xD,0xA,'Port number was edited, using edited port values, please wait ... ',0xD,0xA, '$'
   db 'port='
mprt: db 'MPRT'
