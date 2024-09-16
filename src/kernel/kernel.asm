org 0x0 ; kernel loaded with 0 offset
bits 16 ; emit 16-bit code

%define ENDL 0X0D, 0xA ; CRLF


start:

    ; print message
    mov si, msg_hello ; set si to point to msg_hello
    call puts ; call puts with si

.halt:
    cli
    hlt


; Prints string to screen
; Params:
;   ds:si - pointer to string
puts:
    push si
    push ax
    push bx

.loop:
    lodsb ; load byte from ds:si into al, increment si
    or al, al ; set zero flag if al is 0
    jz .done ; jump to done if al is 0

    mov ah , 0x0E ; enter tty mode
    mov bh, 0 ; page number
    int 0x10 ; call BIOS interrupt

    jmp .loop ; jump back to loop

.done:
    pop bx
    pop ax
    pop si
    ret

msg_hello: db 'Hello from chaOS!', ENDL, 0