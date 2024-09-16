org 0x7C00 ; BIOS loads at 0x7C00 always - set origin (offset)

bits 16 ; emit 16-bit code

%define ENDL 0X0D, 0xA ; CRLF

;
; FAT-12 Header
;
jmp short start
nop ; 3 byte padding

bdm_oem:                    db 'MSWIN4.1' ; OEM string as Windows 4.1 for max compatibility
bdb_bytes_per_sector:       dw 512
bdb_sectors_per_cluster:    db 1
bdb_reserved_sectors:       dw 1
bdb_fat_count:              db 2
bdb_dir_entries_count:      dw 0E0h
bdb_total_sectors:          dw 2880 ; 2880 * 512 = 1.44MB
bdb_media_descriptor_type:  db 0F0h ; 3.5" Floppy
bdb_sectors_per_fat:        dw 9
bdb_sectors_per_track:      dw 18
bdb_heads:                  dw 2
bdb_hidden_sectors:         dd 0
bdb_large_sector_count:     dd 0

; extended boot record
ebr_drive_number:           db 0 ; 0x00 for floppy, 0x80 for hard drive
                            db 0 ; reserved
ebr_signature:              db 29h ; extended boot record signature
ebr_volume_id:              db 53h, 28h, 14h, 76h ; volume serial number (can be any number)
ebr_volume_label:           db 'chaOS      ' ; volume label (11 bytes)
                            db 'FAT12   ' ; file system type (8 bytes)


start:
    mov ax, 0 ; write to register ax
    mov ds, ax ; set data segment to 0
    mov es, ax ; set extra segment to 0

    ; setup stack
    mov ss, ax ; set stack segment to 0
    mov sp, 0x7C00 ; set stack pointer to 0x7C00

    ; BIOS offset fix
    push es
    push word .after
    retf
.after:

    ; read from floppy disk
    ; BIOS should set DL to drive number
    mov [ebr_drive_number], dl ; set drive number in ebr

    ; print message
    mov si, msg_load
    call puts

    ; read drive params on BOOT rather than rely on data from DISK
    push es
    mov ah, 08h
    int 13h
    jc floppy_error
    pop es

    and cl, 0x3F                            ; mask out high bits
    xor ch, ch
    mov [bdb_sectors_per_track], cx         ; set sector count

    inc dh
    mov [bdb_heads], dh                     ; set head count

    ; compute LBA of FAT root directory
    mov ax, [bdb_sectors_per_fat]           ; read FAT sectors
    mov bl, [bdb_fat_count]
    xor bh, bh
    mul bx                                 ; ax = (FAT sectors * FAT count)
    add ax, [bdb_reserved_sectors]         ; ax = LBA address of FAT root directory
    push ax

    ; compute root directory size (number of sectors * 32) / bytes per sector
    mov ax, [bdb_dir_entries_count]
    shl ax, 5                              ; ax = FAT sectors * 32
    xor dx, dx
    div word [bdb_bytes_per_sector]        ; sectors need to be read

    test dx, dx                            ; check if dx not 0, add 1
    jz .root_dir_after
    inc ax                                 ; add 1 to ax
                                           ; partially full sector

.root_dir_after:

    ; read root directory
    mov cl, al                             ; number of sectors to read (root directory size)
    pop ax
    mov dl, [ebr_drive_number]             ; drive number
    mov bx, buffer                         ; buffer address
    call disk_read

    ; search for kernel file
    xor bx, bx
    mov di, buffer

.search_kernel:
    mov si, file_stage2_bin                ; kernel file name
    mov cx, 11                             ; compare 11 bytes
    push di
    repe cmpsb                             ; compare string
    pop di
    je .kernel_found

    add di, 32                              ; move to next entry
    inc bx
    cmp bx, [bdb_dir_entries_count]
    jl .search_kernel

    ; kernel not found
    jmp kernel_not_found_error




.kernel_found:
    ; di points to kernel file entry
    mov ax, [di + 26]                  ; LBA address of kernel file (offset 26)
    mov [stage2_cluster], ax

    ; load FAT from disk into memory
    mov ax, [bdb_reserved_sectors]     ; LBA address of FAT root directory
    mov bx, buffer                     ; buffer address
    mov cl, [bdb_sectors_per_fat]      ; number of sectors to read
    mov dl, [ebr_drive_number]         ; drive number
    call disk_read

    ; read kernel and process FAT chain
    mov bx, KERNEL_LOAD_SEGMENT
    mov es, bx
    mov bx, KERNEL_LOAD_OFFSET
    
.load_kernel_loop:

    ; Read next cluster
    mov ax, [stage2_cluster]
    add ax, 31                        ; first cluster = (stage2_cluster - 2) * sectors per cluster + start sector
                                      ; start sector = rserved + fats + root dir size = 1 + 18 + 134 = 33
    mov cl, 1
    mov dl, [ebr_drive_number]
    call disk_read

    add bx, [bdb_bytes_per_sector]
    
    ; get location of next cluster
    mov ax, [stage2_cluster]
    mov cx, 3
    mul cx
    mov cx, 2
    div cx                            ; ax = index of FAT entry, dx = cluster % 2

    mov si, buffer
    add si, ax
    mov ax, [ds:si]                  ; read FAT entry at index ax

    or dx, dx
    jz .even

.odd:
    shr ax, 4                       ; shift right 4 bits
    jmp .next_cluster

.even:
    and ax, 0x0FFF                   ; mask out low 4 bits

.next_cluster:
    cmp ax, 0x0FF8                   ; check if last cluster
    jae .read_finish

    mov [stage2_cluster], ax
    jmp .load_kernel_loop

.read_finish:
    ; jump to kernel
    mov dl, [ebr_drive_number]      ; boot device in dl

    ; set segment registers
    mov ax, KERNEL_LOAD_SEGMENT     ; set segment registers
    mov ds, ax
    mov es, ax

    jmp KERNEL_LOAD_SEGMENT:KERNEL_LOAD_OFFSET

    jmp wait_key_reboot ; should never reach here


    cli ; disable interrupts
    hlt


;
; Error handling
;

floppy_error:
mov si, msg_read_failed
    call puts
    jmp wait_key_reboot

kernel_not_found_error:
    mov si, msg_stage2_not_found
    call puts
    jmp wait_key_reboot

wait_key_reboot:
    mov ah, 0
    int 16h                             ; wait for key press
    jmp 0FFFFh:0                        ; reboot

.halt:
    cli                               ; disable interrupts
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


;
; Disk routines
;

;
; Converts LBA addr to CHS addr
; Params:
;   - ax: LBA address
; Returns:
;   - cx: [0-5]: sector number
;   - cx: [6-15]: cylinder
;   - dh: head

lba_to_chs:

    push ax
    push dx

    xor dx, dx ; set dx to 0
    div word [bdb_sectors_per_track]    ; ax => LBA / SectorsPerTrack
                                        ; dx => LBA % SectorsPerTrack

    inc dx                              ; dx = (LBA % SectorsPerTrack) + 1 (sector number)
    mov cx, dx                          ; cx = sector number
    
    xor dx, dx                          ; dx = 0
    div word [bdb_heads]                ; ax => (LBA / SectorsPerTrack) / Heads (cylinder)
                                        ; dx => (LBA / SectorsPerTrack) % Heads (head)
    mov dh, dl                          ; dh = head
    mov ch, al                          ; ch = cylinder lower 8 bits
    shl ah, 6                           ; shift ah 6 bits to the left
    or  cl, ah                          ; cl = cylinder upper 2 bits

    pop ax
    mov dl, al                         ; restore dl and al
    pop ax
    ret

;
; Reads sectors from a disk
; Parameters:
;   - ax: LBA address
;   - cl: number of sectors to read
;   - dl: drive number
;   - es:bx: buffer to read sectors into

disk_read:

    push ax                             ; save registers
    push bx
    push cx
    push dx
    push di

    push cx                             ; temporarily save cl
    call lba_to_chs                     ; convert LBA to CHS
    pop ax                              ; AL = number of sectors to read

    mov ah, 02h                         ; read sectors
    mov di, 3                           ; number of retries

.retry:
    pusha                              ; save registers
    stc                                ; set carry flag
    int 13h                            ; call BIOS interrupt
    jnc .done

    ; read failed
    popa
    call disk_reset

    dec di
    test di, di
    jnz .retry

.fail:
    ; attempts exhausted
    jmp floppy_error

.done:
    popa                               ; restore registers

    pop di                             ; save registers
    pop dx
    pop cx
    pop bx
    pop ax
    ret

;
; Resets disk controller
; Parameters:
;   - dl: drive number

disk_reset:
    pusha
    mov ah, 0
    stc
    int 13h
    jc floppy_error
    popa
    ret


msg_load:            db 'Load ~chaOS', ENDL, 0
msg_read_failed:        db 'Read from disk failed!', ENDL, 0
msg_stage2_not_found:   db 'STAGE2.BIN file not found!', ENDL, 0
file_stage2_bin:        db 'STAGE2  BIN'
stage2_cluster:                         dw 0

KERNEL_LOAD_SEGMENT:                    equ 0x2000
KERNEL_LOAD_OFFSET:                     equ 0x0000


times 510-($-$$) db 0 ; pad the rest of the sector with 0s

dw 0AA55h 
; db 0x55, 0xAA ; boot sector signature
; dw 0xAA55 ; boot sector signature (using word)

buffer: 