org 0x100
jmp start
score: db 'Score: '   ;string score
score_count:dw 0      ;score count
slen: dw 7              ;score string length
gover: db 'Game Over!'      ;game over string
glen: dw 10                ;game over string length
direction: dw 0                   ;snake direction flag
snake_body:db 0x2,'*','*','*','*'        ;snake body
body_len:dw 5                     ;snake body length
food_location: dw 0                 ;random location of food
game_name:db 'SNAKE XENZIA'
game_name_len:dw 12
play:db 'press any key to play'
play_len:dw 21 
quotient:dw 0             
row: dw 0
col: dw 0
clrscr:
    push es
    push ax
    push cx
    push di
    mov  ax, 0xb800
    mov  es, ax            ; point es to video base
    xor  di, di            ; point di to top left column
    mov  ax, 0x0720        ; space char in normal attribute
    mov  cx, 2000          ; number of screen locations
    cld                    ; auto increment mode
    rep  stosw             ; clear the whole screen
    pop  di
    pop  cx
    pop  ax
    pop  es
    ret
; its length as parameters 
printstr:     push bp 
              mov  bp, sp 
              push es 
              push ax 
              push cx 
              push si 
              push di 
 
              mov  ax, 0xb800 
              mov  es, ax             ; point es to video base  
              mov ax,[bp+4]           ;load location of string
              mov  di,ax              ; point di to required location 
              mov  si, [bp+10]         ; point si to string 
              mov  cx, [bp+8]         ; load length of string in cx 
              mov  ah, [bp+6]         ; load attribute in ah 
 
              cld                     ; auto increment mode 
nextchar:     lodsb                   ; load next char in al 
              stosw                   ; print char/attribute pair 
              loop nextchar           ; repeat for the whole string 
 
              pop  di 
              pop  si 
              pop  cx 
              pop  ax 
              pop  es 
              pop  bp 
              ret  8
printSnake:
    push bp
    mov bp, sp
    push ax
    push bx
    push si
    push cx
    push dx

    mov si, [bp + 6]        ;snake
    mov cx, [bp + 8]        ;length of snake
    mov di, 1996
    mov ax, 0xb800
    mov es, ax

    mov bx, [bp + 4]
    mov ah, 0x07
    snake_next_part:
        mov al, [si]
        mov [es:di], ax
        mov [bx], di
        inc si
        add bx, 2
        add di, 2
        loop snake_next_part

    pop dx
    pop cx
    pop si
    pop bx
    pop ax
    pop bp
    ret 6

move_snake_up:
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx
    push es
    push di
    push si
     ;snake_parts colision detection
    mov bx, [bp + 4]            ;snake location
    mov dx, [bx]

    mov cx, [bp + 8]
    sub dx, 160
    check_up_colision:
        cmp dx, [bx]
        je no_up_movement
        add bx, 2
        loop check_up_colision
    upward_movement:
    mov si, [bp + 6]            ;snake 
    mov bx, [bp + 4]            ;snake location
    mov dx, [bx]
    sub dx, 160
    mov di, dx

    mov ax, 0xb800
    mov es, ax
    mov ah, 0x07
    mov al, [si]
    mov [es:di],ax             ;snake head placed

    mov cx, [bp + 8]
    mov di, [bx]
    inc si
    mov ah, 0x07
    mov al, [si]
    mov [es:di],ax
    up_location_sort:
        mov ax, [bx]
        mov [bx], dx
        mov dx, ax
        add bx, 2
        loop up_location_sort
    mov di, dx
    mov ax, 0x0720
    mov [es:di], ax
    no_up_movement:
	cmp dx, [bx]
	jne skipUp
	call gameover
	skipUp:
    pop si
    pop di
    pop es
    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret 6
move_snake_down:
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx
    push es
    push di
    push si
     ;snake_parts colision detection
    mov bx, [bp + 4]            ;snake location
    mov dx, [bx]

    mov cx, [bp + 8]
    add dx, 160
    check_down_colision:
        cmp dx, [bx]
        je no_down_movement
        add bx, 2
        loop check_down_colision

    downward_movement:
    mov si, [bp + 6]            ;snake 
    mov bx, [bp + 4]            ;snake location
    mov dx, [bx]
    add dx, 160
    mov di, dx

    mov ax, 0xb800
    mov es, ax
    mov ah, 0x07
    mov al, [si]
    mov [es:di], ax             ;snake head placed

    mov cx, [bp + 8]            ;snake length
    mov di, [bx]
    inc si
    mov ah, 0x07
    mov al, [si]
    mov [es:di],ax
    down_location_sort:
        mov ax, [bx]
        mov [bx], dx
        mov dx, ax
        add bx, 2
        loop down_location_sort
    mov di, dx
    mov ax, 0x0720
    mov [es:di], ax

    no_down_movement:
	cmp dx, [bx]
	jne skipDown
	call gameover
	skipDown:
    pop si
    pop di
    pop es
    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret 6
move_snake_left:
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx
    push es
    push di
    push si
    ;snake_parts colision detection
    mov bx, [bp + 4]            ;snake location
    mov dx, [bx]

    mov cx, [bp + 8]
    sub dx, 2
    check_left_colision:
        cmp dx, [bx]
        je no_left_movement
        add bx, 2
        loop check_left_colision
    left_movement:
    mov si, [bp + 6]            ;snake 
    mov bx, [bp + 4]            ;snake location
    mov dx, [bx]
    sub dx, 2
    mov di, dx

    mov ax, 0xb800
    mov es, ax
    mov ah, 0x07
    mov al, [si]
    mov [es:di],ax             ;snake head placed

    mov cx, [bp + 8]
    mov di, [bx]
    inc si
    mov ah, 0x07
    mov al, [si]
    mov [es:di],ax
    left_location_sort:
        mov ax, [bx]
        mov [bx], dx
        mov dx, ax
        add bx, 2
        
        loop left_location_sort
    mov di, dx
    mov ax, 0x0720
    mov [es:di], ax

    no_left_movement:
	cmp dx, [bx]
	jne skipLeft
	call gameover
	skipLeft:
    pop si
    pop di
    pop es
    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret 6
move_snake_right:
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx
    push es
    push di
    push si
    ;snake_parts colision detection
    mov bx, [bp + 4]            ;snake location
    mov dx, [bx]

    mov cx, [bp + 8]
    add dx, 2
    check_right_colision:
        cmp dx, [bx]
        je no_right_movement
        add bx, 2
        loop check_right_colision

    right_movement:
    mov si, [bp + 6]            ;snake 
    mov bx, [bp + 4]            ;snake location
    mov dx, [bx]
    add dx, 2
    mov di, dx

    mov ax, 0xb800
    mov es, ax
    mov ah, 0x07
    mov al, [si]
    mov [es:di], ax             ;snake head placed

    mov cx, [bp + 8]            ;snake length
    mov di, [bx]
    inc si
    mov ah, 0x07
    mov al, [si]
    mov [es:di],ax
    right_location_sort:
        mov ax, [bx]
        mov [bx], dx
        mov dx, ax
        add bx, 2
        
        loop right_location_sort
    mov di, dx
    mov ax, 0x0720
    mov [es:di], ax

    no_right_movement:
	cmp dx, [bx]
	jne skipRight
	call gameover
	skipRight:
    pop si
    pop di
    pop es
    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret 6
move_snake:
    ; Get keystroke
    push ax
    push bx

    l1:
	    call delay
		call delay
	    mov ah,1
		int 0x16
		jz skip
		mov ah, 0
		int 0x16
		cmp ah,0x48
		jz u
		cmp ah,0x50
		jz d
		cmp ah,0x4B
		jz l
		cmp ah,0x4D
		jz r
		cmp ah,1
		jz exit1
		u:
			mov word[direction],2
			jmp skip
		d:
			mov word[direction],3
			jmp skip
		l:
			mov word[direction],1
			jmp skip
		r:
			mov word[direction],0
			jmp skip
	exit1:
	    jmp exit2
    skip:
	    mov ax,[direction]
	    cmp ax,2
		jz up
		cmp ax,3
		jz down
		cmp ax,1
		jz left
		cmp ax,0
		jz right
	;UpWard Movement
    up:
        push word [body_len]
        mov bx, snake_body
        push bx
        mov bx, snake_location
        push bx
        call move_snake_up
		call checkover
        jmp snake_eat_fruit

    down:
        push word [body_len]
        mov bx, snake_body
        push bx
        mov bx, snake_location
        push bx
        call move_snake_down
		call checkover
        jmp snake_eat_fruit

    left:
        push word [body_len]
        mov bx, snake_body
        push bx
        mov bx, snake_location
        push bx
        call move_snake_left
        jmp snake_eat_fruit

    right:
        push word [body_len]
        mov bx, snake_body
        push bx
        mov bx, snake_location
        push bx
        call move_snake_right
        jmp snake_eat_fruit
    snake_eat_fruit:
        push word [food_location]
        push word [body_len]
        mov bx, snake_body
        push bx
        mov bx, snake_location
        push bx
        call on_eat_food_inc_size
        jmp l1
    exit2:
        pop bx
        pop ax
        ret
printRec:
    push ax
	push cx
	push si
	push di
    
    mov ax,0xb800
    mov es,ax 
    len1:
	    mov ax,0x032D
        mov cx,80
	    mov di,320
        nextchar1:
	        rep stosw
	len2:
	    mov ax,0x032D
        mov cx,80
	    mov di,3840
        nextchar2:
	        rep stosw
	
	pop di
	pop si
	pop cx
	pop ax
	ret
printScore:
    push bp
	mov bp,sp
    push ax 
	push bx
    push cx
    push dx	
    push si 
    push di 
 
    mov  ax, 0xb800 
    mov  es, ax             ; point es to video base 
	mov  di,230              ; point di to required location 
    mov  si,score         ; point si to string 
    mov  cx, [slen]         ; load length of string in cx 
    mov  ah,0x03         ; load attribute in ah
    cld                     ; auto increment mode 
    nextchar5:     
        lodsb                   ; load next char in al 
        stosw                   ; print char/attribute pair
        loop nextchar5		
	mov  ax, [bp+4]         ; load number in ax 
    mov  bx, 10             ; use base 10 for division 
    mov  cx, 0              ; initialize count of digits 
    nextdigit:    
        mov  dx, 0              ; zero upper half of dividend 
        div  bx                 ; divide by 10 
        add  dl, 0x30           ; convert digit into ascii value 
        push dx                 ; save ascii value on stack 
        inc  cx                 ; increment count of values  
        cmp  ax, 0              ; is the quotient zero 
        jnz  nextdigit          ; if no divide it again
    nextpos:      
	    pop  dx                 ; remove a digit from the stack 
        mov  dh, 0x07           ; use normal attribute 
        mov [es:di], dx         ; print char on screen 
        add  di, 2              ; move to next screen location 
        loop nextpos            ; repeat for all digits on stack
    pop  di 
    pop  si 
	pop  dx
    pop  cx 
	pop  bx
    pop  ax
    pop  bp	
    ret  2	
delay:
    push dx
    mov dx,0
    jm:
        inc dx
        cmp dx,0xffff
    jne jm
    pop dx
    ret
;Increase Snake Size
on_eat_food_inc_size:
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx
    push es
    push di
    push si

    mov bx, [bp + 4]      ;snake location
    mov dx, [bp + 10]     ;food location

    cmp [bx], dx
    jne not_increase_size
    ;else
	
    mov cx, [bp + 8]        ;snake length
    shl cx, 1
    sub cx, 2
    add bx, cx      
    mov dx, [bx]
    sub dx, [bx - 2]        ;last - second last

    mov ax, [bx]
    add ax, dx          
    mov dx, ax
 
    add cx, 2
    shr cx,1
    inc cx
    mov [body_len], cx

    add bx, 2
    mov [bx], dx
    mov si, [bp + 6]
    inc si

    mov ax, 0xb800
    mov es, ax
    mov di, dx
    mov ah, 0x07
    mov al, [si]

    mov [es:di], ax
	call randgen 
    push word [food_location]
    call display_food
    ;call Increase_score
    add word [score_count], 1

    push word [score_count]
    call printScore
    not_increase_size:
    pop si
    pop di
    pop es
    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret 8
display_food:
    push bp
    mov bp, sp
    push ax
    push di
    push es

    mov ax, 0xb800
    mov es, ax
    mov di, [bp + 4]        ;food location
    mov ax, 0x0309 
    mov [es:di], ax

    pop es
    pop di
    pop ax
    pop bp
    ret 2
randgen:
   mov ah, 00h  ; interrupts to get system time        
   int 1ah      ; CX:DX now hold number of clock ticks since midnight      
   mov  ax, dx
   xor  dx, dx
   mov  cx, 1680 
   div  cx       ; Here dx contains the remainder of the division - from 0 to 1680
   add  dx, 240   ; Adjust the range start to 240
   shl dx, 1 ;range set to 480-3840
   mov word[food_location],dx
   ;calculation of row and column from di value
   ret
		 
checkover:
   push ax
   push bx
   push cx
   push dx
   mov dx,480
   mov bx,3840
   mov ax,[snake_location]
   cmp dx,ax
   jge gameover
   cmp bx,ax
   jle gameover
   pop dx
   pop cx
   pop bx
   pop ax
   ret
gameover:
    push ax
    push bx
    push cx
    push dx
    push si
    push di
	mov ax,0xb800
	mov es,ax
	mov di,1994
	mov si,gover
	mov cx,[glen]
	mov ah,0x04
	nextch:
	    lodsb
		stosw
		loop nextch
		
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    mov ax, 0x4c00
    int 0x21
    ret
welcome:
    push ax
    push bx
    push cx
    push dx
	mov ax,game_name
	push ax
	push word[game_name_len]
	mov ax,0x70
	push ax	
	mov ax,1170
	push ax
	call printstr
	mov ax,play
	push ax
	push word[play_len]
	mov ax,0x87
	push ax	
	mov ax,2282
	push ax
	call printstr
	pop dx
    pop cx
    pop bx
    pop ax
	ret
	
start:
    call clrscr
	l0:       ;loop for Intro on screen
	  call welcome     ;function to print strings
	  mov ah,1            ;detect if any key is pressed
	  int 0x16
	  jz l0
	call clrscr
	mov word[direction],1       ;set default direction of snake to left
	push word[score_count]      ;push score value to stack
	call printScore             ;subroutine to print score
	call printRec                ;print borders
	call randgen
	push word[food_location]
	call display_food
    push word [body_len]
    mov ax, snake_body
    push ax
    mov ax, snake_location
    push ax
    call printSnake        ; call the printstr subroutine
    call move_snake
mov ax, 0x4c00
int 0x21
snake_location:dw 0