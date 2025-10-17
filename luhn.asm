[org 0x0100]
jmp start
	sum: dw 0
	newArr: db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	len: dw 0
	num: db '4539148803436467',0
	x1: dw 0
	x2: dw 0
	algoMsg: db 'Alpha - Credit Card Validator',0
	message1: db 'Credit Card Number: ',0
	message2: db 'Luhn Algorithm Execution: ',0
	message3: db 'Processed Card Number: ',0
	message4: db 'Sum: ',0
	message5: db ' mod 10 = ',0
	message6: db 'Status: Credit Number Valid. ',0 
	message10: db ' Verification Successfull !',0
	message7: db 'Status: Credit Number Not Valid. ',0
	message11: db ' Verification Not Successfull !',0
	message8: db 'The Credit Card Number is Always 16 Digits.So, Not Valid',0
	message9: db 'This Credit Card Number Contains Non-Digit.So, Not Valid',0
start:
	call clrScreen
	mov di,0x0210
	push algoMsg
	call printAlgoMsg
	mov ax,num
	push ax
	call calcLength
	mov cx,[len]
	cmp cx,16
	je main
	push message8
	call calcLength
	mov cx,[len]
	push cx
	push message8
	call displayStr
	jmp exit
main:
	mov ax,num
	push ax
	call Check
exit:
	mov ax,0x4c00
	int 0x21
printAlgoMsg:
	push bp
	mov bp,sp
	push ax
	push cx
	push si
	push es
	
	mov ax,0xb800
	mov es,ax
	
	mov si,[bp+4]
	push si
	call calcLength
	mov cx,[len]
	
	mov si,[bp+4]
	mov ah,0x0E
	cld
	loop8:	
		lodsb
		stosw
		loop loop8
	call nextRow
	call nextRow
	pop es
	pop si
	pop cx
	pop ax
	pop bp
	ret 4
Check:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push si
	push es
	push di
	
	mov ax,0xb800
	mov es,ax
	
	mov di,0x338
	push message1
	call calcLength
	mov cx,[len]
	push cx
	push message1
	call displayStr
	
	push word[bp+4]
	call calcLength
	mov cx,[len]

	mov si,[bp+4]
	mov bx,0
	xor ax,ax
	loop1:
		mov ah,0x07
		mov al,[si]

		mov ah,0
		cmp al,0x30
		jl notDig
		cmp al,0x39
		jg notDig
		
		sub al,0x30
		mov [newArr+bx],al
		inc si
		inc bx
		cmp bx,cx
		jne loop1
		
		mov [x1],cx
		push cx
		push newArr
		call printArr
		
		jmp continue
	notDig:
		call nextRow
		push message9
		call calcLength
		mov cx,[len]
		push cx
		push message9
		call displayStr
		jmp clear
	continue:
		call nextRow
		call nextRow
		push message2
		call calcLength
		mov cx,[len]
		push cx
		push message2
		call displayStr
		mov cx,[x1]
		
		mov si,cx
		mov bx,0
	checkLoop:
		mov ah,0
		mov al,[newArr+bx]
		
		mov [x1],ax
		push ax
		call SingleDig
		call printArrow
		mov ax,[x1]
		
		shl al,1
		
		mov [x1],ax
		push ax
		call SingleDig
		call printArrow
		mov ax,[x1]
		
		cmp al,10
		jl setNot
		sub al,9
		
		mov [x1],ax
		push ax
		call SingleDig
		call printBar
		mov ax,[x1]
		
		jmp keep
	setNot:
		mov [x1],ax
		push ax
		call SingleDig
		call printBar
		mov ax,[x1]
	keep:
		mov [newArr+bx],al
	next:
		inc bx
		mov al,[newArr+bx]
		push ax
		call SingleDig
		call printBar
		dec bx
		add bx,2
		cmp bx,si
		jne checkLoop
		
		call nextRow
		call nextRow
		mov di,0x654
		mov [x1],cx
		push message3
		call calcLength
		mov cx,[len]
		push cx
		push message3
		call displayStr
		mov cx,[x1]
		
		mov [x1],cx
		push cx
		push newArr
		call printArr
		mov cx,[x1]
		mov si,0
		mov ax,0
	checking:
		add al,[newArr+si]
		inc si
		cmp si,cx
		jne checking
		
		mov [sum],ax
		call nextRow
		call nextRow
		mov di,0x7B2
		mov [x2],cx
		push message4
		call calcLength
		mov cx,[len]
		push cx
		push message4
		call displayStr
		mov ax,[sum]
		push ax
		call MultiDig
		
		cmp ax,0
		je notValid
		
		mov ax,0xb800
		mov es,ax
		mov word[es:di],0x0720
		add di,2
		call printArrow
		mov ax,0xb800
		mov es,ax
		mov word[es:di],0x0720
		add di,2
		mov ax,[sum]
		push ax
		call MultiDig
		push message5
		call calcLength
		mov cx,[len]
		push cx
		push message5
		call displayStr
		
		mov al,[sum]
		mov bl,10
		div bl
		mov [x1],ax
		xor dx,dx
		mov dl,ah
		push dx
		call SingleDig
		mov ax,[x1]
		cmp ah,0
		jne notValid
		
		call nextRow
		call nextRow
		mov di,0x8F0
		push message10
		push message6
		call ValidMsg
		mov cx,[x2]
		jmp clear
	notValid:
		call nextRow
		call nextRow
		mov di,0x8EA
		push message11
		push message7
		call inValidMsg
	clear:
		pop di
		pop es
		pop si
		pop dx
		pop cx
		pop bx
		pop ax
		pop bp
		ret 2
MultiDig:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push si
	push es
	
	mov ax,0xb800
	mov es,ax
	mov ax,[bp+4]
	mov bx,10
	mov cx,0
	loop6:
		mov dx,0
		div bx
		add dl,0x30
		push dx
		inc cx
		cmp ax,0
		jne loop6
	loop7:
		pop dx
		mov dh,0x0B
		mov [es:di],dx
		add di,2
		dec cx
		jnz loop7
	pop es
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 2
ValidMsg:
	push bp
	mov bp,sp
	push ax
	push cx
	push si
	push es
	
	mov ax,0xb800
	mov es,ax
	
	mov si,[bp+4]
	push si
	call calcLength
	mov cx,[len]
	mov si,[bp+4]
	push cx
	push si
	call displayStr
	
	call nextRow
	call nextRow
	
	mov di,0XA30
	mov ax,0xb800
	mov es,ax
	mov word[es:di],0x0AFE
	add di,2
	mov si,[bp+6]
	push si
	call calcLength
	mov cx,[len]
	
	mov si,[bp+6]
	mov ah,0x0A
	cld
	loop9:	
		lodsb
		stosw
		loop loop9
	call nextRow
	call nextRow
	pop es
	pop si
	pop cx
	pop ax
	pop bp
	ret 4
inValidMsg:
	push bp
	mov bp,sp
	push ax
	push cx
	push si
	push es
	
	mov ax,0xb800
	mov es,ax
	
	mov si,[bp+4]
	push si
	call calcLength
	mov cx,[len]
	mov si,[bp+4]
	push cx
	push si
	call displayStr
	
	call nextRow
	call nextRow
	mov di,0xA2A
	
	mov ax,0xb800
	mov es,ax
	mov word[es:di],0x0CFE
	add di,2
	
	mov si,[bp+6]
	push si
	call calcLength
	mov cx,[len]
	
	mov si,[bp+6]
	mov ah,0x0C
	cld
	loop10:	
		lodsb
		stosw
		loop loop10
	call nextRow
	call nextRow
	pop es
	pop si
	pop cx
	pop ax
	pop bp
	ret 4
nextRow:
	push ax
	push bx
	push dx
	
	mov ax, di
	mov bx, 160
	xor dx, dx
	div bx
	sub di, dx
	add di, 160
	
	pop ax
	pop bx
	pop dx
	ret
printArr:
	push bp
	mov bp,sp
	push ax
	push bx
	push si
	push es
	push dx
	
	mov ax,0xb800
	mov es,ax
	mov si,[bp+4]
	mov cx,[bp+6]
	mov dx,0
	loop5:
		xor ax,ax
		mov al,[si]
		push ax
		call SingleDig
		mov word[es:di],0x0720
		add di,2
		inc si
		inc dx
		cmp dx,cx
		jne loop5
	pop dx	
	pop es
	pop si
	pop bx
	pop ax
	pop bp
	ret 4
printBar:
	push bp
	mov bp,sp
	push ax
	push es
	
	mov ax,0xb800
	mov es,ax
	mov ax,0x0720
	mov [es:di],ax
	add di,2
	mov ax,0x0D7C
	mov [es:di],ax
	add di,2
	mov ax,0x0720
	mov [es:di],ax
	add di,2
	pop es
	pop ax
	pop bp
	ret
printArrow:
	push bp
	mov bp,sp
	push ax
	push es
	
	mov ax,0xb800
	mov es,ax
	mov ax,0x0C2D
	mov [es:di],ax
	add di,2
	mov ax,0x0C3E
	mov [es:di],ax
	add di,2
	pop es
	pop ax
	pop bp
	ret
SingleDig:
	push bp
	mov bp,sp
	push es
	push ax
	
	mov ax,0xb800
	mov es,ax
	
	mov ax,[bp+4]
	
	cmp al,10
	jl decimal
	cmp al,16
	je set16
	cmp al,18
	je set18
	jmp set
	set16:
		mov word[es:di],0x0B31
		add di,2
		mov word[es:di],0x0B30
		add di,2
		jmp clear1
	set18:
		mov word[es:di],0x0B31
		add di,2
		mov word[es:di],0x0B32
		add di,2
		jmp clear1
		
	set:
		add al,0x37
		jmp print
	decimal:
		add al,0x30
	print:
		mov ah,0x0B
		mov [es:di],ax
		add di,2
	clear1:
		pop ax
		pop es
		pop bp
		ret 2
displayStr:
	push bp
	mov bp,sp
	push ax
	push cx
	push si
	push es
	
	mov ax,0xb800
	mov es,ax
	
	mov si,[bp+4]
	mov cx,[bp+6]
	mov ah,0x0F
	cld
	loop3:	
		lodsb
		stosw
		loop loop3
	
	pop es
	pop si
	pop cx
	pop ax
	pop bp
	ret 4
clrScreen:
	push ax
	push es
	push di
	
	mov ax,0xb800
	mov es,ax
	mov di,0
	
	loop4:
		mov word[es:di],0x0720
		add di,2
		cmp di,4000
		jne loop4
		
		pop di
		pop es
		pop ax
		ret
calcLength:
	push bp
	mov bp,sp
	push si
	push cx
	
	mov si,[bp+4]
	mov cx,0
	loop2:
		cmp byte[si],0
		je clear2
		inc si
		inc cx
		jmp loop2
	clear2:
		mov [len],cx
		pop cx
		pop si
		pop bp
		ret 2