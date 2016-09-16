	.cstring
LC0:
	.ascii "Stack overflow!\0"
LC1:
	.ascii "Stack underflow!\0"
	.align 3
LC2:
	.ascii "Expected vector while creating closure!\0"
	.section __TEXT,__text_cold,regular,pure_instructions
LCOLDB3:
	.text
LHOTB3:
	.align 4,0x90
	.globl _execute
_execute:
LFB4:
	pushq	%r14
LCFI0:
	pushq	%rbp
LCFI1:
	movl	%esi, %ebp
	pushq	%rbx
LCFI2:
	movq	%rdi, %rbx
	movl	$8192, %edi
	subq	$224, %rsp
LCFI3:
	call	_malloc
	testl	%ebp, %ebp
	jle	L1
	xorl	%r8d, %r8d
	xorl	%ecx, %ecx
	jmp	L14
	.align 4,0x90
L22:
	jb	L5
	cmpl	$3, %edx
	jne	L3
	testl	%r8d, %r8d
	je	L9
	leal	-1(%r8), %r9d
	movslq	%r9d, %rdx
	salq	$5, %rdx
	addq	%rax, %rdx
	movq	8(%rdx), %rsi
	cmpl	$2, (%rdx)
	movq	%rsi, 8(%rsp)
	movq	16(%rdx), %rsi
	movq	%rsi, 16(%rsp)
	jne	L21
	movq	8(%rsp), %r10
	movl	%edi, 56(%rsp)
	cmpl	$255, %r9d
	movq	56(%rsp), %r14
	movq	32(%rsp), %r11
	movq	%r10, %rsi
	movq	%r10, 40(%rsp)
	movq	16(%rsp), %r10
	movq	%r14, 216(%rsp)
	movq	%r10, 48(%rsp)
	jg	L8
	movl	%edi, 216(%rsp)
	movq	%rsi, 8(%rdx)
	movq	216(%rsp), %rsi
	movq	%r11, (%rdx)
	movq	%r10, 16(%rdx)
	movq	%rsi, 24(%rdx)
L3:
	cmpl	%ebp, %ecx
	jge	L1
L14:
	movslq	%ecx, %rdx
	salq	$4, %rdx
	addq	%rbx, %rdx
	movq	8(%rdx), %rdi
	movl	(%rdx), %edx
	cmpl	$1, %edx
	jne	L22
	movq	%rdi, %xmm0
	cmpl	$255, %r8d
	movq	$0, 96(%rsp)
	cvttsd2si	%xmm0, %edi
	movl	$2, 96(%rsp)
	movq	96(%rsp), %rdx
	movq	$0, 104(%rsp)
	movq	%rdx, 160(%rsp)
	movl	%edi, 104(%rsp)
	movq	104(%rsp), %rdx
	movq	%rdx, 168(%rsp)
	jg	L8
	movl	$2, 160(%rsp)
	movq	160(%rsp), %rsi
	movslq	%r8d, %rdx
	salq	$5, %rdx
	movl	%edi, 168(%rsp)
	addq	%rax, %rdx
	movq	%rsi, (%rdx)
	movq	168(%rsp), %rsi
L18:
	addl	$1, %ecx
	addl	$1, %r8d
	movq	%rsi, 8(%rdx)
	cmpl	%ebp, %ecx
	movq	$0, 16(%rdx)
	movq	$0, 24(%rdx)
	jl	L14
L1:
	addq	$224, %rsp
LCFI4:
	popq	%rbx
LCFI5:
	popq	%rbp
LCFI6:
	popq	%r14
LCFI7:
	ret
	.align 4,0x90
L5:
LCFI8:
	movq	$0, 64(%rsp)
	cmpl	$255, %r8d
	movl	$1, 64(%rsp)
	movq	64(%rsp), %rdx
	movq	$0, 72(%rsp)
	movl	%edi, 72(%rsp)
	movq	%rdx, 128(%rsp)
	movq	72(%rsp), %rdx
	movq	%rdx, 136(%rsp)
	jg	L8
	movl	$1, 128(%rsp)
	movq	128(%rsp), %rsi
	movslq	%r8d, %rdx
	salq	$5, %rdx
	movl	%edi, 136(%rsp)
	addq	%rax, %rdx
	movq	%rsi, (%rdx)
	movq	136(%rsp), %rsi
	jmp	L18
L8:
	leaq	LC0(%rip), %rdi
	call	_puts
	movl	$1, %edi
	call	_exit
L21:
	leaq	LC2(%rip), %rdi
	call	_puts
	movl	$1, %edi
	call	_exit
L9:
	leaq	LC1(%rip), %rdi
	call	_puts
	movl	$1, %edi
	call	_exit
LFE4:
	.section __TEXT,__text_cold,regular,pure_instructions
LCOLDE3:
	.text
LHOTE3:
	.section __TEXT,__text_cold,regular,pure_instructions
LCOLDB4:
	.section __TEXT,__text_startup,regular,pure_instructions
LHOTB4:
	.align 4
	.globl _main
_main:
LFB7:
	xorl	%eax, %eax
	ret
LFE7:
	.section __TEXT,__text_cold,regular,pure_instructions
LCOLDE4:
	.section __TEXT,__text_startup,regular,pure_instructions
LHOTE4:
	.section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EH_frame1:
	.set L$set$0,LECIE1-LSCIE1
	.long L$set$0
LSCIE1:
	.long	0
	.byte	0x1
	.ascii "zR\0"
	.byte	0x1
	.byte	0x78
	.byte	0x10
	.byte	0x1
	.byte	0x10
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.byte	0x90
	.byte	0x1
	.align 3
LECIE1:
LSFDE1:
	.set L$set$1,LEFDE1-LASFDE1
	.long L$set$1
LASFDE1:
	.long	LASFDE1-EH_frame1
	.quad	LFB4-.
	.set L$set$2,LFE4-LFB4
	.quad L$set$2
	.byte	0
	.byte	0x4
	.set L$set$3,LCFI0-LFB4
	.long L$set$3
	.byte	0xe
	.byte	0x10
	.byte	0x8e
	.byte	0x2
	.byte	0x4
	.set L$set$4,LCFI1-LCFI0
	.long L$set$4
	.byte	0xe
	.byte	0x18
	.byte	0x86
	.byte	0x3
	.byte	0x4
	.set L$set$5,LCFI2-LCFI1
	.long L$set$5
	.byte	0xe
	.byte	0x20
	.byte	0x83
	.byte	0x4
	.byte	0x4
	.set L$set$6,LCFI3-LCFI2
	.long L$set$6
	.byte	0xe
	.byte	0x80,0x2
	.byte	0x4
	.set L$set$7,LCFI4-LCFI3
	.long L$set$7
	.byte	0xa
	.byte	0xe
	.byte	0x20
	.byte	0x4
	.set L$set$8,LCFI5-LCFI4
	.long L$set$8
	.byte	0xe
	.byte	0x18
	.byte	0x4
	.set L$set$9,LCFI6-LCFI5
	.long L$set$9
	.byte	0xe
	.byte	0x10
	.byte	0x4
	.set L$set$10,LCFI7-LCFI6
	.long L$set$10
	.byte	0xe
	.byte	0x8
	.byte	0x4
	.set L$set$11,LCFI8-LCFI7
	.long L$set$11
	.byte	0xb
	.align 3
LEFDE1:
LSFDE3:
	.set L$set$12,LEFDE3-LASFDE3
	.long L$set$12
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB7-.
	.set L$set$13,LFE7-LFB7
	.quad L$set$13
	.byte	0
	.align 3
LEFDE3:
	.subsections_via_symbols
