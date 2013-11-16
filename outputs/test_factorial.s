.data
	
str3:
	.asciz "recursive and iterative give different result\n"
	
str2:
	.asciz "factorial and iterative give same result\n"
	
str1:
	.asciz "%i\n"
	
	.text
	.global main
	
main:
	@ Function main
	@ Local variable offsets and line of death:
	@   this : 4   	l. 0
	@   fr : -28   	l. 22
	@   fi : -32   	l. 25
	@   _t1 : -36   	l. 4
	@   _t2 : -40   	l. 5
	@   _t3 : -44   	l. 7
	@   _t4 : -48   	l. 8
	@   _t5 : -52   	l. 10
	@   _t6 : -56   	l. 13
	@   _t7 : -60   	l. 12
	@   _t8 : -64   	l. 13
	@   _t9 : -68   	l. 14
	@   _t10 : -72   	l. 16
	@   _t11 : -76   	l. 17
	@   _t12 : -80   	l. 22
	@   _t13 : -84   	l. 23
	@   _t14 : -88   	l. 25
	@   _t15 : -92   	l. 26
	
	stmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,lr}
	add fp,sp,#32
	sub sp,fp,#100
	
	@ line 1: fr = new FactorialRecursive()
	@ rallocs: 
	mov a1,#0
	bl _Znwj(PLT)
	mov v7,a1
	
	@ line 2: fi = new FactorialIterative()
	@ rallocs: v7=fr
	mov a1,#0
	bl _Znwj(PLT)
	mov v6,a1
	
	@ line 3: _t1 = 5
	@ rallocs: v6=fi v7=fr
	mov v5,#5
	
	@ line 4: _t2 = FactorialRecursive_0 (fr, _t1)
	@ rallocs: v5=_t1 v6=fi v7=fr
	mov a1,v7
	mov a2,v5
	sub sp,sp,#8
	bl FactorialRecursive_0
	add sp,sp,#8
	mov v4,a1
	
	@ line 5: println(_t2)
	@ rallocs: v4=_t2 v6=fi v7=fr
	ldr a1,=str1
	mov a2,v4
	bl printf(PLT)
	
	@ line 6: _t3 = 5
	@ rallocs: v6=fi v7=fr
	mov v5,#5
	
	@ line 7: _t4 = FactorialIterative_0 (fi, _t3)
	@ rallocs: v5=_t3 v6=fi v7=fr
	mov a1,v6
	mov a2,v5
	sub sp,sp,#8
	bl FactorialIterative_0
	add sp,sp,#8
	mov v4,a1
	
	@ line 8: println(_t4)
	@ rallocs: v4=_t4 v6=fi v7=fr
	ldr a1,=str1
	mov a2,v4
	bl printf(PLT)
	
	@ line 9: _t5 = 10
	@ rallocs: v6=fi v7=fr
	mov v5,#10
	
	@ line 10: _t6 = FactorialRecursive_0 (fr, _t5)
	@ rallocs: v5=_t5 v6=fi v7=fr
	mov a1,v7
	mov a2,v5
	sub sp,sp,#8
	bl FactorialRecursive_0
	add sp,sp,#8
	mov v4,a1
	
	@ line 11: _t7 = 10
	@ rallocs: v4=_t6 v6=fi v7=fr
	mov v5,#10
	
	@ line 12: _t8 = FactorialIterative_0 (fi, _t7)
	@ rallocs: v4=_t6 v5=_t7 v6=fi v7=fr
	mov a1,v6
	mov a2,v5
	sub sp,sp,#8
	bl FactorialIterative_0
	add sp,sp,#8
	mov v3,a1
	
	@ line 13: _t9 = _t6 == _t8
	@ rallocs: v3=_t8 v4=_t6 v6=fi v7=fr
	cmp v4,v3
	moveq v5,#1
	movne v5,#0
	
	@ line 14: If (_t9 == false) goto 1
	@ rallocs: v5=_t9 v6=fi v7=fr
	cmp v5,#0
	beq L1
	
	@ line 15: _t10 = 10
	@ rallocs: v6=fi v7=fr
	mov v5,#10
	
	@ line 16: _t11 = FactorialRecursive_0 (fr, _t10)
	@ rallocs: v5=_t10 v6=fi v7=fr
	mov a1,v7
	mov a2,v5
	sub sp,sp,#8
	bl FactorialRecursive_0
	add sp,sp,#8
	mov v4,a1
	
	@ line 17: println(_t11)
	@ rallocs: v4=_t11 v6=fi v7=fr
	ldr a1,=str1
	mov a2,v4
	bl printf(PLT)
	
	@ line 18: println("factorial and iterative give same result")
	@ rallocs: v6=fi v7=fr
	ldr a1,=str2
	bl printf(PLT)
	
	@ line 19: goto 2
	@ rallocs: v6=fi v7=fr
	b L2
	
	@ line 20: L 1:
	@ rallocs: v6=fi v7=fr
	
L1:
	
	@ line 21: _t12 = 10
	@ rallocs: v6=fi v7=fr
	mov v5,#10
	
	@ line 22: _t13 = FactorialRecursive_0 (fr, _t12)
	@ rallocs: v5=_t12 v6=fi v7=fr
	mov a1,v7
	mov a2,v5
	sub sp,sp,#8
	bl FactorialRecursive_0
	add sp,sp,#8
	mov v4,a1
	
	@ line 23: println(_t13)
	@ rallocs: v4=_t13 v6=fi
	ldr a1,=str1
	mov a2,v4
	bl printf(PLT)
	
	@ line 24: _t14 = 10
	@ rallocs: v6=fi
	mov v7,#10
	
	@ line 25: _t15 = FactorialIterative_0 (fi, _t14)
	@ rallocs: v6=fi v7=_t14
	mov a1,v6
	mov a2,v7
	sub sp,sp,#8
	bl FactorialIterative_0
	add sp,sp,#8
	mov v5,a1
	
	@ line 26: println(_t15)
	@ rallocs: v5=_t15
	ldr a1,=str1
	mov a2,v5
	bl printf(PLT)
	
	@ line 27: println("recursive and iterative give different result")
	@ rallocs: 
	ldr a1,=str3
	bl printf(PLT)
	
	@ line 28: L 2:
	@ rallocs: 
	
L2:
	
.L1:
	sub sp,fp,#32
	ldmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,pc}
	
FactorialRecursive_0:
	@ Function FactorialRecursive_0
	@ Local variable offsets and line of death:
	@   this : 4   	l. 8
	@   a : 8   	l. 9
	@   _t16 : -28   	l. 2
	@   _t17 : -32   	l. 4
	@   _t18 : -36   	l. 8
	@   _t19 : -40   	l. 9
	@   _t20 : -44   	l. 10
	
	stmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,lr}
	add fp,sp,#32
	sub sp,fp,#52
	
	@ line 1: _t16 = a <= 1
	@ rallocs: a1=this a2=a
	cmp a2,#1
	movle v7,#1
	movgt v7,#0
	
	@ line 2: If (_t16 == false) goto 3
	@ rallocs: a1=this a2=a v7=_t16
	cmp v7,#0
	beq L3
	
	@ line 3: _t17 = 1
	@ rallocs: a1=this a2=a
	mov v7,#1
	
	@ line 4: Return _t17
	@ rallocs: a1=this a2=a v7=_t17
	mov a1,v7
	b .L2
	
	@ line 5: L 3:
	@ rallocs: a1=this a2=a
	
L3:
	
	@ line 6: _t18 = a - 1
	@ rallocs: a1=this a2=a
	sub v7,a2,#1
	
	@ line 7: _t19 = FactorialRecursive_0 (this, _t18)
	@ rallocs: a1=this a2=a v7=_t18
	str a1,[fp,#4]
	str a2,[fp,#8]
	ldr a1,[fp,#4]
	mov a2,v7
	sub sp,sp,#8
	bl FactorialRecursive_0
	add sp,sp,#8
	mov v6,a1
	
	@ line 8: _t20 = a * _t19
	@ rallocs: v6=_t19 v7=_t18
	ldr v5,[fp,#8]
	mul v4,v5,v6
	
	@ line 9: Return _t20
	@ rallocs: v4=_t20 v5=a v6=_t19
	mov a1,v4
	b .L2
	
	@ line 10: L 4:
	@ rallocs: v4=_t20
	
L4:
	
.L2:
	sub sp,fp,#32
	ldmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,pc}
	
FactorialIterative_0:
	@ Function FactorialIterative_0
	@ Local variable offsets and line of death:
	@   this : 4   	l. 0
	@   a : 8   	l. 8
	@   result : -28   	l. 10
	@   _t21 : -32   	l. 4
	
	stmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,lr}
	add fp,sp,#32
	sub sp,fp,#40
	
	@ line 1: result = 1
	@ rallocs: a2=a
	mov v7,#1
	
	@ line 2: L 5:
	@ rallocs: a2=a v7=result
	
L5:
	
	@ line 3: _t21 = a >= 1
	@ rallocs: a2=a v7=result
	cmp a2,#1
	movge v6,#1
	movlt v6,#0
	
	@ line 4: If (_t21 == false) goto 6
	@ rallocs: a2=a v6=_t21 v7=result
	cmp v6,#0
	beq L6
	
	@ line 5: result = result * a
	@ rallocs: a2=a v7=result
	mul v7,a2,v7
	
	@ line 6: a = a - 1
	@ rallocs: a2=a v7=result
	sub a2,a2,#1
	
	@ line 7: goto 5
	@ rallocs: a2=a v7=result
	b L5
	
	@ line 8: L 6:
	@ rallocs: a2=a v7=result
	
L6:
	
	@ line 9: Return result
	@ rallocs: v7=result
	mov a1,v7
	b .L3
	
.L3:
	sub sp,fp,#32
	ldmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,pc}
