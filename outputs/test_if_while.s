.data
	
str36:
	.asciz "Loop with comparison with constant\n"
	
str34:
	.asciz "method call is working fine in if expression\n"
	
str18:
	.asciz "not not b is true\n"
	
str28:
	.asciz "true or false is true\n"
	
str22:
	.asciz "b and c is true\n"
	
str30:
	.asciz "true and false is true\n"
	
str6:
	.asciz "dummy1 != dummy2\n"
	
str32:
	.asciz "field is working fine in if expression\n"
	
str33:
	.asciz "field is not working in if expression\n"
	
str23:
	.asciz "b and c is false\n"
	
str7:
	.asciz "true is true\n"
	
str40:
	.asciz "this shouldn't be printed\n"
	
str16:
	.asciz "not b is true\n"
	
str39:
	.asciz "Testing nested loop\n"
	
str35:
	.asciz "method call is not working in if expression\n"
	
str19:
	.asciz "not not b is false\n"
	
str1:
	.asciz "dummy1 < dummy2\n"
	
str12:
	.asciz "true is not always true\n"
	
str13:
	.asciz "true is false\n"
	
str14:
	.asciz "b is true\n"
	
str41:
	.asciz "this should print forever\n"
	
str9:
	.asciz "false is true\n"
	
str17:
	.asciz "not b is false\n"
	
str31:
	.asciz "true and false is false\n"
	
str11:
	.asciz "true is true twice\n"
	
str10:
	.asciz "false is not true\n"
	
str38:
	.asciz "Loop with comparison between variables\n"
	
str24:
	.asciz "b or false is true\n"
	
str21:
	.asciz "b or c is false\n"
	
str5:
	.asciz "dummy1 == dummy2\n"
	
str27:
	.asciz "b and false is false\n"
	
str20:
	.asciz "b or c is true\n"
	
str15:
	.asciz "b is false\n"
	
str3:
	.asciz "dummy1 > dummy2\n"
	
str2:
	.asciz "dummy1 >= dummy2\n"
	
str4:
	.asciz "dummy1 <= dummy2\n"
	
str26:
	.asciz "b and false is true\n"
	
str29:
	.asciz "true or false is false\n"
	
str25:
	.asciz "b or false is false\n"
	
str8:
	.asciz "true is not true\n"
	
str37:
	.asciz "%i\n"
	
	.text
	.global main
	
main:
	@ Function main
	@ Local variable offsets and line of death:
	@   this : 4   	l. 0
	@   a : 8   	l. 0
	@   ti : -28   	l. 3
	@   tw : -32   	l. 5
	
	stmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,lr}
	add fp,sp,#32
	sub sp,fp,#40
	
	@ line 1: ti = new TestIf()
	@ rallocs: 
	mov a1,#0
	bl _Znwj(PLT)
	mov v7,a1
	
	@ line 2: tw = new TestWhile()
	@ rallocs: v7=ti
	mov a1,#0
	bl _Znwj(PLT)
	mov v6,a1
	
	@ line 3: TestIf_0 (ti)
	@ rallocs: v6=tw v7=ti
	mov a1,v7
	sub sp,sp,#4
	bl TestIf_0
	add sp,sp,#4
	
	@ line 4: TestWhile_0 (tw)
	@ rallocs: v6=tw
	mov a1,v6
	sub sp,sp,#4
	bl TestWhile_0
	add sp,sp,#4
	
.L1:
	sub sp,fp,#32
	ldmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,pc}
	
TestIf_0:
	@ Function TestIf_0
	@ Local variable offsets and line of death:
	@   this : 4   	l. 0
	@   b : -28   	l. 121
	@   c : -32   	l. 106
	@   sc : -36   	l. 151
	@   dummy1 : -40   	l. 41
	@   dummy2 : -44   	l. 41
	@   _t1 : -48   	l. 5
	@   _t2 : -52   	l. 7
	@   _t3 : -56   	l. 14
	@   _t4 : -60   	l. 21
	@   _t5 : -64   	l. 28
	@   _t6 : -68   	l. 35
	@   _t7 : -72   	l. 42
	@   _t8 : -76   	l. 49
	@   _t9 : -80   	l. 56
	@   _t10 : -84   	l. 63
	@   _t11 : -88   	l. 65
	@   _t12 : -92   	l. 83
	@   _t13 : -96   	l. 90
	@   _t14 : -100   	l. 91
	@   _t15 : -104   	l. 100
	@   _t16 : -108   	l. 107
	@   _t17 : -112   	l. 115
	@   _t18 : -116   	l. 122
	@   _t19 : -120   	l. 129
	@   _t20 : -124   	l. 130
	@   _t21 : -128   	l. 137
	@   _t22 : -132   	l. 138
	@   _t23 : -136   	l. 145
	@   _t24 : -140   	l. 152
	
	stmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,lr}
	add fp,sp,#32
	sub sp,fp,#148
	
	@ line 1: dummy1 = 1
	@ rallocs: 
	mov v7,#1
	
	@ line 2: dummy2 = 2
	@ rallocs: v7=dummy1
	mov v6,#2
	
	@ line 3: sc = new SomeClass()
	@ rallocs: v6=dummy2 v7=dummy1
	mov a1,#4
	bl _Znwj(PLT)
	mov v5,a1
	
	@ line 4: _t1 = true
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	mov v4,#1
	
	@ line 5: sc.b = _t1
	@ rallocs: v4=_t1 v5=sc v6=dummy2 v7=dummy1
	str v4,[v5,#0]
	
	@ line 6: _t2 = dummy1 < dummy2
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	cmp v7,v6
	movlt v4,#1
	movge v4,#0
	
	@ line 7: If (_t2 == false) goto 1
	@ rallocs: v4=_t2 v5=sc v6=dummy2 v7=dummy1
	cmp v4,#0
	beq L1
	
	@ line 8: println("dummy1 < dummy2")
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	ldr a1,=str1
	bl printf(PLT)
	
	@ line 9: goto 2
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	b L2
	
	@ line 10: L 1:
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	
L1:
	
	@ line 11: println("dummy1 >= dummy2")
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	ldr a1,=str2
	bl printf(PLT)
	
	@ line 12: L 2:
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	
L2:
	
	@ line 13: _t3 = dummy1 > dummy2
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	cmp v7,v6
	movgt v4,#1
	movle v4,#0
	
	@ line 14: If (_t3 == false) goto 3
	@ rallocs: v4=_t3 v5=sc v6=dummy2 v7=dummy1
	cmp v4,#0
	beq L3
	
	@ line 15: println("dummy1 > dummy2")
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	ldr a1,=str3
	bl printf(PLT)
	
	@ line 16: goto 4
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	b L4
	
	@ line 17: L 3:
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	
L3:
	
	@ line 18: println("dummy1 <= dummy2")
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	ldr a1,=str4
	bl printf(PLT)
	
	@ line 19: L 4:
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	
L4:
	
	@ line 20: _t4 = dummy1 <= dummy2
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	cmp v7,v6
	movle v4,#1
	movgt v4,#0
	
	@ line 21: If (_t4 == false) goto 5
	@ rallocs: v4=_t4 v5=sc v6=dummy2 v7=dummy1
	cmp v4,#0
	beq L5
	
	@ line 22: println("dummy1 <= dummy2")
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	ldr a1,=str4
	bl printf(PLT)
	
	@ line 23: goto 6
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	b L6
	
	@ line 24: L 5:
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	
L5:
	
	@ line 25: println("dummy1 > dummy2")
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	ldr a1,=str3
	bl printf(PLT)
	
	@ line 26: L 6:
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	
L6:
	
	@ line 27: _t5 = dummy1 >= dummy2
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	cmp v7,v6
	movge v4,#1
	movlt v4,#0
	
	@ line 28: If (_t5 == false) goto 7
	@ rallocs: v4=_t5 v5=sc v6=dummy2 v7=dummy1
	cmp v4,#0
	beq L7
	
	@ line 29: println("dummy1 >= dummy2")
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	ldr a1,=str2
	bl printf(PLT)
	
	@ line 30: goto 8
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	b L8
	
	@ line 31: L 7:
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	
L7:
	
	@ line 32: println("dummy1 < dummy2")
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	ldr a1,=str1
	bl printf(PLT)
	
	@ line 33: L 8:
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	
L8:
	
	@ line 34: _t6 = dummy1 == dummy2
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	cmp v7,v6
	moveq v4,#1
	movne v4,#0
	
	@ line 35: If (_t6 == false) goto 9
	@ rallocs: v4=_t6 v5=sc v6=dummy2 v7=dummy1
	cmp v4,#0
	beq L9
	
	@ line 36: println("dummy1 == dummy2")
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	ldr a1,=str5
	bl printf(PLT)
	
	@ line 37: goto 10
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	b L10
	
	@ line 38: L 9:
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	
L9:
	
	@ line 39: println("dummy1 != dummy2")
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	ldr a1,=str6
	bl printf(PLT)
	
	@ line 40: L 10:
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	
L10:
	
	@ line 41: _t7 = dummy1 != dummy2
	@ rallocs: v5=sc v6=dummy2 v7=dummy1
	cmp v7,v6
	movne v4,#1
	moveq v4,#0
	
	@ line 42: If (_t7 == false) goto 11
	@ rallocs: v4=_t7 v5=sc
	cmp v4,#0
	beq L11
	
	@ line 43: println("dummy1 != dummy2")
	@ rallocs: v5=sc
	ldr a1,=str6
	bl printf(PLT)
	
	@ line 44: goto 12
	@ rallocs: v5=sc
	b L12
	
	@ line 45: L 11:
	@ rallocs: v5=sc
	
L11:
	
	@ line 46: println("dummy1 == dummy2")
	@ rallocs: v5=sc
	ldr a1,=str5
	bl printf(PLT)
	
	@ line 47: L 12:
	@ rallocs: v5=sc
	
L12:
	
	@ line 48: _t8 = true
	@ rallocs: v5=sc
	mov v7,#1
	
	@ line 49: If (_t8 == false) goto 13
	@ rallocs: v5=sc v7=_t8
	cmp v7,#0
	beq L13
	
	@ line 50: println("true is true")
	@ rallocs: v5=sc
	ldr a1,=str7
	bl printf(PLT)
	
	@ line 51: goto 14
	@ rallocs: v5=sc
	b L14
	
	@ line 52: L 13:
	@ rallocs: v5=sc
	
L13:
	
	@ line 53: println("true is not true")
	@ rallocs: v5=sc
	ldr a1,=str8
	bl printf(PLT)
	
	@ line 54: L 14:
	@ rallocs: v5=sc
	
L14:
	
	@ line 55: _t9 = false
	@ rallocs: v5=sc
	mov v7,#0
	
	@ line 56: If (_t9 == false) goto 15
	@ rallocs: v5=sc v7=_t9
	cmp v7,#0
	beq L15
	
	@ line 57: println("false is true")
	@ rallocs: v5=sc
	ldr a1,=str9
	bl printf(PLT)
	
	@ line 58: goto 16
	@ rallocs: v5=sc
	b L16
	
	@ line 59: L 15:
	@ rallocs: v5=sc
	
L15:
	
	@ line 60: println("false is not true")
	@ rallocs: v5=sc
	ldr a1,=str10
	bl printf(PLT)
	
	@ line 61: L 16:
	@ rallocs: v5=sc
	
L16:
	
	@ line 62: _t10 = true
	@ rallocs: v5=sc
	mov v7,#1
	
	@ line 63: If (_t10 == false) goto 19
	@ rallocs: v5=sc v7=_t10
	cmp v7,#0
	beq L19
	
	@ line 64: _t11 = true
	@ rallocs: v5=sc
	mov v7,#1
	
	@ line 65: If (_t11 == false) goto 17
	@ rallocs: v5=sc v7=_t11
	cmp v7,#0
	beq L17
	
	@ line 66: println("true is true twice")
	@ rallocs: v5=sc
	ldr a1,=str11
	bl printf(PLT)
	
	@ line 67: goto 18
	@ rallocs: v5=sc
	b L18
	
	@ line 68: L 17:
	@ rallocs: v5=sc
	
L17:
	
	@ line 69: println("true is not always true")
	@ rallocs: v5=sc
	ldr a1,=str12
	bl printf(PLT)
	
	@ line 70: L 18:
	@ rallocs: v5=sc
	
L18:
	
	@ line 71: goto 20
	@ rallocs: v5=sc
	b L20
	
	@ line 72: L 19:
	@ rallocs: v5=sc
	
L19:
	
	@ line 73: println("true is false")
	@ rallocs: v5=sc
	ldr a1,=str13
	bl printf(PLT)
	
	@ line 74: L 20:
	@ rallocs: v5=sc
	
L20:
	
	@ line 75: b = true
	@ rallocs: v5=sc
	mov v7,#1
	
	@ line 76: If (b == false) goto 21
	@ rallocs: v5=sc v7=b
	cmp v7,#0
	beq L21
	
	@ line 77: println("b is true")
	@ rallocs: v5=sc v7=b
	ldr a1,=str14
	bl printf(PLT)
	
	@ line 78: goto 22
	@ rallocs: v5=sc v7=b
	b L22
	
	@ line 79: L 21:
	@ rallocs: v5=sc v7=b
	
L21:
	
	@ line 80: println("b is false")
	@ rallocs: v5=sc v7=b
	ldr a1,=str15
	bl printf(PLT)
	
	@ line 81: L 22:
	@ rallocs: v5=sc v7=b
	
L22:
	
	@ line 82: _t12 = !b
	@ rallocs: v5=sc v7=b
	cmp v7,#0
	moveq v6,#1
	movne v6,#0
	
	@ line 83: If (_t12 == false) goto 23
	@ rallocs: v5=sc v6=_t12 v7=b
	cmp v6,#0
	beq L23
	
	@ line 84: println("not b is true")
	@ rallocs: v5=sc v7=b
	ldr a1,=str16
	bl printf(PLT)
	
	@ line 85: goto 24
	@ rallocs: v5=sc v7=b
	b L24
	
	@ line 86: L 23:
	@ rallocs: v5=sc v7=b
	
L23:
	
	@ line 87: println("not b is false")
	@ rallocs: v5=sc v7=b
	ldr a1,=str17
	bl printf(PLT)
	
	@ line 88: L 24:
	@ rallocs: v5=sc v7=b
	
L24:
	
	@ line 89: _t13 = !b
	@ rallocs: v5=sc v7=b
	cmp v7,#0
	moveq v6,#1
	movne v6,#0
	
	@ line 90: _t14 = !_t13
	@ rallocs: v5=sc v6=_t13 v7=b
	cmp v6,#0
	moveq v4,#1
	movne v4,#0
	
	@ line 91: If (_t14 == false) goto 25
	@ rallocs: v4=_t14 v5=sc v7=b
	cmp v4,#0
	beq L25
	
	@ line 92: println("not not b is true")
	@ rallocs: v5=sc v7=b
	ldr a1,=str18
	bl printf(PLT)
	
	@ line 93: goto 26
	@ rallocs: v5=sc v7=b
	b L26
	
	@ line 94: L 25:
	@ rallocs: v5=sc v7=b
	
L25:
	
	@ line 95: println("not not b is false")
	@ rallocs: v5=sc v7=b
	ldr a1,=str19
	bl printf(PLT)
	
	@ line 96: L 26:
	@ rallocs: v5=sc v7=b
	
L26:
	
	@ line 97: b = true
	@ rallocs: v5=sc v7=b
	mov v7,#1
	
	@ line 98: c = false
	@ rallocs: v5=sc v7=b
	mov v6,#0
	
	@ line 99: _t15 = b || c
	@ rallocs: v5=sc v6=c v7=b
	orr v4,v7,v6
	
	@ line 100: If (_t15 == false) goto 27
	@ rallocs: v4=_t15 v5=sc v6=c v7=b
	cmp v4,#0
	beq L27
	
	@ line 101: println("b or c is true")
	@ rallocs: v5=sc v6=c v7=b
	ldr a1,=str20
	bl printf(PLT)
	
	@ line 102: goto 28
	@ rallocs: v5=sc v6=c v7=b
	b L28
	
	@ line 103: L 27:
	@ rallocs: v5=sc v6=c v7=b
	
L27:
	
	@ line 104: println("b or c is false")
	@ rallocs: v5=sc v6=c v7=b
	ldr a1,=str21
	bl printf(PLT)
	
	@ line 105: L 28:
	@ rallocs: v5=sc v6=c v7=b
	
L28:
	
	@ line 106: _t16 = b && c
	@ rallocs: v5=sc v6=c v7=b
	and v4,v7,v6
	
	@ line 107: If (_t16 == false) goto 29
	@ rallocs: v4=_t16 v5=sc v7=b
	cmp v4,#0
	beq L29
	
	@ line 108: println("b and c is true")
	@ rallocs: v5=sc v7=b
	ldr a1,=str22
	bl printf(PLT)
	
	@ line 109: goto 30
	@ rallocs: v5=sc v7=b
	b L30
	
	@ line 110: L 29:
	@ rallocs: v5=sc v7=b
	
L29:
	
	@ line 111: println("b and c is false")
	@ rallocs: v5=sc v7=b
	ldr a1,=str23
	bl printf(PLT)
	
	@ line 112: L 30:
	@ rallocs: v5=sc v7=b
	
L30:
	
	@ line 113: b = true
	@ rallocs: v5=sc v7=b
	mov v7,#1
	
	@ line 114: _t17 = b || false
	@ rallocs: v5=sc v7=b
	orr v6,v7,#0
	
	@ line 115: If (_t17 == false) goto 31
	@ rallocs: v5=sc v6=_t17 v7=b
	cmp v6,#0
	beq L31
	
	@ line 116: println("b or false is true")
	@ rallocs: v5=sc v7=b
	ldr a1,=str24
	bl printf(PLT)
	
	@ line 117: goto 32
	@ rallocs: v5=sc v7=b
	b L32
	
	@ line 118: L 31:
	@ rallocs: v5=sc v7=b
	
L31:
	
	@ line 119: println("b or false is false")
	@ rallocs: v5=sc v7=b
	ldr a1,=str25
	bl printf(PLT)
	
	@ line 120: L 32:
	@ rallocs: v5=sc v7=b
	
L32:
	
	@ line 121: _t18 = b && false
	@ rallocs: v5=sc v7=b
	and v6,v7,#0
	
	@ line 122: If (_t18 == false) goto 33
	@ rallocs: v5=sc v6=_t18
	cmp v6,#0
	beq L33
	
	@ line 123: println("b and false is true")
	@ rallocs: v5=sc
	ldr a1,=str26
	bl printf(PLT)
	
	@ line 124: goto 34
	@ rallocs: v5=sc
	b L34
	
	@ line 125: L 33:
	@ rallocs: v5=sc
	
L33:
	
	@ line 126: println("b and false is false")
	@ rallocs: v5=sc
	ldr a1,=str27
	bl printf(PLT)
	
	@ line 127: L 34:
	@ rallocs: v5=sc
	
L34:
	
	@ line 128: _t19 = true
	@ rallocs: v5=sc
	mov v7,#1
	
	@ line 129: _t20 = _t19 || false
	@ rallocs: v5=sc v7=_t19
	orr v6,v7,#0
	
	@ line 130: If (_t20 == false) goto 35
	@ rallocs: v5=sc v6=_t20
	cmp v6,#0
	beq L35
	
	@ line 131: println("true or false is true")
	@ rallocs: v5=sc
	ldr a1,=str28
	bl printf(PLT)
	
	@ line 132: goto 36
	@ rallocs: v5=sc
	b L36
	
	@ line 133: L 35:
	@ rallocs: v5=sc
	
L35:
	
	@ line 134: println("true or false is false")
	@ rallocs: v5=sc
	ldr a1,=str29
	bl printf(PLT)
	
	@ line 135: L 36:
	@ rallocs: v5=sc
	
L36:
	
	@ line 136: _t21 = true
	@ rallocs: v5=sc
	mov v7,#1
	
	@ line 137: _t22 = _t21 && false
	@ rallocs: v5=sc v7=_t21
	and v6,v7,#0
	
	@ line 138: If (_t22 == false) goto 37
	@ rallocs: v5=sc v6=_t22
	cmp v6,#0
	beq L37
	
	@ line 139: println("true and false is true")
	@ rallocs: v5=sc
	ldr a1,=str30
	bl printf(PLT)
	
	@ line 140: goto 38
	@ rallocs: v5=sc
	b L38
	
	@ line 141: L 37:
	@ rallocs: v5=sc
	
L37:
	
	@ line 142: println("true and false is false")
	@ rallocs: v5=sc
	ldr a1,=str31
	bl printf(PLT)
	
	@ line 143: L 38:
	@ rallocs: v5=sc
	
L38:
	
	@ line 144: _t23 = sc.b
	@ rallocs: v5=sc
	ldr v7,[v5,#0]
	
	@ line 145: If (_t23 == false) goto 39
	@ rallocs: v5=sc v7=_t23
	cmp v7,#0
	beq L39
	
	@ line 146: println("field is working fine in if expression")
	@ rallocs: v5=sc
	ldr a1,=str32
	bl printf(PLT)
	
	@ line 147: goto 40
	@ rallocs: v5=sc
	b L40
	
	@ line 148: L 39:
	@ rallocs: v5=sc
	
L39:
	
	@ line 149: println("field is not working in if expression")
	@ rallocs: v5=sc
	ldr a1,=str33
	bl printf(PLT)
	
	@ line 150: L 40:
	@ rallocs: v5=sc
	
L40:
	
	@ line 151: _t24 = SomeClass_0 (sc)
	@ rallocs: v5=sc
	mov a1,v5
	sub sp,sp,#4
	bl SomeClass_0
	add sp,sp,#4
	mov v7,a1
	
	@ line 152: If (_t24 == false) goto 41
	@ rallocs: v7=_t24
	cmp v7,#0
	beq L41
	
	@ line 153: println("method call is working fine in if expression")
	@ rallocs: 
	ldr a1,=str34
	bl printf(PLT)
	
	@ line 154: goto 42
	@ rallocs: 
	b L42
	
	@ line 155: L 41:
	@ rallocs: 
	
L41:
	
	@ line 156: println("method call is not working in if expression")
	@ rallocs: 
	ldr a1,=str35
	bl printf(PLT)
	
	@ line 157: L 42:
	@ rallocs: 
	
L42:
	
.L2:
	sub sp,fp,#32
	ldmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,pc}
	
TestWhile_0:
	@ Function TestWhile_0
	@ Local variable offsets and line of death:
	@   this : 4   	l. 0
	@   a : -28   	l. 36
	@   b : -32   	l. 33
	@   sc : -36   	l. 51
	@   _t25 : -40   	l. 5
	@   _t26 : -44   	l. 15
	@   _t27 : -48   	l. 24
	@   _t28 : -52   	l. 28
	@   _t29 : -56   	l. 30
	@   _t30 : -60   	l. 39
	@   _t31 : -64   	l. 42
	@   _t32 : -68   	l. 48
	@   _t33 : -72   	l. 54
	@   _t34 : -76   	l. 60
	
	stmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,lr}
	add fp,sp,#32
	sub sp,fp,#84
	
	@ line 1: println("Loop with comparison with constant")
	@ rallocs: 
	ldr a1,=str36
	bl printf(PLT)
	
	@ line 2: a = 10
	@ rallocs: 
	mov v7,#10
	
	@ line 3: L 43:
	@ rallocs: v7=a
	
L43:
	
	@ line 4: _t25 = a > 0
	@ rallocs: v7=a
	cmp v7,#0
	movgt v6,#1
	movle v6,#0
	
	@ line 5: If (_t25 == false) goto 44
	@ rallocs: v6=_t25 v7=a
	cmp v6,#0
	beq L44
	
	@ line 6: println(a)
	@ rallocs: v7=a
	ldr a1,=str37
	mov a2,v7
	bl printf(PLT)
	
	@ line 7: a = a - 1
	@ rallocs: v7=a
	sub v7,v7,#1
	
	@ line 8: goto 43
	@ rallocs: v7=a
	b L43
	
	@ line 9: L 44:
	@ rallocs: v7=a
	
L44:
	
	@ line 10: println("Loop with comparison between variables")
	@ rallocs: v7=a
	ldr a1,=str38
	bl printf(PLT)
	
	@ line 11: a = 10
	@ rallocs: v7=a
	mov v7,#10
	
	@ line 12: b = 0
	@ rallocs: v7=a
	mov v6,#0
	
	@ line 13: L 45:
	@ rallocs: v6=b v7=a
	
L45:
	
	@ line 14: _t26 = a > b
	@ rallocs: v6=b v7=a
	cmp v7,v6
	movgt v5,#1
	movle v5,#0
	
	@ line 15: If (_t26 == false) goto 46
	@ rallocs: v5=_t26 v6=b v7=a
	cmp v5,#0
	beq L46
	
	@ line 16: println(b)
	@ rallocs: v6=b v7=a
	ldr a1,=str37
	mov a2,v6
	bl printf(PLT)
	
	@ line 17: b = b + 1
	@ rallocs: v6=b v7=a
	add v6,v6,#1
	
	@ line 18: goto 45
	@ rallocs: v6=b v7=a
	b L45
	
	@ line 19: L 46:
	@ rallocs: v6=b v7=a
	
L46:
	
	@ line 20: println("Testing nested loop")
	@ rallocs: v6=b v7=a
	ldr a1,=str39
	bl printf(PLT)
	
	@ line 21: a = 0
	@ rallocs: v6=b v7=a
	mov v7,#0
	
	@ line 22: L 49:
	@ rallocs: v6=b v7=a
	
L49:
	
	@ line 23: _t27 = a < 10
	@ rallocs: v6=b v7=a
	cmp v7,#10
	movlt v5,#1
	movge v5,#0
	
	@ line 24: If (_t27 == false) goto 50
	@ rallocs: v5=_t27 v6=b v7=a
	cmp v5,#0
	beq L50
	
	@ line 25: b = 0
	@ rallocs: v6=b v7=a
	mov v6,#0
	
	@ line 26: L 47:
	@ rallocs: v6=b v7=a
	
L47:
	
	@ line 27: _t28 = b < 10
	@ rallocs: v6=b v7=a
	cmp v6,#10
	movlt v5,#1
	movge v5,#0
	
	@ line 28: If (_t28 == false) goto 48
	@ rallocs: v5=_t28 v6=b v7=a
	cmp v5,#0
	beq L48
	
	@ line 29: _t29 = a + b
	@ rallocs: v6=b v7=a
	add v5,v7,v6
	
	@ line 30: println(_t29)
	@ rallocs: v5=_t29 v6=b v7=a
	ldr a1,=str37
	mov a2,v5
	bl printf(PLT)
	
	@ line 31: b = b + 1
	@ rallocs: v6=b v7=a
	add v6,v6,#1
	
	@ line 32: goto 47
	@ rallocs: v6=b v7=a
	b L47
	
	@ line 33: L 48:
	@ rallocs: v6=b v7=a
	
L48:
	
	@ line 34: a = a + 1
	@ rallocs: v7=a
	add v7,v7,#1
	
	@ line 35: goto 49
	@ rallocs: v7=a
	b L49
	
	@ line 36: L 50:
	@ rallocs: v7=a
	
L50:
	
	@ line 37: sc = new SomeClass()
	@ rallocs: 
	mov a1,#4
	bl _Znwj(PLT)
	mov v7,a1
	
	@ line 38: _t30 = false
	@ rallocs: v7=sc
	mov v6,#0
	
	@ line 39: sc.b = _t30
	@ rallocs: v6=_t30 v7=sc
	str v6,[v7,#0]
	
	@ line 40: L 51:
	@ rallocs: v7=sc
	
L51:
	
	@ line 41: _t31 = sc.b
	@ rallocs: v7=sc
	ldr v6,[v7,#0]
	
	@ line 42: If (_t31 == false) goto 52
	@ rallocs: v6=_t31 v7=sc
	cmp v6,#0
	beq L52
	
	@ line 43: println("this shouldn't be printed")
	@ rallocs: v7=sc
	ldr a1,=str40
	bl printf(PLT)
	
	@ line 44: goto 51
	@ rallocs: v7=sc
	b L51
	
	@ line 45: L 52:
	@ rallocs: v7=sc
	
L52:
	
	@ line 46: L 53:
	@ rallocs: v7=sc
	
L53:
	
	@ line 47: _t32 = SomeClass_1 (sc)
	@ rallocs: v7=sc
	mov a1,v7
	sub sp,sp,#4
	bl SomeClass_1
	add sp,sp,#4
	mov v6,a1
	
	@ line 48: If (_t32 == false) goto 54
	@ rallocs: v6=_t32 v7=sc
	cmp v6,#0
	beq L54
	
	@ line 49: println("this shouldn't be printed")
	@ rallocs: v7=sc
	ldr a1,=str40
	bl printf(PLT)
	
	@ line 50: goto 53
	@ rallocs: v7=sc
	b L53
	
	@ line 51: L 54:
	@ rallocs: v7=sc
	
L54:
	
	@ line 52: L 55:
	@ rallocs: 
	
L55:
	
	@ line 53: _t33 = false
	@ rallocs: 
	mov v7,#0
	
	@ line 54: If (_t33 == false) goto 56
	@ rallocs: v7=_t33
	cmp v7,#0
	beq L56
	
	@ line 55: println("this shouldn't be printed")
	@ rallocs: 
	ldr a1,=str40
	bl printf(PLT)
	
	@ line 56: goto 55
	@ rallocs: 
	b L55
	
	@ line 57: L 56:
	@ rallocs: 
	
L56:
	
	@ line 58: L 57:
	@ rallocs: 
	
L57:
	
	@ line 59: _t34 = true
	@ rallocs: 
	mov v7,#1
	
	@ line 60: If (_t34 == false) goto 58
	@ rallocs: v7=_t34
	cmp v7,#0
	beq L58
	
	@ line 61: println("this should print forever")
	@ rallocs: 
	ldr a1,=str41
	bl printf(PLT)
	
	@ line 62: goto 57
	@ rallocs: 
	b L57
	
	@ line 63: L 58:
	@ rallocs: 
	
L58:
	
.L3:
	sub sp,fp,#32
	ldmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,pc}
	
SomeClass_0:
	@ Function SomeClass_0
	@ Local variable offsets and line of death:
	@   this : 4   	l. 0
	@   _t36 : -28   	l. 3
	
	stmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,lr}
	add fp,sp,#32
	sub sp,fp,#36
	
	@ line 1: _t36 = true
	@ rallocs: 
	mov v7,#1
	
	@ line 2: Return _t36
	@ rallocs: v7=_t36
	mov a1,v7
	b .L4
	
.L4:
	sub sp,fp,#32
	ldmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,pc}
	
SomeClass_1:
	@ Function SomeClass_1
	@ Local variable offsets and line of death:
	@   this : 4   	l. 0
	@   _t35 : -28   	l. 3
	
	stmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,lr}
	add fp,sp,#32
	sub sp,fp,#36
	
	@ line 1: _t35 = false
	@ rallocs: 
	mov v7,#0
	
	@ line 2: Return _t35
	@ rallocs: v7=_t35
	mov a1,v7
	b .L5
	
.L5:
	sub sp,fp,#32
	ldmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,pc}
