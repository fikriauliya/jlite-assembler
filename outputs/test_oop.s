.data
	
str24:
	.asciz "Iron the laundry"
	
str16:
	.asciz "Nothing a specialist like me can't handle!"
	
str7:
	.asciz "I can fly! I can FLYYY!"
	
str14:
	.asciz "Ironman"
	
str23:
	.asciz "Take care of the bat population in the bat cave"
	
str15:
	.asciz "Leave the ironing to me, the IRONMAN!!!"
	
str20:
	.asciz "Here is the bill"
	
str4:
	.asciz "Right"
	
str19:
	.asciz "With great power comes great electricity bill"
	
str6:
	.asciz "Superman"
	
str28:
	.asciz "Case:\n"
	
str5:
	.asciz "Heryandi"
	
str12:
	.asciz "See? I'm not bad"
	
str30:
	.asciz "Solved by superhero:\n"
	
str32:
	.asciz "Solved?\n"
	
str2:
	.asciz "Down"
	
str25:
	.asciz "Take care of the daily power outage in some parts of Indonesia"
	
str26:
	.asciz "Case list\n"
	
str8:
	.asciz "It's cold up there"
	
str22:
	.asciz "Get a cat down from 3000km tree"
	
str9:
	.asciz "Jesper"
	
str29:
	.asciz "%s\n"
	
str11:
	.asciz "It's BATMAN ok? Not BADMAN!"
	
str18:
	.asciz "Thor"
	
str38:
	.asciz "Name: \n"
	
str27:
	.asciz "Superhero profile:\n"
	
str39:
	.asciz "Secret identity: \n"
	
str13:
	.asciz "Lionel"
	
str17:
	.asciz "Pahlevi"
	
str35:
	.asciz "Case: \n"
	
str40:
	.asciz "Toilet seat preference: \n"
	
str10:
	.asciz "Batman"
	
str3:
	.asciz "Left"
	
str31:
	.asciz "Remark by superhero:\n"
	
str1:
	.asciz "Up"
	
str34:
	.asciz "Assigned to:\n"
	
str36:
	.asciz "Assigned to: \n"
	
str21:
	.asciz "\n"
	
str33:
	.asciz "%i\n"
	
str37:
	.asciz "Assurance from superhero:\n"
	
	.text
	.global main
	
main:
	@ Function main
	@ Local variable offsets and line of death:
	@   this : 4   	l. 0
	@   h : -28   	l. 91
	@   j : -32   	l. 93
	@   l : -36   	l. 95
	@   p : -40   	l. 97
	@   up : -44   	l. 22
	@   down : -48   	l. 32
	@   left : -52   	l. 42
	@   right : -56   	l. 52
	@   c1 : -60   	l. 85
	@   c2 : -64   	l. 86
	@   c3 : -68   	l. 87
	@   c4 : -72   	l. 88
	@   _t1 : -76   	l. 6
	@   _t2 : -80   	l. 8
	@   _t3 : -84   	l. 10
	@   _t4 : -88   	l. 12
	@   _t5 : -92   	l. 15
	@   _t6 : -96   	l. 17
	@   _t7 : -100   	l. 19
	@   _t8 : -104   	l. 21
	@   _t9 : -108   	l. 25
	@   _t10 : -112   	l. 27
	@   _t11 : -116   	l. 29
	@   _t12 : -120   	l. 31
	@   _t13 : -124   	l. 35
	@   _t14 : -128   	l. 37
	@   _t15 : -132   	l. 39
	@   _t16 : -136   	l. 41
	@   _t17 : -140   	l. 45
	@   _t18 : -144   	l. 47
	@   _t19 : -148   	l. 49
	@   _t20 : -152   	l. 51
	@   _t21 : -156   	l. 56
	@   _t22 : -160   	l. 58
	@   _t23 : -164   	l. 62
	@   _t24 : -168   	l. 64
	@   _t25 : -172   	l. 68
	@   _t26 : -176   	l. 70
	@   _t27 : -180   	l. 74
	@   _t28 : -184   	l. 76
	
	stmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,lr}
	add fp,sp,#32
	sub sp,fp,#192
	
	@ line 1: up = new ToiletSeatPreference()
	@ rallocs: 
	mov a1,#4
	bl _Znwj(PLT)
	mov v7,a1
	
	@ line 2: down = new ToiletSeatPreference()
	@ rallocs: v7=up
	mov a1,#4
	bl _Znwj(PLT)
	mov v6,a1
	
	@ line 3: left = new ToiletSeatPreference()
	@ rallocs: v6=down v7=up
	mov a1,#4
	bl _Znwj(PLT)
	mov v5,a1
	
	@ line 4: right = new ToiletSeatPreference()
	@ rallocs: v5=left v6=down v7=up
	mov a1,#4
	bl _Znwj(PLT)
	mov v4,a1
	
	@ line 5: _t1 = "Up"
	@ rallocs: v4=right v5=left v6=down v7=up
	ldr v3,=str1
	
	@ line 6: up.dir = _t1
	@ rallocs: v3=_t1 v4=right v5=left v6=down v7=up
	str v3,[v7,#0]
	
	@ line 7: _t2 = "Down"
	@ rallocs: v4=right v5=left v6=down v7=up
	ldr v3,=str2
	
	@ line 8: down.dir = _t2
	@ rallocs: v3=_t2 v4=right v5=left v6=down v7=up
	str v3,[v6,#0]
	
	@ line 9: _t3 = "Left"
	@ rallocs: v4=right v5=left v6=down v7=up
	ldr v3,=str3
	
	@ line 10: left.dir = _t3
	@ rallocs: v3=_t3 v4=right v5=left v6=down v7=up
	str v3,[v5,#0]
	
	@ line 11: _t4 = "Right"
	@ rallocs: v4=right v5=left v6=down v7=up
	ldr v3,=str4
	
	@ line 12: right.dir = _t4
	@ rallocs: v3=_t4 v4=right v5=left v6=down v7=up
	str v3,[v4,#0]
	
	@ line 13: h = new Person()
	@ rallocs: v4=right v5=left v6=down v7=up
	mov a1,#20
	bl _Znwj(PLT)
	mov v3,a1
	
	@ line 14: _t5 = "Heryandi"
	@ rallocs: v3=h v4=right v5=left v6=down v7=up
	ldr v2,=str5
	
	@ line 15: h.name = _t5
	@ rallocs: v2=_t5 v3=h v4=right v5=left v6=down v7=up
	str v2,[v3,#0]
	
	@ line 16: _t6 = "Superman"
	@ rallocs: v3=h v4=right v5=left v6=down v7=up
	ldr v2,=str6
	
	@ line 17: h.secretIdentity = _t6
	@ rallocs: v2=_t6 v3=h v4=right v5=left v6=down v7=up
	str v2,[v3,#4]
	
	@ line 18: _t7 = "I can fly! I can FLYYY!"
	@ rallocs: v3=h v4=right v5=left v6=down v7=up
	ldr v2,=str7
	
	@ line 19: h.assignedQuote = _t7
	@ rallocs: v2=_t7 v3=h v4=right v5=left v6=down v7=up
	str v2,[v3,#8]
	
	@ line 20: _t8 = "It's cold up there"
	@ rallocs: v3=h v4=right v5=left v6=down v7=up
	ldr v2,=str8
	
	@ line 21: h.successQuote = _t8
	@ rallocs: v2=_t8 v3=h v4=right v5=left v6=down v7=up
	str v2,[v3,#12]
	
	@ line 22: h.pref = up
	@ rallocs: v3=h v4=right v5=left v6=down v7=up
	str v7,[v3,#16]
	
	@ line 23: j = new Person()
	@ rallocs: v3=h v4=right v5=left v6=down
	mov a1,#20
	bl _Znwj(PLT)
	mov v7,a1
	
	@ line 24: _t9 = "Jesper"
	@ rallocs: v3=h v4=right v5=left v6=down v7=j
	ldr v2,=str9
	
	@ line 25: j.name = _t9
	@ rallocs: v2=_t9 v3=h v4=right v5=left v6=down v7=j
	str v2,[v7,#0]
	
	@ line 26: _t10 = "Batman"
	@ rallocs: v3=h v4=right v5=left v6=down v7=j
	ldr v2,=str10
	
	@ line 27: j.secretIdentity = _t10
	@ rallocs: v2=_t10 v3=h v4=right v5=left v6=down v7=j
	str v2,[v7,#4]
	
	@ line 28: _t11 = "It's BATMAN ok? Not BADMAN!"
	@ rallocs: v3=h v4=right v5=left v6=down v7=j
	ldr v2,=str11
	
	@ line 29: j.assignedQuote = _t11
	@ rallocs: v2=_t11 v3=h v4=right v5=left v6=down v7=j
	str v2,[v7,#8]
	
	@ line 30: _t12 = "See? I'm not bad"
	@ rallocs: v3=h v4=right v5=left v6=down v7=j
	ldr v2,=str12
	
	@ line 31: j.successQuote = _t12
	@ rallocs: v2=_t12 v3=h v4=right v5=left v6=down v7=j
	str v2,[v7,#12]
	
	@ line 32: j.pref = down
	@ rallocs: v3=h v4=right v5=left v6=down v7=j
	str v6,[v7,#16]
	
	@ line 33: l = new Person()
	@ rallocs: v3=h v4=right v5=left v7=j
	mov a1,#20
	bl _Znwj(PLT)
	mov v6,a1
	
	@ line 34: _t13 = "Lionel"
	@ rallocs: v3=h v4=right v5=left v6=l v7=j
	ldr v2,=str13
	
	@ line 35: l.name = _t13
	@ rallocs: v2=_t13 v3=h v4=right v5=left v6=l v7=j
	str v2,[v6,#0]
	
	@ line 36: _t14 = "Ironman"
	@ rallocs: v3=h v4=right v5=left v6=l v7=j
	ldr v2,=str14
	
	@ line 37: l.secretIdentity = _t14
	@ rallocs: v2=_t14 v3=h v4=right v5=left v6=l v7=j
	str v2,[v6,#4]
	
	@ line 38: _t15 = "Leave the ironing to me, the IRONMAN!!!"
	@ rallocs: v3=h v4=right v5=left v6=l v7=j
	ldr v2,=str15
	
	@ line 39: l.assignedQuote = _t15
	@ rallocs: v2=_t15 v3=h v4=right v5=left v6=l v7=j
	str v2,[v6,#8]
	
	@ line 40: _t16 = "Nothing a specialist like me can't handle!"
	@ rallocs: v3=h v4=right v5=left v6=l v7=j
	ldr v2,=str16
	
	@ line 41: l.successQuote = _t16
	@ rallocs: v2=_t16 v3=h v4=right v5=left v6=l v7=j
	str v2,[v6,#12]
	
	@ line 42: l.pref = left
	@ rallocs: v3=h v4=right v5=left v6=l v7=j
	str v5,[v6,#16]
	
	@ line 43: p = new Person()
	@ rallocs: v3=h v4=right v6=l v7=j
	mov a1,#20
	bl _Znwj(PLT)
	mov v5,a1
	
	@ line 44: _t17 = "Pahlevi"
	@ rallocs: v3=h v4=right v5=p v6=l v7=j
	ldr v2,=str17
	
	@ line 45: p.name = _t17
	@ rallocs: v2=_t17 v3=h v4=right v5=p v6=l v7=j
	str v2,[v5,#0]
	
	@ line 46: _t18 = "Thor"
	@ rallocs: v3=h v4=right v5=p v6=l v7=j
	ldr v2,=str18
	
	@ line 47: p.secretIdentity = _t18
	@ rallocs: v2=_t18 v3=h v4=right v5=p v6=l v7=j
	str v2,[v5,#4]
	
	@ line 48: _t19 = "With great power comes great electricity bill"
	@ rallocs: v3=h v4=right v5=p v6=l v7=j
	ldr v2,=str19
	
	@ line 49: p.assignedQuote = _t19
	@ rallocs: v2=_t19 v3=h v4=right v5=p v6=l v7=j
	str v2,[v5,#8]
	
	@ line 50: _t20 = "Here is the bill"
	@ rallocs: v3=h v4=right v5=p v6=l v7=j
	ldr v2,=str20
	
	@ line 51: p.successQuote = _t20
	@ rallocs: v2=_t20 v3=h v4=right v5=p v6=l v7=j
	str v2,[v5,#12]
	
	@ line 52: p.pref = right
	@ rallocs: v3=h v4=right v5=p v6=l v7=j
	str v4,[v5,#16]
	
	@ line 53: println("")
	@ rallocs: v3=h v5=p v6=l v7=j
	ldr a1,=str21
	bl printf(PLT)
	
	@ line 54: c1 = new Case()
	@ rallocs: v3=h v5=p v6=l v7=j
	mov a1,#12
	bl _Znwj(PLT)
	mov v4,a1
	
	@ line 55: _t21 = "Get a cat down from 3000km tree"
	@ rallocs: v3=h v4=c1 v5=p v6=l v7=j
	ldr v2,=str22
	
	@ line 56: c1.content = _t21
	@ rallocs: v2=_t21 v3=h v4=c1 v5=p v6=l v7=j
	str v2,[v4,#4]
	
	@ line 57: _t22 = false
	@ rallocs: v3=h v4=c1 v5=p v6=l v7=j
	mov v2,#0
	
	@ line 58: c1.solved = _t22
	@ rallocs: v2=_t22 v3=h v4=c1 v5=p v6=l v7=j
	str v2,[v4,#8]
	
	@ line 59: Person_0 (h, c1)
	@ rallocs: v3=h v4=c1 v5=p v6=l v7=j
	mov a1,v3
	mov a2,v4
	sub sp,sp,#8
	bl Person_0
	add sp,sp,#8
	
	@ line 60: c2 = new Case()
	@ rallocs: v3=h v4=c1 v5=p v6=l v7=j
	mov a1,#12
	bl _Znwj(PLT)
	mov v2,a1
	
	@ line 61: _t23 = "Take care of the bat population in the bat cave"
	@ rallocs: v2=c2 v3=h v4=c1 v5=p v6=l v7=j
	ldr v1,=str23
	
	@ line 62: c2.content = _t23
	@ rallocs: v1=_t23 v2=c2 v3=h v4=c1 v5=p v6=l v7=j
	str v1,[v2,#4]
	
	@ line 63: _t24 = false
	@ rallocs: v2=c2 v3=h v4=c1 v5=p v6=l v7=j
	mov v1,#0
	
	@ line 64: c2.solved = _t24
	@ rallocs: v1=_t24 v2=c2 v3=h v4=c1 v5=p v6=l v7=j
	str v1,[v2,#8]
	
	@ line 65: Person_0 (j, c2)
	@ rallocs: v2=c2 v3=h v4=c1 v5=p v6=l v7=j
	mov a1,v7
	mov a2,v2
	sub sp,sp,#8
	bl Person_0
	add sp,sp,#8
	
	@ line 66: c3 = new Case()
	@ rallocs: v2=c2 v3=h v4=c1 v5=p v6=l v7=j
	mov a1,#12
	bl _Znwj(PLT)
	mov v1,a1
	
	@ line 67: _t25 = "Iron the laundry"
	@ rallocs: v1=c3 v2=c2 v3=h v4=c1 v5=p v6=l v7=j
	ldr lr,=str24
	
	@ line 68: c3.content = _t25
	@ rallocs: lr=_t25 v1=c3 v2=c2 v3=h v4=c1 v5=p v6=l v7=j
	str lr,[v1,#4]
	
	@ line 69: _t26 = false
	@ rallocs: v1=c3 v2=c2 v3=h v4=c1 v5=p v6=l v7=j
	mov lr,#0
	
	@ line 70: c3.solved = _t26
	@ rallocs: lr=_t26 v1=c3 v2=c2 v3=h v4=c1 v5=p v6=l v7=j
	str lr,[v1,#8]
	
	@ line 71: Person_0 (l, c3)
	@ rallocs: v1=c3 v2=c2 v3=h v4=c1 v5=p v6=l v7=j
	mov a1,v6
	mov a2,v1
	sub sp,sp,#8
	bl Person_0
	add sp,sp,#8
	
	@ line 72: c4 = new Case()
	@ rallocs: v1=c3 v2=c2 v3=h v4=c1 v5=p v6=l v7=j
	mov a1,#12
	bl _Znwj(PLT)
	mov lr,a1
	
	@ line 73: _t27 = "Take care of the daily power outage in some parts of Indonesia"
	@ rallocs: lr=c4 v1=c3 v2=c2 v3=h v4=c1 v5=p v6=l v7=j
	ldr a4,=str25
	
	@ line 74: c4.content = _t27
	@ rallocs: a4=_t27 lr=c4 v1=c3 v2=c2 v3=h v4=c1 v5=p v6=l v7=j
	str a4,[lr,#4]
	
	@ line 75: _t28 = false
	@ rallocs: lr=c4 v1=c3 v2=c2 v3=h v4=c1 v5=p v6=l v7=j
	mov a4,#0
	
	@ line 76: c4.solved = _t28
	@ rallocs: a4=_t28 lr=c4 v1=c3 v2=c2 v3=h v4=c1 v5=p v6=l v7=j
	str a4,[lr,#8]
	
	@ line 77: Person_0 (p, c4)
	@ rallocs: lr=c4 v1=c3 v2=c2 v3=h v4=c1 v5=p v6=l v7=j
	str lr,[fp,#-72]
	mov a1,v5
	ldr a2,[fp,#-72]
	sub sp,sp,#8
	bl Person_0
	add sp,sp,#8
	
	@ line 78: println("")
	@ rallocs: v1=c3 v2=c2 v3=h v4=c1 v5=p v6=l v7=j
	ldr a1,=str21
	bl printf(PLT)
	
	@ line 79: Case_0 (c1)
	@ rallocs: v1=c3 v2=c2 v3=h v4=c1 v5=p v6=l v7=j
	mov a1,v4
	sub sp,sp,#4
	bl Case_0
	add sp,sp,#4
	
	@ line 80: Case_0 (c2)
	@ rallocs: v1=c3 v2=c2 v3=h v4=c1 v5=p v6=l v7=j
	mov a1,v2
	sub sp,sp,#4
	bl Case_0
	add sp,sp,#4
	
	@ line 81: Case_0 (c3)
	@ rallocs: v1=c3 v2=c2 v3=h v4=c1 v5=p v6=l v7=j
	mov a1,v1
	sub sp,sp,#4
	bl Case_0
	add sp,sp,#4
	
	@ line 82: Case_0 (c4)
	@ rallocs: v1=c3 v2=c2 v3=h v4=c1 v5=p v6=l v7=j
	ldr a1,[fp,#-72]
	sub sp,sp,#4
	bl Case_0
	add sp,sp,#4
	
	@ line 83: println("")
	@ rallocs: v1=c3 v2=c2 v3=h v4=c1 v5=p v6=l v7=j
	ldr a1,=str21
	bl printf(PLT)
	
	@ line 84: println("Case list")
	@ rallocs: v1=c3 v2=c2 v3=h v4=c1 v5=p v6=l v7=j
	ldr a1,=str26
	bl printf(PLT)
	
	@ line 85: Case_1 (c1)
	@ rallocs: v1=c3 v2=c2 v3=h v4=c1 v5=p v6=l v7=j
	mov a1,v4
	sub sp,sp,#4
	bl Case_1
	add sp,sp,#4
	
	@ line 86: Case_1 (c2)
	@ rallocs: v1=c3 v2=c2 v3=h v5=p v6=l v7=j
	mov a1,v2
	sub sp,sp,#4
	bl Case_1
	add sp,sp,#4
	
	@ line 87: Case_1 (c3)
	@ rallocs: v1=c3 v3=h v5=p v6=l v7=j
	mov a1,v1
	sub sp,sp,#4
	bl Case_1
	add sp,sp,#4
	
	@ line 88: Case_1 (c4)
	@ rallocs: v3=h v5=p v6=l v7=j
	ldr a1,[fp,#-72]
	sub sp,sp,#4
	bl Case_1
	add sp,sp,#4
	
	@ line 89: println("")
	@ rallocs: v3=h v5=p v6=l v7=j
	ldr a1,=str21
	bl printf(PLT)
	
	@ line 90: println("Superhero profile:")
	@ rallocs: v3=h v5=p v6=l v7=j
	ldr a1,=str27
	bl printf(PLT)
	
	@ line 91: Person_1 (h)
	@ rallocs: v3=h v5=p v6=l v7=j
	mov a1,v3
	sub sp,sp,#4
	bl Person_1
	add sp,sp,#4
	
	@ line 92: println("")
	@ rallocs: v5=p v6=l v7=j
	ldr a1,=str21
	bl printf(PLT)
	
	@ line 93: Person_1 (j)
	@ rallocs: v5=p v6=l v7=j
	mov a1,v7
	sub sp,sp,#4
	bl Person_1
	add sp,sp,#4
	
	@ line 94: println("")
	@ rallocs: v5=p v6=l
	ldr a1,=str21
	bl printf(PLT)
	
	@ line 95: Person_1 (l)
	@ rallocs: v5=p v6=l
	mov a1,v6
	sub sp,sp,#4
	bl Person_1
	add sp,sp,#4
	
	@ line 96: println("")
	@ rallocs: v5=p
	ldr a1,=str21
	bl printf(PLT)
	
	@ line 97: Person_1 (p)
	@ rallocs: v5=p
	mov a1,v5
	sub sp,sp,#4
	bl Person_1
	add sp,sp,#4
	
	@ line 98: println("")
	@ rallocs: 
	ldr a1,=str21
	bl printf(PLT)
	
.L1:
	sub sp,fp,#32
	ldmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,pc}
	
Case_0:
	@ Function Case_0
	@ Local variable offsets and line of death:
	@   this : 4   	l. 11
	@   _t33 : -28   	l. 2
	@   _t34 : -32   	l. 5
	@   _t35 : -36   	l. 8
	@   _t36 : -40   	l. 9
	@   _t37 : -44   	l. 12
	@   _t38 : -48   	l. 14
	
	stmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,lr}
	add fp,sp,#32
	sub sp,fp,#56
	
	@ line 1: _t33 = true
	@ rallocs: a1=this
	mov v7,#1
	
	@ line 2: this.solved = _t33
	@ rallocs: a1=this v7=_t33
	str v7,[a1,#8]
	
	@ line 3: println("Case:")
	@ rallocs: a1=this
	str a1,[fp,#4]
	ldr a1,=str28
	bl printf(PLT)
	
	@ line 4: _t34 = this.content
	@ rallocs: 
	ldr v7,[fp,#4]
	ldr v6,[v7,#4]
	
	@ line 5: println(_t34)
	@ rallocs: v6=_t34 v7=this
	ldr a1,=str29
	mov a2,v6
	bl printf(PLT)
	
	@ line 6: println("Solved by superhero:")
	@ rallocs: v7=this
	ldr a1,=str30
	bl printf(PLT)
	
	@ line 7: _t35 = this.assignee
	@ rallocs: v7=this
	ldr v6,[v7,#0]
	
	@ line 8: _t36 = _t35.secretIdentity
	@ rallocs: v6=_t35 v7=this
	ldr v5,[v6,#4]
	
	@ line 9: println(_t36)
	@ rallocs: v5=_t36 v7=this
	ldr a1,=str29
	mov a2,v5
	bl printf(PLT)
	
	@ line 10: println("Remark by superhero:")
	@ rallocs: v7=this
	ldr a1,=str31
	bl printf(PLT)
	
	@ line 11: _t37 = this.assignee
	@ rallocs: v7=this
	ldr v6,[v7,#0]
	
	@ line 12: _t38 = _t37.successQuote
	@ rallocs: v6=_t37
	ldr v7,[v6,#12]
	
	@ line 13: println(_t38)
	@ rallocs: v7=_t38
	ldr a1,=str29
	mov a2,v7
	bl printf(PLT)
	
.L2:
	sub sp,fp,#32
	ldmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,pc}
	
Case_1:
	@ Function Case_1
	@ Local variable offsets and line of death:
	@   this : 4   	l. 8
	@   _t29 : -28   	l. 3
	@   _t30 : -32   	l. 6
	@   _t31 : -36   	l. 9
	@   _t32 : -40   	l. 11
	
	stmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,lr}
	add fp,sp,#32
	sub sp,fp,#48
	
	@ line 1: println("Case:")
	@ rallocs: a1=this
	str a1,[fp,#4]
	ldr a1,=str28
	bl printf(PLT)
	
	@ line 2: _t29 = this.content
	@ rallocs: 
	ldr v7,[fp,#4]
	ldr v6,[v7,#4]
	
	@ line 3: println(_t29)
	@ rallocs: v6=_t29 v7=this
	ldr a1,=str29
	mov a2,v6
	bl printf(PLT)
	
	@ line 4: println("Solved?")
	@ rallocs: v7=this
	ldr a1,=str32
	bl printf(PLT)
	
	@ line 5: _t30 = this.solved
	@ rallocs: v7=this
	ldr v6,[v7,#8]
	
	@ line 6: println(_t30)
	@ rallocs: v6=_t30 v7=this
	ldr a1,=str33
	mov a2,v6
	bl printf(PLT)
	
	@ line 7: println("Assigned to:")
	@ rallocs: v7=this
	ldr a1,=str34
	bl printf(PLT)
	
	@ line 8: _t31 = this.assignee
	@ rallocs: v7=this
	ldr v6,[v7,#0]
	
	@ line 9: _t32 = _t31.secretIdentity
	@ rallocs: v6=_t31
	ldr v7,[v6,#4]
	
	@ line 10: println(_t32)
	@ rallocs: v7=_t32
	ldr a1,=str29
	mov a2,v7
	bl printf(PLT)
	
.L3:
	sub sp,fp,#32
	ldmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,pc}
	
Person_0:
	@ Function Person_0
	@ Local variable offsets and line of death:
	@   this : 4   	l. 9
	@   c : 8   	l. 3
	@   _t43 : -28   	l. 4
	@   _t44 : -32   	l. 7
	@   _t45 : -36   	l. 11
	
	stmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,lr}
	add fp,sp,#32
	sub sp,fp,#44
	
	@ line 1: c.assignee = this
	@ rallocs: a1=this a2=c
	str a1,[a2,#0]
	
	@ line 2: println("Case: ")
	@ rallocs: a1=this a2=c
	str a1,[fp,#4]
	str a2,[fp,#8]
	ldr a1,=str35
	bl printf(PLT)
	
	@ line 3: _t43 = c.content
	@ rallocs: 
	ldr v7,[fp,#8]
	ldr v6,[v7,#4]
	
	@ line 4: println(_t43)
	@ rallocs: v6=_t43
	ldr a1,=str29
	mov a2,v6
	bl printf(PLT)
	
	@ line 5: println("Assigned to: ")
	@ rallocs: 
	ldr a1,=str36
	bl printf(PLT)
	
	@ line 6: _t44 = this.secretIdentity
	@ rallocs: 
	ldr v7,[fp,#4]
	ldr v6,[v7,#4]
	
	@ line 7: println(_t44)
	@ rallocs: v6=_t44 v7=this
	ldr a1,=str29
	mov a2,v6
	bl printf(PLT)
	
	@ line 8: println("Assurance from superhero:")
	@ rallocs: v7=this
	ldr a1,=str37
	bl printf(PLT)
	
	@ line 9: _t45 = this.assignedQuote
	@ rallocs: v7=this
	ldr v6,[v7,#8]
	
	@ line 10: println(_t45)
	@ rallocs: v6=_t45
	ldr a1,=str29
	mov a2,v6
	bl printf(PLT)
	
.L4:
	sub sp,fp,#32
	ldmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,pc}
	
Person_1:
	@ Function Person_1
	@ Local variable offsets and line of death:
	@   this : 4   	l. 8
	@   _t39 : -28   	l. 3
	@   _t40 : -32   	l. 6
	@   _t41 : -36   	l. 9
	@   _t42 : -40   	l. 11
	
	stmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,lr}
	add fp,sp,#32
	sub sp,fp,#48
	
	@ line 1: println("Name: ")
	@ rallocs: a1=this
	str a1,[fp,#4]
	ldr a1,=str38
	bl printf(PLT)
	
	@ line 2: _t39 = this.name
	@ rallocs: 
	ldr v7,[fp,#4]
	ldr v6,[v7,#0]
	
	@ line 3: println(_t39)
	@ rallocs: v6=_t39 v7=this
	ldr a1,=str29
	mov a2,v6
	bl printf(PLT)
	
	@ line 4: println("Secret identity: ")
	@ rallocs: v7=this
	ldr a1,=str39
	bl printf(PLT)
	
	@ line 5: _t40 = this.secretIdentity
	@ rallocs: v7=this
	ldr v6,[v7,#4]
	
	@ line 6: println(_t40)
	@ rallocs: v6=_t40 v7=this
	ldr a1,=str29
	mov a2,v6
	bl printf(PLT)
	
	@ line 7: println("Toilet seat preference: ")
	@ rallocs: v7=this
	ldr a1,=str40
	bl printf(PLT)
	
	@ line 8: _t41 = this.pref
	@ rallocs: v7=this
	ldr v6,[v7,#16]
	
	@ line 9: _t42 = ToiletSeatPreference_0 (_t41)
	@ rallocs: v6=_t41
	mov a1,v6
	sub sp,sp,#4
	bl ToiletSeatPreference_0
	add sp,sp,#4
	mov v7,a1
	
	@ line 10: println(_t42)
	@ rallocs: v7=_t42
	ldr a1,=str29
	mov a2,v7
	bl printf(PLT)
	
.L5:
	sub sp,fp,#32
	ldmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,pc}
	
ToiletSeatPreference_0:
	@ Function ToiletSeatPreference_0
	@ Local variable offsets and line of death:
	@   this : 4   	l. 1
	@   _t46 : -28   	l. 3
	
	stmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,lr}
	add fp,sp,#32
	sub sp,fp,#36
	
	@ line 1: _t46 = this.dir
	@ rallocs: a1=this
	ldr v7,[a1,#0]
	
	@ line 2: Return _t46
	@ rallocs: v7=_t46
	mov a1,v7
	b .L6
	
.L6:
	sub sp,fp,#32
	ldmfd sp!,{v1,v2,v3,v4,v5,v6,v7,fp,pc}
