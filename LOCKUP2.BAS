'$INCLUDE: 'QB.BI'
DECLARE SUB RaporSil (satir, sutun, zemin)
DECLARE SUB LoadFont (CharSet$(), HowMany, Where)
DECLARE SUB TurkceFont ()
DECLARE SUB OrijinalFont ()
DECLARE SUB DisketBoot ()
DECLARE SUB Bekle ()
DECLARE SUB Hiz ()
DECLARE SUB AnaMenu ()
DECLARE SUB ProgramHazirla ()
DECLARE SUB EkranKartiTest ()
DECLARE SUB Kilitle ()
DECLARE SUB KilitAc ()
DECLARE SUB Disket (OpCode)
DECLARE SUB DisketVerify ()
DECLARE SUB OpErrPrint (OpCode, ErrMesaj$)
DECLARE SUB PrintOp (Msj$)
DECLARE SUB Harddisk (OpCode)
DECLARE SUB HDinit ()
DECLARE SUB Park ()
DECLARE SUB HDOperationOk (OpCode)
DECLARE SUB Intro ()
DECLARE SUB ProgramSonu ()

DIM SHARED Regs AS RegTypeX
DIM SHARED PartTable$(10), Verify$(10), BosAlan$(10), Sifre$(2), Yardim$(91), DiskBoot$(4)
DIM SHARED CharSet$(20), HowMany, Where, SysSpeed, F1$, HDnumber
DIM SHARED satir, sutun, zemin, zeminArka, LogoColor, Yazar$, Program$, Yil$, AnaSifre$
DIM SHARED ParkSide, ParkCyl, ParkSector, DonenTus$

CONST HDPartOku = 0, Track27Test = 1, Track26Test = 2, HDPartYaz = 4
CONST PartDisketYaz = 5, DisketPartVerify = 6, DisketBootInt = 7, PassYarat = 8
CONST PassDisketYaz = 9, DisketPassVerify = 10, PassHDYaz = 11, HDPassVerify = 12
CONST HDPartSil = 13, HDKilitlimi = 14, HDPartVerify = 15, DisketBootVerify = 16
CONST UnlockHDKilitlimi = 100, UnlockDisketPassOku = 101, UnlockHDPassOku = 102
CONST UnlockPassVerify = 103, UnlockDisketPartOku = 104, UnlockHDPartRestore = 105
CONST UnlockDisketPartVerify = 106, UnlockHDPartVerify = 107
CONST UnlockDisketPassVerify = 108, UnlockHDPassVerify = 109
CONST HD0 = &H1, FD0 = 0, Locked = 0, UnLocked = 1, HDPark = 200

CALL ProgramHazirla

Tus:
k$ = INKEY$: IF k$ = "" THEN GOTO Tus
CALL RaporSil(satir, sutun, zemin)
IF k$ = "k" OR k$ = "K" THEN GOTO Kilitle
IF k$ = "a" OR k$ = "A" THEN GOTO KilitAc
IF k$ = F1$ THEN GOTO OnlineYardim
IF k$ = CHR$(27) THEN COLOR 7, 0: CLS : CALL OrijinalFont: END
LOCATE satir + 16, sutun + 32: COLOR 15, zemin: PRINT "Tançmsçz Tuü": SOUND 3500, .2: CALL Bekle
CALL RaporSil(satir, sutun, zemin): GOTO Tus

'Kilitleme yordamç...........................................................
Kilitle:
LOCATE satir + 14, sutun + 20: COLOR 15, zemin: PRINT "HARDDòSK KòLòTLEME òûLEMò BAûLATILDI"
COLOR 31, zemin: LOCATE satir + 16, sutun + 16: PRINT "LÅtfen anahtar disketi takçp bir tuüa basçn."
COLOR 7, zemin: LOCATE satir + 18, sutun + 32: PRINT "òptal: "; : COLOR 15, zemin: PRINT "ESC"
k$ = INPUT$(1)
IF ASC(k$) = 27 THEN
 CALL RaporSil(satir, sutun, zemin)
 GOTO Tus
END IF
CALL RaporSil(satir, sutun, zemin)
LOCATE satir + 16, sutun + 26: COLOR 15, zemin: PRINT "HARDDòSK KòLòTLENòYOR..."
CALL Kilitle
CALL RaporSil(satir, sutun, zemin)
GOTO Tus

'Kilit aáma yordamç..........................................................
KilitAc:
LOCATE satir + 14, sutun + 23: COLOR 15, zemin: PRINT "HARDDòSK AÄMA òûLEMò BAûLATILDI"
COLOR 31, zemin: LOCATE satir + 16, sutun + 16: PRINT "LÅtfen anahtar disketi takçp bir tuüa basçn."
COLOR 7, zemin
COLOR 7, zemin: LOCATE satir + 18, sutun + 32: PRINT "òptal: "; : COLOR 15, zemin: PRINT "ESC"
k$ = INPUT$(1)
IF ASC(k$) = 27 THEN
 CALL RaporSil(satir, sutun, zemin)
 GOTO Tus
END IF
CALL RaporSil(satir, sutun, zemin)
LOCATE satir + 16, sutun + 28: COLOR 15, zemin: PRINT "HARDDòSK AÄILIYOR..."
CALL KilitAc
CALL RaporSil(satir, sutun, zemin)
GOTO Tus

'Online yardçm...............................................................

OnlineYardim:
PCOPY 0, 1
sira = 1
COLOR 15, zemin

LOCATE 1, 1: PRINT STRING$(80, " ")
COLOR 15, zemin: LOCATE 1, 37: PRINT Program$
COLOR 11, zemin: LOCATE 1, 2: PRINT "Harddisk GÅvenlik Yazçlçmç"
LOCATE 1, 72: PRINT Yil$
LOCATE 1, 57: PRINT Yazar$
COLOR LogoColor, zemin
LOCATE 1, 77: PRINT CHR$(250); CHR$(251); CHR$(252)
COLOR LogoColor, zeminArka
LOCATE 2, 77: PRINT CHR$(253); CHR$(254); CHR$(255)

COLOR 15, zemin
LOCATE satir, sutun: PRINT "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ YARDIM ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
FOR i = satir + 1 TO satir + 18: COLOR 15, zemin: LOCATE i, sutun: PRINT "∫"; : COLOR 3, zemin: PRINT STRING$(73, " "); : COLOR 15, zemin: PRINT "∫"; : COLOR 0, 0: PRINT "€€"
NEXT i
LOCATE satir + 19, sutun: COLOR 15, zemin: PRINT "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"; : COLOR 0, 0: PRINT "€€"
COLOR 0, 0: LOCATE satir + 20, sutun + 2: PRINT STRING$(75, "€")

Up$ = CHR$(0) + CHR$(72)
Down$ = CHR$(0) + CHR$(80)
PageUp$ = CHR$(0) + CHR$(73)
PageDown$ = CHR$(0) + CHR$(81)
Home$ = CHR$(0) + CHR$(71)
End$ = CHR$(0) + CHR$(79)

GOTO YardimGoster

YardimTus:
k$ = INKEY$
IF k$ = "" THEN GOTO YardimTus
IF k$ = CHR$(27) THEN
 PCOPY 1, 0
 GOTO Tus
END IF

IF k$ = Up$ AND sira > 1 THEN sira = sira - 1
IF k$ = Down$ AND sira <= 73 THEN sira = sira + 1
IF k$ = PageDown$ THEN
 sira = sira + 18
 IF sira > 73 THEN sira = 74
END IF
IF k$ = PageUp$ THEN
 sira = sira - 18
 IF sira < 1 THEN sira = 1
END IF
IF k$ = Home$ THEN sira = 1
IF k$ = End$ THEN sira = 74

YardimGoster:
COLOR 11, zemin
LOCATE satir + 1, sutun + 2: PRINT Yardim$(sira)
LOCATE satir + 2, sutun + 2: PRINT Yardim$(sira + 1)
LOCATE satir + 3, sutun + 2: PRINT Yardim$(sira + 2)
LOCATE satir + 4, sutun + 2: PRINT Yardim$(sira + 3)
LOCATE satir + 5, sutun + 2: PRINT Yardim$(sira + 4)
LOCATE satir + 6, sutun + 2: PRINT Yardim$(sira + 5)
LOCATE satir + 7, sutun + 2: PRINT Yardim$(sira + 6)
LOCATE satir + 8, sutun + 2: PRINT Yardim$(sira + 7)
LOCATE satir + 9, sutun + 2: PRINT Yardim$(sira + 8)
LOCATE satir + 10, sutun + 2: PRINT Yardim$(sira + 9)
LOCATE satir + 11, sutun + 2: PRINT Yardim$(sira + 10)
LOCATE satir + 12, sutun + 2: PRINT Yardim$(sira + 11)
LOCATE satir + 13, sutun + 2: PRINT Yardim$(sira + 12)
LOCATE satir + 14, sutun + 2: PRINT Yardim$(sira + 13)
LOCATE satir + 15, sutun + 2: PRINT Yardim$(sira + 14)
LOCATE satir + 16, sutun + 2: PRINT Yardim$(sira + 15)
LOCATE satir + 17, sutun + 2: PRINT Yardim$(sira + 16)
LOCATE satir + 18, sutun + 2: PRINT Yardim$(sira + 17)

GOTO YardimTus

'Yardçm tançmlamalarç........................................................
DATA "                 Bu program IBM ve uyumlusu bilgisayar sistemlerindeki "
DATA "Harddisk Ånitesini gÅvenlik altçna almak amacçyla hazçrlanmçütçr.      "
DATA "                 Yukarçda sîzÅ edilen gÅvenlik înlemi harddiskin kili- "
DATA "tlenmesi anlamçndadçr,yani LOCKUP harddiski kilitleyerek anahtar bir   "
DATA "disket oluüturmaktadçr.Harddiske eriüilmek istendißinde ise bu anahtar "
DATA "disket LOCKUP tarafçndan kullançlarak harddisk aáçlmaktadçr.Anahtar    "
DATA "disket harddiske eriüebilmenin tek yoludur.Anahtar disket îzenle sak - "
DATA "lanmalçdçr.                                                            "
DATA "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
DATA "1) Anahtar disketin hazçrlanmasç:                                      "
DATA "          3.5 inch , minimum 1.44 Mb'lçk saßlam bir disket alçp buna   "
DATA "normal format komutunu uygulayçn.Formatlama sonrasçnda diskette bozuk- "
DATA "luk oldußu rapor edilirse , baüka bir disket alçp aynç iülemi tekrarla-"
DATA "yçn (Dikkat! hçzlç formattan kaáçnçn).                                 "
DATA "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
DATA "2) Harddiskin kilitlenmesi:                                            "
DATA "                         LOCKUP'ç áalçütçrçn ve menÅden 'K' seáeneßini "
DATA "seáin,sizden anahtar disket istendißinde yukarçdaki üekilde hazçrladç- "
DATA "ßçnçz disketi 'A:' sÅrÅcÅsÅne takçp bir tuüa basçn (Disketin yazma ko- "
DATA "rumasçnç kaldçrçn) gerisini LOCKUP halledecektir.Ekran áçktçlarçnç in- "
DATA "celemeniz yeterlidir.                                                  "
DATA "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
DATA "3) Harddiskin aáçlmasç:                                                "
DATA "                     LOCKUP'ç áalçütçrçn ve menÅden 'A' seáeneßini se- "
DATA "áin. Sizden anahtar disket istenecektir,anahtar disketi takçp bir tuüa "
DATA "basçn ve ekran áçktçlarçna gîre gerekeni yapçn.                        "
DATA "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
DATA "4) Uyarçlar:                                                           "
DATA "                                                                       "
DATA " 4a) Bu program , MS-DOS dçüçnda tam anlamçyla test edilmemiütir.      "
DATA " 4b) LOCKUP sadece 1.Harddisk'i kilitler , bu akçldan áçkarçlmamalçdçr."
DATA " 4c) LOCKUP'ç kullanmadan înce bir sistem disketi hazçrlayçn ve test e-"
DATA "     din. Sistem disketi olarak kesinlikle MS-DOS install disketi kul- "
DATA "     lanmayçn !.                                                       "
DATA " 4d) Operasyon încesinde LOCKUP încelikle olasç hatalara karüç disket  "
DATA "     sÅrÅcÅye eriüir , hata mesajç verildißi zaman hatançn esas sebebi "
DATA "     iáin durum penceresine bakçlmalçdçr.                              "
DATA " 4e) Harddisk kilitli deßilse asla aáma ('A') seáeneßini kullanmayçn!. "
DATA " 4f) Hiábir operasyonu yarçda kesmeyin (mÅmkÅnse ESC tuüunu kullançn). "
DATA " 4g) Yapmak istedißiniz iü iáin doßru tuüa bastçßçnçzdan emin olun.    "
DATA " 4h) Kilitleme sçrasçnda disketin yazma korumasçnç kaldçrçn.           "
DATA " 4i) Zaten kilitli olan harddiski asla , aámadan tekrar kilitlemeyin!. "
DATA " 4j) Disketin takçlç olup olmadçßçnç ve saßlamlçßçnç kontrol edin      "
DATA "     (SCANDISK , PC Tools , Norton Disk Doctor , CHKDSK vs ile).       "
DATA " 4k) Disketin 'A' sÅrÅcÅsÅnde oldußundan emin olun.                    "
DATA " 4l) Anahtar disketi LOCKUP'çn dçüçnda kullanmayçn , iáine dosya kopya-"
DATA "     lamayçn !.                                                        "
DATA " 4m) Disket Åzerinde edit iülemi yapmayçn.                             "
DATA " 4n) Anahtar diskete asla format atmayçn !.                            "
DATA " 4o) Anahtar disketi manyetik ortamlardan koruyun.                     "
DATA " 4p) Harddiskiniz kilitliyken asla MS-DOS install disketiyle bilgisa-  "
DATA "     yarçnçzç aámayçn.ÄÅnkÅ MS-DOS'un install disketi harddiskinizi    "
DATA "     formatlayabilir !.                                                "
DATA " 4r) Bellekte kalan anti-virÅs programlarçnç ve BIOS'unuzun anti-virÅs "
DATA "     îzellißini -varsa- kapatçn. LOCKUP bunlarla uyuümamaktadçr.       "
DATA " 4s) LOCKUP kendinden TÅrkáe font desteßi sunar. Bu yÅzden en az VGA   "
DATA "     ekran kartç gerektirir.                                           "
DATA "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
DATA "5) LOCKUP , gÅvenlik aáçsçndan üu iülemleride yapar:                   "
DATA "                                                                       "
DATA " 5a) Harddisk üifrelenir ve bu üifre her kilitleme iüleminde deßiüir.  "
DATA "     Bu üifre anahtar diskete de yazçlçr.Yani her kilitleme iüleminden "
DATA "     sonra dißer anahtar disketler geáersiz olur.Bu sayede kazara baü- "
DATA "     kasçnçn eline geáen anahtar disket , harddiski aáamaz áÅnkÅ; Åze- "
DATA "     rindeki üifre , harddiskteki ile uyuümayacaktçr.                  "
DATA " 5b) Herhangi bir kilitleme iüleminden înce , anahtar olarak kullançla-"
DATA "     cak olan disketin saßlamlçßç test edilir.                         "
DATA " 5c) Anahtar disketle ilgili tÅm iülemlerde iülemin doßru yapçlçp ya-  "
DATA "     pçlmadçßç son bir kez daha gîzden geáirilir.                      "
DATA " 5d) TÅm iülemlerde kullançcç , olup biten den haberdar edilir.        "
DATA " 5e) Benzer anahtar disketler oluümamasç iáin yaratçlan üifre yÅz hane-"
DATA "     lidir.                                                            "
DATA " 5f) Anahtar disket bir DOS disketi deßildir.Anahtar disketin DOS ile  "
DATA "     okunmasç mÅmkÅn deßildir.DOS,disketin bozuk oldußu mesajçnç verir."
DATA " 5g) Sistem Anahtar disket ile boot edilmek istenirse ekrana 'LOCKUP'  "
DATA "     yazçsç áçkacak ve bir tuüa basçldçßçnda sistem yeniden baülatçla- "
DATA "     caktçr.                                                           "
DATA "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
DATA "ûu klasik sîzlerle aáçklamamç bitirmek istiyorum;                      "
DATA "                                                                       "
DATA "               LOCKUP yazçlçrken her aüamada test edildi ve hatalarç a-"
DATA "yçklandç.TÅm ihtimaller gîz înÅnde bulundurulmaya áalçüçldç. TÅm bu i- "
DATA "tinaya raßmen oluüabilecek herhangi bir zarardan programçn yazarç so - "
DATA "rumlu tutulamaz.                                                       "
DATA "                                                                       "
DATA "               Bu programç kullanmakla yukarçdaki üartç otomatik olarak"
DATA "kabul etmiü olmaktasçnçz.                                              "
DATA "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
DATA "                                                                       "
DATA "                                                                       "
DATA "                        - Yardçm BîlÅmÅ Sonu -                         "

'Turkáe Fontlar..............................................................
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,124,130,130,170,130,130,186,146,130,124,0,0,0,0
DATA 0,0,124,254,254,214,254,254,198,238,254,124,0,0,0,0
DATA 0,0,0,0,0,108,254,254,124,56,16,0,0,0,0,0
DATA 0,0,0,0,16,56,124,254,124,56,16,0,0,0,0,0
DATA 0,0,0,0,56,56,56,238,238,238,16,56,0,0,0,0
DATA 0,0,0,0,16,56,124,254,254,84,16,56,0,0,0,0
DATA 0,0,0,0,0,0,24,60,60,24,0,0,0,0,0,0
DATA 255,255,255,255,255,255,231,195,195,231,255,255,255,255,255,255
DATA 0,0,0,0,0,60,102,66,66,102,60,0,0,0,0,0
DATA 255,255,255,255,255,195,153,189,189,153,195,255,255,255,255,255
DATA 0,0,30,6,10,16,16,124,130,130,130,124,0,0,0,0
DATA 0,0,124,130,130,130,124,16,16,124,16,16,0,0,0,0
DATA 0,0,30,18,30,16,16,16,16,240,240,224,0,0,0,0
DATA 0,0,62,34,62,34,34,34,46,238,236,192,0,0,0,0
DATA 0,0,16,146,124,68,130,130,130,68,124,146,16,0,0,0
DATA 0,0,0,0,0,192,224,240,248,240,224,192,0,0,0,0
DATA 0,0,0,0,0,24,56,120,248,120,56,24,0,0,0,0
DATA 0,0,16,56,84,146,16,16,146,84,56,16,0,0,0,0
DATA 0,0,68,68,68,68,68,68,68,0,68,68,0,0,0,0
DATA 0,0,126,242,242,242,114,18,18,18,18,18,0,0,0,0
DATA 0,120,132,64,48,72,132,132,72,48,8,132,120,0,0,0
DATA 0,0,0,0,0,0,0,0,254,254,254,254,0,0,0,0
DATA 0,0,16,56,84,146,16,16,146,84,56,254,0,0,0,0
DATA 0,0,16,56,84,146,16,16,16,16,16,16,0,0,0,0
DATA 0,0,16,16,16,16,16,16,146,84,56,16,0,0,0,0
DATA 0,0,0,0,16,8,4,254,4,8,16,0,0,0,0,0
DATA 0,0,0,0,16,32,64,254,64,32,16,0,0,0,0,0
DATA 0,0,0,0,0,0,128,128,128,254,0,0,0,0,0,0
DATA 0,0,0,0,0,36,66,255,66,36,0,0,0,0,0,0
DATA 0,0,0,0,0,16,56,124,254,254,0,0,0,0,0,0
DATA 0,0,0,0,0,254,254,124,56,16,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,16,16,16,16,16,16,16,0,16,16,0,0,0,0
DATA 0,72,72,72,72,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,68,68,254,68,68,68,254,68,68,0,0,0,0
DATA 0,16,16,124,146,144,128,124,2,18,146,124,16,16,0,0
DATA 0,0,0,96,242,100,8,16,32,76,158,12,0,0,0,0
DATA 0,0,48,72,72,72,50,74,132,132,132,122,0,0,0,0
DATA 0,16,16,16,32,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,8,16,32,32,32,32,32,32,16,8,0,0,0,0
DATA 0,0,32,16,8,8,8,8,8,8,16,32,0,0,0,0
DATA 0,0,0,0,16,84,56,254,56,84,16,0,0,0,0,0
DATA 0,0,0,0,16,16,16,254,16,16,16,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,16,16,16,32,0,0
DATA 0,0,0,0,0,0,0,254,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,48,48,0,0,0,0
DATA 0,0,0,0,0,2,4,8,16,32,64,128,0,0,0,0
DATA 0,0,56,68,130,130,146,146,130,130,68,56,0,0,0,0
DATA 0,0,16,48,80,16,16,16,16,16,16,124,0,0,0,0
DATA 0,0,124,130,2,2,4,8,16,32,64,254,0,0,0,0
DATA 0,0,124,130,2,2,60,2,2,2,130,124,0,0,0,0
DATA 0,0,4,12,20,36,68,132,254,4,4,4,0,0,0,0
DATA 0,0,254,128,128,128,252,130,2,2,130,124,0,0,0,0
DATA 0,0,124,128,128,128,252,130,130,130,130,124,0,0,0,0
DATA 0,0,254,2,2,4,8,16,16,16,16,16,0,0,0,0
DATA 0,0,124,130,130,68,124,130,130,130,130,124,0,0,0,0
DATA 0,0,124,130,130,130,130,126,2,2,2,124,0,0,0,0
DATA 0,0,0,0,0,16,16,0,0,0,16,16,0,0,0,0
DATA 0,0,0,0,0,16,16,0,0,0,16,16,16,32,0,0
DATA 0,0,0,4,8,16,32,64,32,16,8,4,0,0,0,0
DATA 0,0,0,0,0,254,0,0,254,0,0,0,0,0,0,0
DATA 0,0,0,64,32,16,8,4,8,16,32,64,0,0,0,0
DATA 0,0,124,130,130,2,12,16,16,0,16,16,0,0,0,0
DATA 0,0,124,130,130,186,170,170,156,128,128,124,0,0,0,0
DATA 0,0,56,68,130,130,130,254,130,130,130,130,0,0,0,0
DATA 0,0,252,130,130,130,252,130,130,130,130,252,0,0,0,0
DATA 0,0,124,130,128,128,128,128,128,130,130,124,0,0,0,0
DATA 0,0,248,132,130,130,130,130,130,130,132,248,0,0,0,0
DATA 0,0,254,128,128,128,252,128,128,128,128,254,0,0,0,0
DATA 0,0,254,128,128,128,252,128,128,128,128,128,0,0,0,0
DATA 0,0,124,130,128,128,158,130,130,130,130,126,0,0,0,0
DATA 0,0,130,130,130,130,254,130,130,130,130,130,0,0,0,0
DATA 0,0,124,16,16,16,16,16,16,16,16,124,0,0,0,0
DATA 0,0,62,4,4,4,4,4,4,4,132,120,0,0,0,0
DATA 0,0,132,136,144,160,192,160,144,136,132,130,0,0,0,0
DATA 0,0,128,128,128,128,128,128,128,128,128,254,0,0,0,0
DATA 0,0,130,198,170,146,146,130,130,130,130,130,0,0,0,0
DATA 0,0,130,194,162,146,138,134,130,130,130,130,0,0,0,0
DATA 0,0,124,130,130,130,130,130,130,130,130,124,0,0,0,0
DATA 0,0,252,130,130,130,252,128,128,128,128,128,0,0,0,0
DATA 0,0,124,130,130,130,130,130,146,146,146,124,8,4,0,0
DATA 0,0,252,130,130,130,252,132,130,130,130,130,0,0,0,0
DATA 0,0,124,130,128,96,48,12,2,130,130,124,0,0,0,0
DATA 0,0,254,16,16,16,16,16,16,16,16,16,0,0,0,0
DATA 0,0,130,130,130,130,130,130,130,130,130,124,0,0,0,0
DATA 0,0,130,130,130,130,130,130,130,68,40,16,0,0,0,0
DATA 0,0,130,130,130,130,130,146,146,146,170,68,0,0,0,0
DATA 0,0,130,130,68,40,16,40,68,130,130,130,0,0,0,0
DATA 0,0,130,130,130,68,40,16,16,16,16,16,0,0,0,0
DATA 0,0,254,2,4,8,16,32,64,128,128,254,0,0,0,0
DATA 0,0,56,32,32,32,32,32,32,32,32,56,0,0,0,0
DATA 0,0,0,0,0,128,64,32,16,8,4,2,0,0,0,0
DATA 0,0,56,8,8,8,8,8,8,8,8,56,0,0,0,0
DATA 16,40,68,130,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,254,0,0
DATA 0,32,32,32,16,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,120,4,4,124,132,132,126,0,0,0,0
DATA 0,0,128,128,128,188,194,130,130,130,130,252,0,0,0,0
DATA 0,0,0,0,0,124,130,128,128,128,130,124,0,0,0,0
DATA 0,0,2,2,2,122,134,130,130,130,130,126,0,0,0,0
DATA 0,0,0,0,0,124,130,130,254,128,128,124,0,0,0,0
DATA 0,0,28,32,32,248,32,32,32,32,32,32,0,0,0,0
DATA 0,0,0,0,0,126,130,130,130,130,134,122,2,2,124,0
DATA 0,0,128,128,128,188,194,130,130,130,130,130,0,0,0,0
DATA 0,0,16,16,0,112,16,16,16,16,16,124,0,0,0,0
DATA 0,0,8,8,0,56,8,8,8,8,8,8,8,8,112,0
DATA 0,0,128,128,128,134,152,224,144,136,132,130,0,0,0,0
DATA 0,0,96,32,32,32,32,32,32,32,32,28,0,0,0,0
DATA 0,0,0,0,0,236,146,146,146,146,146,146,0,0,0,0
DATA 0,0,0,0,0,188,194,130,130,130,130,130,0,0,0,0
DATA 0,0,0,0,0,124,130,130,130,130,130,124,0,0,0,0
DATA 0,0,0,0,0,188,194,130,130,130,194,188,128,128,128,0
DATA 0,0,0,0,0,126,130,130,130,130,134,122,2,2,2,0
DATA 0,0,0,0,0,188,194,128,128,128,128,128,0,0,0,0
DATA 0,0,0,0,0,126,128,128,124,2,2,252,0,0,0,0
DATA 0,0,0,32,32,252,32,32,32,32,32,28,0,0,0,0
DATA 0,0,0,0,0,130,130,130,130,130,134,122,0,0,0,0
DATA 0,0,0,0,0,130,130,130,130,68,40,16,0,0,0,0
DATA 0,0,0,0,0,130,130,130,146,146,146,108,0,0,0,0
DATA 0,0,0,0,0,130,68,40,16,40,68,130,0,0,0,0
DATA 0,0,0,0,0,130,130,130,130,130,134,122,2,2,124,0
DATA 0,0,0,0,0,254,4,8,16,32,64,254,0,0,0,0
DATA 0,0,12,16,16,16,96,16,16,16,16,12,0,0,0,0
DATA 0,16,16,16,16,16,16,16,16,16,16,16,16,16,16,0
DATA 0,0,96,16,16,16,12,16,16,16,16,96,0,0,0,0
DATA 0,0,98,146,140,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,16,40,68,130,130,130,254,0,0,0,0
DATA 0,0,124,130,128,128,128,128,128,130,130,124,16,8,112,0
DATA 0,0,68,68,0,130,130,130,130,130,134,122,0,0,0,0
DATA 0,8,16,32,0,124,130,130,254,128,128,126,0,0,0,0
DATA 0,16,40,68,0,124,2,2,126,130,130,126,0,0,0,0
DATA 0,0,68,68,0,124,2,2,126,130,130,126,0,0,0,0
DATA 0,32,16,8,0,124,2,2,126,130,130,126,0,0,0,0
DATA 0,24,36,24,0,124,2,2,126,130,130,126,0,0,0,0
DATA 0,0,0,0,0,124,130,128,128,128,130,124,16,8,112,0
DATA 0,16,40,68,0,124,130,130,254,128,128,126,0,0,0,0
DATA 0,0,68,68,0,124,130,130,254,128,128,126,0,0,0,0
DATA 0,32,16,8,0,124,130,130,254,128,128,126,0,0,0,0
DATA 0,0,72,72,0,112,16,16,16,16,16,124,0,0,0,0
DATA 0,16,40,68,0,112,16,16,16,16,16,124,0,0,0,0
DATA 0,0,0,0,0,112,16,16,16,16,16,124,0,0,0,0
DATA 68,68,0,56,68,130,130,130,254,130,130,130,0,0,0,0
DATA 56,68,68,56,68,130,130,130,254,130,130,130,0,0,0,0
DATA 12,48,0,254,128,128,128,252,128,128,128,254,0,0,0,0
DATA 0,0,0,0,0,108,146,18,126,144,144,110,0,0,0,0
DATA 0,0,62,72,136,136,254,136,136,136,136,142,0,0,0,0
DATA 0,16,40,68,0,124,130,130,130,130,130,124,0,0,0,0
DATA 0,0,68,68,0,124,130,130,130,130,130,124,0,0,0,0
DATA 0,32,16,8,0,124,130,130,130,130,130,124,0,0,0,0
DATA 0,16,40,68,0,130,130,130,130,130,134,122,0,0,0,0
DATA 0,32,16,8,0,130,130,130,130,130,134,122,0,0,0,0
DATA 16,16,0,124,16,16,16,16,16,16,16,124,0,0,0,0
DATA 68,68,0,124,130,130,130,130,130,130,130,124,0,0,0,0
DATA 68,68,0,130,130,130,130,130,130,130,130,124,0,0,0,0
DATA 0,0,0,0,0,62,67,69,73,81,97,62,0,0,0,0
DATA 0,0,56,68,64,64,240,64,64,64,114,204,0,0,0,0
DATA 0,0,62,65,67,69,73,73,81,97,65,62,0,0,0,0
DATA 0,0,124,130,128,96,48,12,2,130,130,124,16,8,112,0
DATA 0,0,0,0,0,126,128,128,124,2,2,252,16,8,112,0
DATA 0,8,16,32,0,124,2,2,126,130,130,126,0,0,0,0
DATA 0,8,16,32,0,112,16,16,16,16,16,124,0,0,0,0
DATA 0,8,16,32,0,124,130,130,130,130,130,124,0,0,0,0
DATA 0,8,16,32,0,130,130,130,130,130,134,122,0,0,0,0
DATA 0,100,152,0,0,188,194,130,130,130,130,130,0,0,0,0
DATA 100,152,0,130,194,162,146,138,134,130,130,130,0,0,0,0
DATA 68,56,0,124,130,128,128,158,130,130,130,126,0,0,0,0
DATA 0,0,68,56,0,126,130,130,130,130,134,122,2,2,124,0
DATA 0,0,16,16,0,16,16,96,128,130,130,124,0,0,0,0
DATA 0,0,124,130,178,170,178,170,130,124,0,0,0,0,0,0
DATA 0,0,0,0,0,0,252,4,4,4,4,0,0,0,0,0
DATA 0,128,128,132,136,144,32,64,128,56,4,8,16,60,0,0
DATA 0,128,128,128,132,136,16,32,68,140,20,60,4,4,0,0
DATA 0,0,16,16,0,16,16,16,16,16,16,16,0,0,0,0
DATA 0,0,0,0,0,36,72,144,72,36,0,0,0,0,0,0
DATA 0,0,0,0,0,144,72,36,72,144,0,0,0,0,0,0
DATA 17,68,17,68,17,68,17,68,17,68,17,68,17,68,17,68
DATA 85,170,85,170,85,170,85,170,85,170,85,170,85,170,85,170
DATA 221,119,221,119,221,119,221,119,221,119,221,119,221,119,221,119
DATA 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
DATA 8,8,8,8,8,8,8,248,8,8,8,8,8,8,8,8
DATA 24,96,0,56,68,130,130,130,254,130,130,130,0,0,0,0
DATA 56,68,0,56,68,130,130,130,254,130,130,130,0,0,0,0
DATA 48,12,0,56,68,130,130,130,254,130,130,130,0,0,0,0
DATA 0,0,124,130,154,162,162,154,130,124,0,0,0,0,0,0
DATA 36,36,36,36,36,228,4,228,36,36,36,36,36,36,36,36
DATA 36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36
DATA 0,0,0,0,0,252,4,228,36,36,36,36,36,36,36,36
DATA 36,36,36,36,36,228,4,252,0,0,0,0,0,0,0,0
DATA 0,0,16,16,124,130,128,128,130,124,16,16,0,0,0,0
DATA 0,0,130,130,68,40,16,124,16,124,16,16,0,0,0,0
DATA 0,0,0,0,0,0,0,248,8,8,8,8,8,8,8,8
DATA 8,8,8,8,8,8,8,15,0,0,0,0,0,0,0,0
DATA 8,8,8,8,8,8,8,255,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,255,8,8,8,8,8,8,8,8
DATA 8,8,8,8,8,8,8,15,8,8,8,8,8,8,8,8
DATA 0,0,0,0,0,0,0,255,0,0,0,0,0,0,0,0
DATA 8,8,8,8,8,8,8,255,8,8,8,8,8,8,8,8
DATA 0,50,76,0,0,124,2,2,126,130,130,126,0,0,0,0
DATA 100,152,0,56,68,130,130,130,254,130,130,130,0,0,0,0
DATA 36,36,36,36,36,39,32,63,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,63,32,39,36,36,36,36,36,36,36,36
DATA 36,36,36,36,36,231,0,255,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,255,0,231,36,36,36,36,36,36,36,36
DATA 36,36,36,36,36,39,32,39,36,36,36,36,36,36,36,36
DATA 0,0,0,0,0,255,0,255,0,0,0,0,0,0,0,0
DATA 36,36,36,36,36,231,0,231,36,36,36,36,36,36,36,36
DATA 0,0,0,0,132,120,132,132,132,132,120,132,0,0,0,0
DATA 0,56,68,68,68,56,0,124,0,0,0,0,0,0,0,0
DATA 0,56,4,60,68,60,0,124,0,0,0,0,0,0,0,0
DATA 56,68,0,254,128,128,128,252,128,128,128,254,0,0,0,0
DATA 68,68,0,254,128,128,128,252,128,128,128,254,0,0,0,0
DATA 48,12,0,254,128,128,128,252,128,128,128,254,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 12,48,0,124,16,16,16,16,16,16,16,124,0,0,0,0
DATA 56,68,0,124,16,16,16,16,16,16,16,124,0,0,0,0
DATA 68,68,0,124,16,16,16,16,16,16,16,124,0,0,0,0
DATA 8,8,8,8,8,8,8,248,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,15,8,8,8,8,8,8,8,8
DATA 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
DATA 0,0,0,0,0,0,0,255,255,255,255,255,255,255,255,255
DATA 0,16,16,16,16,16,0,0,16,16,16,16,16,0,0,0
DATA 96,24,0,124,16,16,16,16,16,16,16,124,0,0,0,0
DATA 255,255,255,255,255,255,255,0,0,0,0,0,0,0,0,0
DATA 12,48,0,124,130,130,130,130,130,130,130,124,0,0,0,0
DATA 0,0,56,68,132,132,152,132,130,130,130,156,128,128,0,0
DATA 56,68,0,124,130,130,130,130,130,130,130,124,0,0,0,0
DATA 96,24,0,124,130,130,130,130,130,130,130,124,0,0,0,0
DATA 0,100,152,0,0,124,130,130,130,130,130,124,0,0,0,0
DATA 100,152,0,124,130,130,130,130,130,130,130,124,0,0,0,0
DATA 0,0,0,0,0,132,132,132,132,132,196,186,128,128,128,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,132,72,48,48,72,132,0,0,0,0,0,0
DATA 12,48,0,130,130,130,130,130,130,130,130,124,0,0,0,0
DATA 56,68,0,130,130,130,130,130,130,130,130,124,0,0,0,0
DATA 96,24,0,130,130,130,130,130,130,130,130,124,0,0,0,0
DATA 0,64,32,16,0,112,16,16,16,16,16,124,0,0,0,0
DATA 0,0,68,68,0,130,130,130,130,130,134,122,2,2,124,0
DATA 0,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,4,8,16,32,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,254,0,0,0,0,0,0,0,0
DATA 0,0,16,16,16,254,16,16,16,0,254,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,224,16,96,18,228,8,16,34,70,138,30,2,2,0,0
DATA 0,0,126,242,242,242,114,18,18,18,18,18,0,0,0,0
DATA 0,120,132,64,48,72,132,132,72,48,8,132,120,0,0,0
DATA 0,0,0,0,16,16,0,254,0,16,16,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,16,8,112,0,0
DATA 0,112,136,136,136,112,0,0,0,0,0,0,0,0,0,0
DATA 0,68,68,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,28,34,65,65,65,65,65,65,65,65,65,65,65,95
DATA 0,0,28,34,65,65,65,65,65,65,65,65,65,65,65,65
DATA 0,0,28,34,65,65,65,65,65,65,65,65,65,65,65,65
DATA 64,65,65,65,65,65,65,65,65,65,65,65,65,65,34,28
DATA 64,95,65,65,65,65,65,65,65,65,65,65,65,65,34,28
DATA 65,65,65,65,65,65,65,65,65,65,65,65,65,65,34,28

'Disket Boot Sector..........................................................
DATA 233,95,1
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 180,2,183,0,186,0,0,205,16,184,32,9,187,7,0
DATA 185,255,15,205,16,180,2,183,0,186,0,0,205,16
DATA 184,76,9,187,7,0,185,1,0,205,16,180,2,183,0
DATA 186,1,0,205,16,184,79,9,187,7,0,185,1,0,205
DATA 16,180,2,183,0,186,2,0,205,16,184,67,9,187,7
DATA 0,185,1,0,205,16,180,2,183,0,186,3,0,205,16
DATA 184,75,9,187,7,0,185,1,0,205,16,180,2,183,0
DATA 186,4,0,205,16,184,85,9,187,7,0,185,1,0,205
DATA 16,180,2,183,0,186,5,0,205,16,184,80,9,187,7
DATA 0,185,1,0,205,16,180,2,183,0,186,6,0,205,16
DATA 180,0,205,22,234,0,0,255,255

SUB AnaMenu

COLOR 0, zeminArka: CLS
satir = 3: sutun = 3
COLOR 0, zemin: PRINT STRING$(80, " ")
COLOR 15, zemin: LOCATE 1, 37: PRINT Program$
COLOR 11, zemin: LOCATE 1, 2: PRINT "Harddisk GÅvenlik Yazçlçmç"
LOCATE 1, 72: PRINT Yil$
LOCATE 1, 57: PRINT Yazar$
COLOR LogoColor, zemin
LOCATE 1, 77: PRINT CHR$(250); CHR$(251); CHR$(252)
COLOR LogoColor, zeminArka
LOCATE 2, 77: PRINT CHR$(253); CHR$(254); CHR$(255)

COLOR 15, zemin
LOCATE satir, sutun: PRINT "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ INFO ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
FOR i = 2 TO 10: COLOR 15, zemin: LOCATE (satir - 1) + i, sutun: PRINT "∫"; : COLOR 3, zemin: PRINT STRING$(73, " "); : COLOR 15, zemin: PRINT "∫"; : COLOR 0, 0: PRINT "€€"
NEXT i
LOCATE satir + 10, sutun: COLOR 15, zemin: PRINT "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"; : COLOR 0, 0: PRINT "€€"
COLOR 0, 0: LOCATE satir + 11, sutun + 2: PRINT STRING$(75, "€")

LOCATE satir + 2, sutun + 2: COLOR 11, zemin: PRINT "Programcç..: "; : COLOR 15, zemin: PRINT Yazar$
LOCATE satir + 3, sutun + 2: COLOR 11, zemin: PRINT "Versiyon...: "; : COLOR 15, zemin: PRINT "1.50"
LOCATE satir + 5, sutun + 2: COLOR 11, zemin: PRINT "Kilitleme..: "; : COLOR 15, zemin: PRINT "K"
LOCATE satir + 6, sutun + 2: COLOR 11, zemin: PRINT "Kilit aáma.: "; : COLOR 15, zemin: PRINT "A"
LOCATE satir + 7, sutun + 2: COLOR 11, zemin: PRINT "Yardçm.....: "; : COLOR 15, zemin: PRINT "F1"
LOCATE satir + 8, sutun + 2: COLOR 11, zemin: PRINT "Äçkçü......: "; : COLOR 15, zemin: PRINT "Esc"
REM LOCATE satir + 11, sutun + 16: COLOR 7, zemin: PRINT "( Hard disk kilitleme ve kilit aáma yazçlçmç. )"


COLOR 15, zemin: LOCATE satir + 13, sutun: PRINT "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ MESAJ ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
CALL RaporSil(satir, sutun, zemin)
LOCATE satir + 19, sutun: COLOR 15, zemin: PRINT "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"; : COLOR 0, 0: PRINT "€€"
COLOR 0, 0: LOCATE satir + 20, sutun + 2: PRINT STRING$(75, "€")

END SUB

SUB Bekle
 FOR i = 1 TO SysSpeed * 30
 NEXT
END SUB

SUB Disket (OpCode)

CALL DisketVerify
SELECT CASE OpCode

       CASE Track27Test
	    Regs.ax = &H402
	    Regs.cx = &H2701
	    Regs.dx = FD0
	    CALL INTERRUPTX(&H13, Regs, Regs)
	    IF Regs.flags AND 1 THEN OpCode = -1: EXIT SUB
	    IF (Regs.ax AND 255) <> 2 OR (Regs.ax AND 65280) <> 0 THEN OpCode = -1

       CASE Track26Test
	    Regs.ax = &H401
	    Regs.cx = &H2601
	    Regs.dx = FD0
	    CALL INTERRUPTX(&H13, Regs, Regs)
	    IF Regs.flags AND 1 THEN OpCode = -1: EXIT SUB
	    IF (Regs.ax AND 255) <> 1 OR (Regs.ax AND 65280) <> 0 THEN OpCode = -1

       CASE PartDisketYaz
	    Regs.ax = &H302
	    Regs.cx = &H2701
	    Regs.dx = FD0
	    Regs.es = VARSEG(PartTable$(0))
	    Regs.bx = SADD(PartTable$(0))
	    CALL INTERRUPTX(&H13, Regs, Regs)
	    IF Regs.flags AND 1 THEN OpCode = -1

       CASE DisketPartVerify
	    Regs.ax = &H202
	    Regs.cx = &H2701
	    Regs.dx = FD0
	    Regs.es = VARSEG(Verify$(0))
	    Regs.bx = SADD(Verify$(0))
	    CALL INTERRUPTX(&H13, Regs, Regs)
	    IF Regs.flags AND 1 THEN OpCode = -1: EXIT SUB
	    IF LEFT$(PartTable$(0), 2048) <> LEFT$(Verify$(0), 2048) THEN OpCode = -2

       CASE DisketBootInt
	    Regs.ax = &H301
	    Regs.cx = &H1
	    Regs.dx = FD0
	    Regs.es = VARSEG(DiskBoot$(0))
	    Regs.bx = SADD(DiskBoot$(0))
	    CALL INTERRUPTX(&H13, Regs, Regs)
	    IF Regs.flags AND 1 THEN OpCode = -1

       CASE DisketBootVerify
	    Regs.ax = &H201
	    Regs.cx = &H1
	    Regs.dx = FD0
	    Regs.es = VARSEG(Verify$(0))
	    Regs.bx = SADD(Verify$(0))
	    CALL INTERRUPTX(&H13, Regs, Regs)
	    IF Regs.flags AND 1 THEN OpCode = -1
	    IF LEFT$(DiskBoot$(0), 512) <> LEFT$(Verify$(0), 512) THEN OpCode = -2

       CASE PassYarat
	    RANDOMIZE (TIMER)
	    Sifre$ = ""
	    FOR i = 1 TO 79
	     Sifre$ = Sifre$ + CHR$(INT(RND(1) * 26) + 65)
	    NEXT
	    Sifre$ = Sifre$ + AnaSifre$
	    Sifre$(0) = Sifre$ + STRING$(1948, CHR$(0))

       CASE PassDisketYaz
	    Regs.ax = &H301
	    Regs.cx = &H2601
	    Regs.dx = FD0
	    Regs.es = VARSEG(Sifre$(0))
	    Regs.bx = SADD(Sifre$(0))
	    CALL INTERRUPTX(&H13, Regs, Regs)
	    IF Regs.flags AND 1 THEN OpCode = -1

       CASE DisketPassVerify
	    Regs.ax = &H201
	    Regs.cx = &H2601
	    Regs.dx = FD0
	    Regs.es = VARSEG(Verify$(0))
	    Regs.bx = SADD(Verify$(0))
	    CALL INTERRUPTX(&H13, Regs, Regs)
	    IF Regs.flags AND 1 THEN OpCode = -1: EXIT SUB
	    IF LEFT$(Sifre$(0), 100) <> LEFT$(Verify$(0), 100) THEN OpCode = -2

END SELECT


SELECT CASE OpCode
      
       CASE UnlockDisketPassOku
	    Regs.ax = &H201
	    Regs.cx = &H2601
	    Regs.dx = FD0
	    Regs.es = VARSEG(PartTable$(0))
	    Regs.bx = SADD(PartTable$(0))
	    CALL INTERRUPTX(&H13, Regs, Regs)
	    IF Regs.flags AND 1 THEN OpCode = -1
      
       CASE UnlockDisketPassVerify
	    Regs.ax = &H201
	    Regs.cx = &H2601
	    Regs.dx = FD0
	    Regs.es = VARSEG(Verify$(0))
	    Regs.bx = SADD(Verify$(0))
	    CALL INTERRUPTX(&H13, Regs, Regs)
	    IF Regs.flags AND 1 THEN OpCode = -1
	    IF LEFT$(PartTable$(0), 100) <> LEFT$(Verify$(0), 100) THEN OpCode = -2

       CASE UnlockDisketPartOku
	    Regs.ax = &H202
	    Regs.cx = &H2701
	    Regs.dx = FD0
	    Regs.es = VARSEG(PartTable$(0))
	    Regs.bx = SADD(PartTable$(0))
	    CALL INTERRUPTX(&H13, Regs, Regs)
	    IF Regs.flags AND 1 THEN OpCode = -1

       CASE UnlockDisketPartVerify
	    Regs.ax = &H202
	    Regs.cx = &H2701
	    Regs.dx = FD0
	    Regs.es = VARSEG(Verify$(0))
	    Regs.bx = SADD(Verify$(0))
	    CALL INTERRUPTX(&H13, Regs, Regs)
	    IF Regs.flags AND 1 THEN OpCode = -1
	    IF LEFT$(PartTable$(0), 100) <> LEFT$(Verify$(0), 100) THEN OpCode = -1

END SELECT

ExitDisketRoutine:
END SUB

SUB DisketBoot

 FOR i = 1 TO 512
 READ byte
 byte$ = CHR$(byte)
 MID$(DiskBoot$(0), i, 1) = byte$
 NEXT

END SUB

SUB DisketVerify

FOR jjj = 1 TO 2
 Regs.ax = &H201
 Regs.cx = &H1
 Regs.dx = FD0
 Regs.es = VARSEG(BosAlan$(0))
 Regs.bx = SADD(BosAlan$(0))
 CALL INTERRUPTX(&H13, Regs, Regs)
NEXT jjj

END SUB

SUB EkranKartiTest

SCREEN 0: COLOR 7, 0: CLS : WIDTH 80, 25
CALL INTERRUPTX(&H11, Regs, Regs)
Adaptor1 = Regs.ax AND 48

Regs.ax = &H1A00
CALL INTERRUPTX(&H10, Regs, Regs)
Adaptor2 = Regs.ax AND 255
CLS
IF (Adaptor1 <> 16 AND Adaptor1 <> 32) OR Adaptor2 <> &H1A THEN
 CALL ProgramSonu
 PRINT Program$; " , "; Yazar$; " , "; Yil$
 PRINT "Bu programçn áalçüabilmesi iáin en az , VGA ekran kartç gerekir."
 BEEP
 END
END IF

END SUB

SUB Harddisk (OpCode)

SELECT CASE OpCode
      
       CASE HDPartOku
	    Regs.ax = &H202
	    Regs.cx = &H1
	    Regs.dx = HD0
	    Regs.es = VARSEG(PartTable$(0))
	    Regs.bx = SADD(PartTable$(0))
	    CALL INTERRUPTX(&H13, Regs, Regs)
	    IF Regs.flags AND 1 THEN OpCode = -1
      
       CASE HDPartVerify
	    Regs.ax = &H202
	    Regs.cx = &H1
	    Regs.dx = HD0
	    Regs.es = VARSEG(Verify$(0))
	    Regs.bx = SADD(Verify$(0))
	    CALL INTERRUPTX(&H13, Regs, Regs)
	    IF Regs.flags AND 1 THEN OpCode = -1: EXIT SUB
	    IF LEFT$(PartTable$(0), 2048) <> LEFT$(Verify$(0), 2048) THEN OpCode = -2

       CASE HDKilitlimi
	    Regs.ax = &H201
	    Regs.cx = &H2
	    Regs.dx = HD0
	    Regs.es = VARSEG(PartTable$(0))
	    Regs.bx = SADD(PartTable$(0))
	    CALL INTERRUPTX(&H13, Regs, Regs)
	    IF Regs.flags AND 1 THEN OpCode = -1
	    IF MID$(PartTable$(0), 80, 21) = AnaSifre$ THEN OpCode = -2
      
       CASE HDPartYaz
	    Regs.ax = &H302
	    Regs.cx = &H1
	    Regs.dx = HD0
	    Regs.es = VARSEG(PartTable$(0))
	    Regs.bx = SADD(PartTable$(0))
	    CALL INTERRUPTX(&H13, Regs, Regs)
	    IF Regs.flags AND 1 THEN OpCode = -1
      
	    FOR sayac = 0 TO 48 STEP 16
	     Side = ASC(MID$(PartTable$(0), 452 + sayac, 1))
	     Sector = ASC(MID$(PartTable$(0), 453 + sayac, 1)) AND 63
	     HighCyl = ((ASC(MID$(PartTable$(0), 453 + sayac, 1)) AND 192) / 64) * 256
	     Cyl = ASC(MID$(PartTable$(0), 454 + sayac, 1)) + HighCyl

	     IF Side <> 0 OR Cyl <> 0 OR Sector <> 0 THEN
	      ParkSide = Side
	      ParkCyl = Cyl
	      ParkSector = Sector
	     END IF
	    NEXT
      
       CASE PassHDYaz
	    Regs.ax = &H301
	    Regs.cx = &H2
	    Regs.dx = HD0
	    Regs.es = VARSEG(Sifre$(0))
	    Regs.bx = SADD(Sifre$(0))
	    CALL INTERRUPTX(&H13, Regs, Regs)
	    IF Regs.flags AND 1 THEN OpCode = -1
      
       CASE HDPassVerify
	    Regs.ax = &H201
	    Regs.cx = &H2
	    Regs.dx = HD0
	    Regs.es = VARSEG(Verify$(0))
	    Regs.bx = SADD(Verify$(0))
	    CALL INTERRUPTX(&H13, Regs, Regs)
	    IF Regs.flags AND 1 THEN OpCode = -1: EXIT SUB
	    IF LEFT$(Sifre$(0), 100) <> LEFT$(Verify$(0), 100) THEN OpCode = -2
      
       CASE HDPartSil
	    PartTable$(0) = STRING$(2048, CHR$(0))
	    Regs.ax = &H301
	    Regs.cx = &H1
	    Regs.dx = HD0
	    Regs.es = VARSEG(PartTable$(0))
	    Regs.bx = SADD(PartTable$(0))
	    CALL INTERRUPTX(&H13, Regs, Regs)
	    IF Regs.flags AND 1 THEN OpCode = -1

       CASE HDPark
	    cx = ((ParkCyl AND 255) * 256) + (((ParkCyl AND 768) / 256) * 64) + ParkSector
	    IF cx > 32767 THEN cx = cx - 65536
	    
	    Regs.ax = &H0
	    CALL INTERRUPTX(&H13, Regs, Regs)
	    IF Regs.flags AND 1 THEN OpCode = -1: EXIT SUB

	    Regs.ax = &HC00
	    Regs.cx = cx
	    Regs.dx = (ParkSide * 256) + &H80
	    CALL INTERRUPTX(&H13, Regs, Regs)
	    IF Regs.flags AND 1 THEN OpCode = -1: EXIT SUB


END SELECT


SELECT CASE OpCode
      
       CASE UnlockHDKilitlimi
	    Regs.ax = &H201
	    Regs.cx = &H1
	    Regs.dx = HD0
	    Regs.es = VARSEG(PartTable$(0))
	    Regs.bx = SADD(PartTable$(0))
	    CALL INTERRUPTX(&H13, Regs, Regs)
	    IF Regs.flags AND 1 THEN OpCode = -1: EXIT SUB
	    IF LEFT$(PartTable$(0), 512) <> STRING$(512, CHR$(0)) THEN OpCode = -2
      
       CASE UnlockHDPassOku
	    Regs.ax = &H201
	    Regs.cx = &H2
	    Regs.dx = HD0
	    Regs.es = VARSEG(Verify$(0))
	    Regs.bx = SADD(Verify$(0))
	    CALL INTERRUPTX(&H13, Regs, Regs)
	    IF Regs.flags AND 1 THEN OpCode = -1
     
       CASE UnlockHDPassVerify
	    Regs.ax = &H201
	    Regs.cx = &H2
	    Regs.dx = HD0
	    Regs.es = VARSEG(BosAlan$(0))
	    Regs.bx = SADD(BosAlan$(0))
	    CALL INTERRUPTX(&H13, Regs, Regs)
	    IF Regs.flags AND 1 THEN OpCode = -1
	    IF LEFT$(BosAlan$(0), 100) <> LEFT$(Verify$(0), 100) THEN OpCode = -2
      
       CASE UnlockPassVerify
	    IF LEFT$(PartTable$(0), 100) <> LEFT$(Verify$(0), 100) THEN OpCode = -1
      
       CASE UnlockHDPartRestore
	    Regs.ax = &H302
	    Regs.cx = &H1
	    Regs.dx = HD0
	    Regs.es = VARSEG(PartTable$(0))
	    Regs.bx = SADD(PartTable$(0))
	    CALL INTERRUPTX(&H13, Regs, Regs)
	    IF Regs.flags AND 1 THEN OpCode = -1

       CASE UnlockHDPartVerify
	    Regs.ax = &H202
	    Regs.cx = &H1
	    Regs.dx = HD0
	    Regs.es = VARSEG(Verify$(0))
	    Regs.bx = SADD(Verify$(0))
	    CALL INTERRUPTX(&H13, Regs, Regs)
	    IF Regs.flags AND 1 THEN OpCode = -1: EXIT SUB
	    IF LEFT$(PartTable$(0), 2048) <> LEFT$(Verify$(0), 2048) THEN OpCode = -2


END SELECT


END SUB

SUB HDOperationOk (OpCode)

COLOR 7, 0: CLS
COLOR 15, zemin
LOCATE 10, 22: PRINT "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
LOCATE 11, 22: PRINT "∫               - "; Program$; " -               ∫"
LOCATE 12, 22: PRINT "∫                                        ∫"
LOCATE 13, 22: PRINT "∫                                        ∫"
LOCATE 14, 22: PRINT "∫                                        ∫"
LOCATE 15, 22: PRINT "∫                                        ∫"
LOCATE 16, 22: PRINT "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"

COLOR 11, zemin

IF OpCode = Locked THEN
 LOCATE 13, 31: PRINT "Harddiskiniz kilitlendi."
 LOCATE 14, 31: PRINT "Harddiskiniz park edildi."
 LOCATE 15, 28: PRINT "LÅtfen bilgisayarçnçzç kapatçn."
END IF

IF OpCode = UnLocked THEN
 LOCATE 13, 33: PRINT "Harddiskiniz aáçldç. "
 LOCATE 14, 26: PRINT "LÅtfen bilgisayarçnçzç resetleyin."
END IF

COLOR 7, 0: BEEP: k$ = INPUT$(1): CLS
CALL OrijinalFont
END

END SUB

SUB Hiz

again:
 t = TIMER
 WHILE TIMER = t
 SysSpeed = SysSpeed + 1
 WEND
IF bossay = 0 THEN bossay = 1: SysSpeed = 0: GOTO again

END SUB

SUB Intro

COLOR 0, zeminArka: CLS
satir = 9: sutun = 3

COLOR 15, zemin
LOCATE satir, sutun: PRINT "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
FOR i = satir + 1 TO satir + 7: COLOR 15, zemin: LOCATE i, sutun: PRINT "∫"; : COLOR 3, zemin: PRINT STRING$(73, " "); : COLOR 15, zemin: PRINT "∫"; : COLOR 0, 0: PRINT "€€"
NEXT i
LOCATE satir + 8, sutun: COLOR 15, zemin: PRINT "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"; : COLOR 0, 0: PRINT "€€"
COLOR 0, 0: LOCATE satir + 9, sutun + 2: PRINT STRING$(75, "€")
COLOR LogoColor, zemin: LOCATE satir + 1, sutun + 70: PRINT CHR$(250); CHR$(251); CHR$(252)
COLOR LogoColor, zemin: LOCATE satir + 2, sutun + 70: PRINT CHR$(253); CHR$(254); CHR$(255)
LOCATE satir + 3, sutun + 35: COLOR 15, zemin: PRINT Program$
LOCATE satir + 5, sutun + 36: COLOR 11, zemin: PRINT Yil$
LOCATE satir + 1, sutun + 55: COLOR 15, zemin: PRINT Yazar$
LOCATE satir + 4, sutun + 27: COLOR 11, zemin: PRINT "Harddisk GÅvenlik Yazçlçmç"
LOCATE satir + 1, sutun + 2: COLOR 15, zemin: PRINT ""
k$ = INPUT$(1)
IF k$ = CHR$(27) THEN CALL ProgramSonu: END

END SUB

SUB KilitAc

CALL PrintOp("Harddisk'in Durumu ònceleniyor")
OpCode = UnlockHDKilitlimi
CALL Harddisk(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(80, ""): EXIT SUB
IF OpCode = -2 THEN
 LOCATE 21, 66: COLOR 7, zemin: PRINT "òptal:"; : COLOR 15, zemin: PRINT "ESC"
 CALL OpErrPrint(80, "Harddisk Kilitli Olmayabilir")
 IF DonenTus$ = CHR$(27) THEN CALL RaporSil(satir, sutun, zemin): EXIT SUB
END IF

CALL PrintOp("Disket'in ûifresi Okunuyor")
OpCode = UnlockDisketPassOku
CALL Disket(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(0, ""): EXIT SUB

CALL PrintOp("Disket'in ûifresi Test Ediliyor")
OpCode = UnlockDisketPassVerify
CALL Disket(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(0, ""): EXIT SUB
IF OpCode = -2 THEN CALL OpErrPrint(0, "Disket'in ûifresi Doßru Okunamçyor"): EXIT SUB

CALL PrintOp("Harddisk'in ûifresi Okunuyor")
OpCode = UnlockHDPassOku
CALL Harddisk(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(80, ""): EXIT SUB

CALL PrintOp("Harddisk'in ûifresi Test Ediliyor")
OpCode = UnlockHDPassVerify
CALL Harddisk(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(80, ""): EXIT SUB
IF OpCode = -2 THEN CALL OpErrPrint(80, "Harddisk'in ûifresi Doßru Okunamçyor"): EXIT SUB

CALL PrintOp("ûifreler Karüçlaütçrçlçyor")
OpCode = UnlockPassVerify
CALL Harddisk(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(255, "Anahtar Disket Geáersiz"): EXIT SUB

CALL PrintOp("Disket'ten Harddisk Bilgileri Okunuyor")
OpCode = UnlockDisketPartOku
CALL Disket(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(0, ""): EXIT SUB

CALL PrintOp("Disket'ten Okunan Veriler Test Ediliyor")
OpCode = UnlockDisketPartVerify
CALL Disket(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(0, ""): EXIT SUB

CALL PrintOp("Harddisk Aáçlçyor")
OpCode = UnlockHDPartRestore
CALL Harddisk(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(80, "Harddisk Aáçlamçyor"): EXIT SUB

CALL PrintOp("Harddisk Test Ediliyor")
OpCode = UnlockHDPartVerify
CALL Harddisk(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(80, "Harddisk Aáçlamçyor"): EXIT SUB
IF OpCode = -2 THEN CALL OpErrPrint(80, "Harddisk Tam Olarak Aáçlamçyor"): EXIT SUB

CALL HDOperationOk(UnLocked)
ExitKilitAcRoutine:
CALL RaporSil(satir, sutun, zemin)

END SUB

SUB Kilitle

CALL PrintOp("Harddisk'in Durumu ònceleniyor")
OpCode = HDKilitlimi
CALL Harddisk(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(80, ""): EXIT SUB
IF OpCode = -2 THEN CALL OpErrPrint(80, "Harddisk Zaten Kilitli"): EXIT SUB

CALL PrintOp("Disket Test Ediliyor")
OpCode = Track27Test
CALL Disket(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(0, "Disket YÅzeyinde Hata Tespit Edildi"): EXIT SUB

CALL PrintOp("Disket Test Ediliyor")
OpCode = Track26Test
CALL Disket(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(0, "Disket YÅzeyinde Hata Tespit Edildi"): EXIT SUB

CALL PrintOp("Harddisk Verileri Okunuyor")
OpCode = HDPartOku
CALL Harddisk(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(80, ""): EXIT SUB

CALL PrintOp("Harddisk Verileri Test Ediliyor")
OpCode = HDPartVerify
CALL Harddisk(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(80, ""): EXIT SUB
IF OpCode = -2 THEN CALL OpErrPrint(80, "Harddisk Verileri Doßru Okunamçyor"): EXIT SUB

CALL PrintOp("Boot Sector Koruyucularç Tetikleniyor")
OpCode = HDPartYaz
CALL Harddisk(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(80, ""): EXIT SUB

CALL PrintOp("Veriler Diskete Yazçlçyor")
OpCode = PartDisketYaz
CALL Disket(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(0, ""): EXIT SUB

CALL PrintOp("Disket'teki Veriler Test Ediliyor")
OpCode = DisketPartVerify
CALL Disket(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(0, ""): EXIT SUB
IF OpCode = -2 THEN CALL OpErrPrint(0, "Veriler Doßru Yazçlamçyor"): EXIT SUB

CALL PrintOp("ûifre Oluüturuluyor")
OpCode = PassYarat
CALL Disket(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(255, ""): EXIT SUB

CALL PrintOp("ûifre Disket'e Yazçlçyor")
OpCode = PassDisketYaz
CALL Disket(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(0, ""): EXIT SUB

CALL PrintOp("Disket ûifresi Test Ediliyor")
OpCode = DisketPassVerify
CALL Disket(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(0, ""): EXIT SUB
IF OpCode = -2 THEN CALL OpErrPrint(0, "ûifre Doßru Yazçlamçyor"): EXIT SUB

CALL PrintOp("ûifre Harddisk'e Yazçlçyor")
OpCode = PassHDYaz
CALL Harddisk(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(80, ""): EXIT SUB

CALL PrintOp("Harddisk ûifresi Test Ediliyor")
OpCode = HDPassVerify
CALL Harddisk(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(80, ""): EXIT SUB
IF OpCode = -2 THEN CALL OpErrPrint(80, "ûifre Doßru Yazçlamçyor"): EXIT SUB

CALL PrintOp("Disket'in Boot Sector'u Deßiütiriliyor")
OpCode = DisketBootInt
CALL Disket(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(0, ""): EXIT SUB

CALL PrintOp("Disket'in Boot Sector'u Test Ediliyor")
OpCode = DisketBootVerify
CALL Disket(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(0, ""): EXIT SUB

CALL PrintOp("Harddisk Kilitleniyor")
OpCode = HDPartSil
CALL Harddisk(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(80, "Harddisk Kilitlenemiyor"): EXIT SUB

CALL PrintOp("Harddisk'in Kiliti ònceleniyor")
OpCode = HDKilitlimi
CALL Harddisk(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(80, "Test Sçrasçnda Hata Oldu"): EXIT SUB
IF OpCode <> -2 THEN CALL OpErrPrint(80, "Harddisk Kilitlenemiyor"): EXIT SUB

CALL PrintOp("Harddisk Kafalarç Park Ediliyor")
OpCode = HDPark
CALL Harddisk(OpCode)
IF OpCode = -1 THEN CALL OpErrPrint(80, "Harddisk Kafalarç Park Edilemiyor"): EXIT SUB

CALL HDOperationOk(Locked)
ExitKilitleRoutine:
CALL RaporSil(satir, sutun, zemin)

END SUB

SUB LoadFont (CharSet$(), HowMany, Where)
  Regs.ax = &H1100
  Regs.bx = &H1000
  Regs.cx = HowMany
  Regs.dx = Where
  Regs.es = VARSEG(CharSet$(0))
  Regs.bp = SADD(CharSet$(0))
  CALL INTERRUPTX(&H10, Regs, Regs)
END SUB

SUB OpErrPrint (OpCode, ErrMesaj$)

IF OpCode = 80 THEN Driver$ = "Harddisk" ELSE Driver$ = "Disket"
IF OpCode = 255 THEN Driver$ = ""
IF ErrMesaj$ = "" THEN ErrMesaj$ = "Hata Oldu!"
Yer = LEN(ErrMesaj$) / 2
Yer2 = LEN(Driver$) / 2

COLOR 15, zemin: LOCATE satir + 16, sutun + 38 - Yer2: PRINT Driver$
COLOR 31, zemin: LOCATE satir + 18, sutun + 38 - Yer: PRINT ErrMesaj$
DonenTus$ = INPUT$(1)

END SUB

SUB OrijinalFont
 
  Regs.ax = &H1130
  Regs.bx = &H600
  CALL INTERRUPTX(&H10, Regs, Regs)
  segment = Regs.es
  pointer = Regs.bp
  DEF SEG = segment
  FOR i = 0 TO 4095
   MID$(CharSet$(0), i + 1, 1) = CHR$(PEEK(pointer + i))
  NEXT
  DEF SEG
  CALL LoadFont(CharSet$(), 256, 0)

END SUB

SUB PrintOp (Msj$)

CALL RaporSil(satir, sutun, zemin)
Yer = LEN(Msj$) / 2
LOCATE satir + 14, sutun + 38 - Yer: COLOR 15, zemin: PRINT Msj$

END SUB

SUB ProgramHazirla

zemin = 1
zeminArka = 3
LogoColor = 11

HowMany = 256: Where = 0
PartTable$(0) = SPACE$(2048): Verify$(0) = SPACE$(2048): Sifre$(0) = SPACE$(2048): CharSet$(0) = SPACE$(4096): DiskBoot$(0) = SPACE$(512)
BosAlan$(0) = SPACE$(2048)
FOR i = 1 TO 91: READ Yardim$(i): NEXT
Yazar$ = CHR$(83) + CHR$(129) + CHR$(108) + CHR$(101) + CHR$(121) + CHR$(109) + CHR$(97) + CHR$(110) + CHR$(32) + CHR$(71) + CHR$(154) + CHR$(78) + CHR$(69) + CHR$(76)
Program$ = CHR$(76) + CHR$(79) + CHR$(67) + CHR$(75) + CHR$(85) + CHR$(80)
Yil$ = CHR$(49) + CHR$(57) + CHR$(57) + CHR$(55)
AnaSifre$ = Yazar$ + "-" + Program$
F1$ = CHR$(0) + CHR$(59)

CALL EkranKartiTest
CALL TurkceFont
CALL DisketBoot
CALL Hiz
CALL Intro
CALL AnaMenu

END SUB

SUB ProgramSonu
 COLOR 7, 0
 CLS
 CALL OrijinalFont
END SUB

SUB RaporSil (satir, sutun, zemin)
 FOR i = satir + 14 TO satir + 18: COLOR 15, zemin: LOCATE i, sutun: PRINT "∫"; : COLOR 3, zemin: PRINT STRING$(73, " "); : COLOR 15, zemin: PRINT "∫"; : COLOR 0, 0: PRINT "€€"
 NEXT
END SUB

SUB TurkceFont
 FOR i = 1 TO 4096
 READ byte
 byte$ = CHR$(byte)
 MID$(CharSet$(0), i, 1) = byte$
 NEXT
 CALL LoadFont(CharSet$(), 256, 0)
END SUB



































